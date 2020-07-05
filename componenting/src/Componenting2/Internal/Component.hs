{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Componenting2.Internal.Component
  ( StartComponent (..)
  , StopComponent (..), stop
  , StartableWithDependencies
  , RenameDependency (..)
  ) where

import Data.Row (Rec, Label (..), type (.!), (.!), type (.==), (.==), type (.-), (.-), type (.+), (.+), type (.//), (.//), Empty)
import Data.Row.Records (rename, Rename)
import Data.Row.Internal (Row (..), LT (..))
import Data.Kind (Constraint, Type)
import GHC.TypeLits (Symbol, KnownSymbol)
import Componenting2.Internal.Util (If, ItemKnown, All, MaybeConstrained, MaybeSome (..))

class StartComponent
  (stoppedC :: Type)
  where
  type Started stoppedC :: Type

  type ComponentMeta stoppedC :: Type
  type ComponentMeta stoppedC = ()

  type DependenciesSpec stoppedC :: DependenciesSpecKind
  type DependenciesSpec stoppedC = Empty

  type DependenciesConstraint stoppedC (row :: Row Type) :: Constraint
  type DependenciesConstraint stoppedC row =
    All (DependenciesT Dependency (DependenciesSpec stoppedC)) row

  type OptionalDependenciesSpec stoppedC :: DependenciesSpecKind
  type OptionalDependenciesSpec stoppedC = Empty

  type OptionalDependenciesConstraint stoppedC (row :: Row Type) :: Constraint
  type OptionalDependenciesConstraint stoppedC row =
    All (DependenciesT OptionalDependency (OptionalDependenciesSpec stoppedC)) row

  startWithMeta :: ( DependenciesConstraint stoppedC row
                   , OptionalDependenciesConstraint stoppedC row )
                => Rec row -> stoppedC -> IO (ComponentMeta stoppedC, Started stoppedC)
  default startWithMeta :: ( DependenciesConstraint stoppedC row
                           , OptionalDependenciesConstraint stoppedC row
                           , ComponentMeta stoppedC ~ () )
                        => Rec row -> stoppedC -> IO (ComponentMeta stoppedC, Started stoppedC)
  startWithMeta deps stoppedC = ((), ) <$> start deps stoppedC

  start :: ( DependenciesConstraint stoppedC row
           , OptionalDependenciesConstraint stoppedC row )
        => Rec row -> stoppedC -> IO (Started stoppedC)
  start deps stoppedC = snd <$> startWithMeta deps stoppedC

class
  (StartComponent (Stopped startedC meta)
  , meta ~ ComponentMeta (Stopped startedC meta) ) =>
  StopComponent
    (startedC :: Type)
    (meta :: Type)
  where
  type Stopped startedC meta :: Type
  stopWithMeta :: meta -> startedC -> IO (Stopped startedC meta)

stop :: StopComponent startedC ()
     => startedC -> IO (Stopped startedC ())
stop comp = stopWithMeta () comp

-- Rename Dependencies
newtype RenameDependency (outerLabel :: Symbol) (innerLabel :: Symbol) component =
  RenameDependency component

instance
  ( StartComponent component
  , KnownSymbol outerLabel
  , KnownSymbol innerLabel
  ) =>
  StartComponent (RenameDependency outerLabel innerLabel component) where
  type Started (RenameDependency outerLabel innerLabel component)
    = Started component
  type ComponentMeta (RenameDependency outerLabel innerLabel component)
    = (RenameDependency outerLabel innerLabel (ComponentMeta component))
  type DependenciesSpec (RenameDependency outerLabel innerLabel component)
    = Rename innerLabel outerLabel (DependenciesSpec component)
  type DependenciesConstraint (RenameDependency outerLabel innerLabel component) row =
    DependenciesConstraint component (Rename outerLabel innerLabel row)
  type OptionalDependenciesConstraint (RenameDependency outerLabel innerLabel component) row =
    OptionalDependenciesConstraint component (Rename outerLabel innerLabel row)

  startWithMeta deps (RenameDependency component) = do
    let renameDeps = rename (Label @outerLabel) (Label @innerLabel) deps
    (meta, started) <- startWithMeta renameDeps component
    pure (RenameDependency @outerLabel @innerLabel meta, started)

instance
  ( StopComponent runningComponent meta
  , KnownSymbol outerLabel
  , KnownSymbol innerLabel
  ) =>
  StopComponent runningComponent (RenameDependency outerLabel innerLabel meta)
  where
  type Stopped runningComponent (RenameDependency outerLabel innerLabel meta)
    = (RenameDependency outerLabel innerLabel (Stopped runningComponent meta))
  stopWithMeta (RenameDependency meta) runningComponent = do
    stopped <- stopWithMeta meta runningComponent
    pure $ RenameDependency @outerLabel @innerLabel stopped

-- Fill Optional Dependencies
class FillOptionalDependencies
  (optionalDeps :: DependenciesSpecKind)
  (deps :: Row Type)
  where
  type OptionalDependenciesFilled optionalDeps deps :: Row Type
  fillOptionalDependencies :: Rec deps -> Rec (OptionalDependenciesFilled optionalDeps deps)

instance
  FillOptionalDependencies
    Empty
    (deps :: Row Type)
  where
  type OptionalDependenciesFilled Empty deps = deps
  fillOptionalDependencies deps = deps

instance
  ( KnownSymbol optionalDepName
  , FillOptionalDependencies
    ('R moreOptionalDeps)
    Empty) =>
  FillOptionalDependencies
    ('R ((optionalDepName ':-> optionalDepConstraint) ': moreOptionalDeps))
    Empty
  where
  type OptionalDependenciesFilled ('R ((optionalDepName ':-> optionalDepConstraint) ': moreOptionalDeps)) Empty =
    optionalDepName .== MaybeSome optionalDepConstraint
    .+ OptionalDependenciesFilled
        ('R moreOptionalDeps)
        Empty
  fillOptionalDependencies deps =
    Label @optionalDepName .== None @optionalDepConstraint
    .+ fillOptionalDependencies @('R moreOptionalDeps) deps

instance
  ( KnownSymbol optionalDepName
  , FillOptionalDependencies
    ('R moreOptionalDeps)
    ('R moreProvidedDeps)
  , ('R moreProvidedDeps)
    ~ ('R ((optionalDepName ':-> providedOptionalDep) ': moreProvidedDeps)
        .- optionalDepName)
  , optionalDepConstraint providedOptionalDep) =>
  FillOptionalDependencies
    ('R ((optionalDepName ':-> optionalDepConstraint) ': moreOptionalDeps))
    ('R ((optionalDepName ':-> providedOptionalDep) ': moreProvidedDeps))
  where
  type OptionalDependenciesFilled
    ('R ((optionalDepName ':-> optionalDepConstraint) ': moreOptionalDeps))
    ('R ((optionalDepName ':-> providedOptionalDep) ': moreProvidedDeps)) =
    optionalDepName .== MaybeSome optionalDepConstraint
    .+ (OptionalDependenciesFilled
        ('R moreOptionalDeps)
        ('R moreProvidedDeps))
  fillOptionalDependencies deps =
    let label = Label @optionalDepName
    in label .== Some @optionalDepConstraint (deps .! label)
        .+ fillOptionalDependencies @('R moreOptionalDeps) (deps .- label)

-- TODO wtf i dont even know

-- Dependencies

type DependenciesSpecKind = Row (Type -> Constraint)

type family DependenciesT
    (mkDepsConstraint :: Symbol -> specRowK -> depsK -> Constraint)
    (depsSpec :: Row specRowK)
    :: [depsK -> Constraint] where
  DependenciesT mkDepsConstraint ('R '[]) = '[]
  DependenciesT mkDepsConstraint ('R ((label ':-> constraint) ': depsSpec)) =
    mkDepsConstraint label constraint ': DependenciesT mkDepsConstraint ('R depsSpec)

class
  (constraint (row .! label)) =>
  Dependency
    (label :: Symbol)
    (constraint :: Type -> Constraint)
    (row :: Row Type)
  where
instance
  (constraint (row .! label)) =>
  Dependency label constraint row where

class
  ((row .! label) ~ MaybeSome constraint) =>
  OptionalDependency
    (label :: Symbol)
    (constraint :: Type -> Constraint)
    (row :: Row Type)
  where
instance
  ((row .! label) ~ MaybeSome constraint) =>
  OptionalDependency label constraint row where

class
  ( StartComponent comp
  , DependenciesConstraint comp depsRow ) =>
  StartableWithDependencies depsRow comp
instance
  ( StartComponent comp
  , DependenciesConstraint comp depsRow ) =>
  StartableWithDependencies depsRow comp
