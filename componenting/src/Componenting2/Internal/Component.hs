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

import Data.Row (Rec, Label (..), type (.!), Empty)
import Data.Row.Records (rename, Rename)
import Data.Row.Internal (Row (..), LT (..))
import Data.Kind (Constraint, Type)
import GHC.TypeLits (Symbol, KnownSymbol)
import Componenting2.Internal.Util (All)

class StartComponent
  (stoppedC :: Type)
  where
  type Started stoppedC :: Type

  type ComponentMeta stoppedC :: Type
  type ComponentMeta stoppedC = ()

  type DependenciesSpec stoppedC :: DependenciesSpecKind
  type DependenciesSpec stoppedC = Empty

  type DependenciesConstraint stoppedC (row :: Row Type) :: Constraint
  type DependenciesConstraint stoppedC row = Dependencies (DependenciesSpec stoppedC) row

  startWithMeta :: DependenciesConstraint stoppedC row
                => Rec row -> stoppedC -> IO (ComponentMeta stoppedC, Started stoppedC)
  default startWithMeta :: ( DependenciesConstraint stoppedC row
                           , ComponentMeta stoppedC ~ () )
                        => Rec row -> stoppedC -> IO (ComponentMeta stoppedC, Started stoppedC)
  startWithMeta deps stoppedC = ((), ) <$> start deps stoppedC

  start :: DependenciesConstraint stoppedC row
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

-- Dependencies

type DependenciesSpecKind = Row (Type -> Constraint)

type family DependenciesT (deps :: DependenciesSpecKind) :: [Row Type -> Constraint] where
  DependenciesT ('R '[]) = '[]
  DependenciesT ('R ((label ':-> constraint) ': deps)) =
    Dependency label constraint ': DependenciesT ('R deps)

class
  (All (DependenciesT deps) row) =>
  Dependencies (deps :: DependenciesSpecKind) (row :: Row Type)
  where
instance
  {-# OVERLAPPABLE #-}
  (All (DependenciesT deps) row) =>
  Dependencies (deps :: DependenciesSpecKind) (row :: Row Type)
  where

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
  ( StartComponent comp
  , DependenciesConstraint comp depsRow ) =>
  StartableWithDependencies depsRow comp
instance
  ( StartComponent comp
  , DependenciesConstraint comp depsRow ) =>
  StartableWithDependencies depsRow comp
