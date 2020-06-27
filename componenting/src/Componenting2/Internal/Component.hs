{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Componenting2.Internal.Component where

import Data.Row (Row, Rec, type (.!))
import Data.Kind (Constraint, Type)
import GHC.TypeLits (Symbol)
import Componenting2.Internal.Util (And)

class StartComponent
  (stoppedC :: Type)
  where
  type Started stoppedC :: Type
  type ComponentMeta stoppedC :: Type
  type DependenciesSpec stoppedC :: DependenciesSpecKind

  startWithMeta :: Dependencies (DependenciesSpec stoppedC) row
                => Rec row -> stoppedC -> IO (ComponentMeta stoppedC, Started stoppedC)
  default startWithMeta :: ( Dependencies (DependenciesSpec stoppedC) row
                           , ComponentMeta stoppedC ~ () )
                        => Rec row -> stoppedC -> IO (ComponentMeta stoppedC, Started stoppedC)
  startWithMeta deps stoppedC = ((), ) <$> start deps stoppedC

  start :: Dependencies (DependenciesSpec stoppedC) row
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

-- Dependencies

type DependenciesSpecKind = [(Symbol, Type -> Constraint)]

type family Dependencies (deps :: DependenciesSpecKind) :: Row Type -> Constraint where
  Dependencies '[] = NoDependencies
  Dependencies ('(label, constraint) ': deps) = And (Dependency label constraint)
                                                    (Dependencies deps)

class NoDependencies (t :: Row Type) where
instance NoDependencies t where

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
  , Dependencies (DependenciesSpec comp) depsRow ) =>
  StartableWithDependencies depsRow comp
instance
  ( StartComponent comp
  , Dependencies (DependenciesSpec comp) depsRow ) =>
  StartableWithDependencies depsRow comp
