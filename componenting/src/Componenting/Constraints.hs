{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Useful helpers for defining constraints over records of dependencies
module Componenting.Constraints where

import Data.Kind
import Data.Row
import GHC.TypeLits

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

class (constraint1 row, constraint2 row) => And constraint1 constraint2 (row :: Row Type) where
instance (constraint1 row, constraint2 row) => And constraint1 constraint2 row where

type family Dependencies (deps :: [(Symbol, Type -> Constraint)]) :: Row Type -> Constraint where
  Dependencies '[] = NoDependencies
  Dependencies ('(label, constraint) ': deps) = And (Dependency label constraint)
                                                    (Dependencies deps)
