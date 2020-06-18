{-# LANGUAGE UndecidableInstances #-}

module Componenting.Internal.System.Graph where

import Componenting.Component
import Data.Row
import GHC.TypeLits

-- System data types

-- Data types for building the graph and stack

-- | Labels a component
data (label :: Symbol) :-> component = Label label :-> component
  deriving (Eq)

-- | Connects stopped dependencies to the next component, which may depend on
-- them. Main building block of the System definition.
data system :>> component = system :>> component
  deriving (Eq)
infixl 6 :>>

-- | Connects running coponent to their running dependencies.
-- Used to build a stack, stored in the running system, which allows us to
-- stopWithMeta things in order - first a component, then their dependencies.
data startedComponent :<< runningDeps = startedComponent :<< runningDeps
  deriving (Eq)
infixl 6 :<<

-- | Build the dependency list normalizing so they associate to the left
class ConcatDeps front back where
  (~>>) :: front -> back -> Concated front back

infixl 6 ~>>

type family Concated front back where
  Concated front (middle :>> back) = Concated front middle :>> back
  Concated front back = front :>> back

instance
  {-# OVERLAPPABLE #-}
  (Concated a b ~ (a :>> b)) =>
  ConcatDeps a b where
  a ~>> b = a :>> b

instance
  {-# OVERLAPPING #-}
  ( Concated a (b :>> c) ~ (Concated a b :>> c)
  , ConcatDeps a b
  ) =>
  ConcatDeps a (b :>> c) where
  a ~>> (b :>> c) = (a ~>> b) :>> c

-- Start labeled components
instance
  ( StartComponent stoppedComponent startedComponent meta runningDeps
  ) =>
  StartComponent
    (label :-> stoppedComponent)
    (label :-> startedComponent)
    (label :-> meta)
    runningDeps
  where
    startWithMeta runningDeps (label :-> stoppedComponent) = do
      (meta, startedComponent) <- startWithMeta runningDeps stoppedComponent
      pure $ (label :-> meta, label :-> startedComponent)

instance
  ( StopComponent stoppedComponent startedComponent meta
  ) =>
  StopComponent
    (label :-> stoppedComponent)
    (label :-> startedComponent)
    (label :-> meta)
  where
    stopWithMeta (_ :-> meta) (label :-> startedComponent) = do
      stoppedComponent <- stopWithMeta meta startedComponent
      pure $ label :-> stoppedComponent
