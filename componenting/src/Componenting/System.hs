{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Componenting.System
  ( System (..), (:>>), (~>>), (:->) (..), replace
  , RunningSystem, (:<<)
  , components
  ) where

import Componenting.Component
import Data.Row
import GHC.TypeLits

-- System data types
newtype System components = System components
  deriving (Eq)

data RunningSystem startedComponents stopStack =
  RunningSystem (Rec startedComponents) stopStack

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
-- stop things in order - first a component, then their dependencies.
data startedComponent :<< runningDeps = startedComponent :<< runningDeps
  deriving (Eq)
infixl 6 :<<

-- Public fns
components :: RunningSystem startedComponents stopStack -> Rec startedComponents
components (RunningSystem comps _) = comps

-- Build the dependency list normalizing so they associate to the left
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

-- Replace items on the system

type family ReplaceResult (label :: Symbol) newComponent existingSystem where
  ReplaceResult label newComponent (System components)
    = System (ReplaceResult label newComponent components)
  ReplaceResult label newComponent (label :-> existingComponent)
    = label :-> newComponent
  ReplaceResult label newComponent (deps :>> (label :-> existingComponent))
    = deps :>> (label :-> newComponent)
  ReplaceResult label newComponent (deps :>> existingComponent)
    = (ReplaceResult label newComponent deps) :>> existingComponent
  ReplaceResult label newComponent components
    = TypeError ('Text "Could not find label " ':<>: 'ShowType label ':<>:
                 'Text " to replace in " ':<>: 'ShowType components)

class ReplaceSystemComponent
    (label :: Symbol)
    newComponent
    existingSystem
  where
    replace :: (label :-> newComponent)
            -> existingSystem
            -> ReplaceResult label newComponent existingSystem

instance
  ReplaceSystemComponent
    label
    newComponent
    (System (label :-> existingComponent))
  where
    replace newComponent (System _) = System newComponent

instance
  {-# OVERLAPPABLE #-}
  ( ReplaceSystemComponent label newComponent (System deps)
  , replacedDeps ~ ReplaceResult label newComponent deps
  , ReplaceResult label newComponent (System (deps :>> comp))
    ~ System (replacedDeps :>> comp)
  ) =>
  ReplaceSystemComponent
    label
    newComponent
    (System (deps :>> comp))
  where
    replace newComponent (System (deps :>> comp)) =
      let System replaced = replace newComponent (System deps)
      in System (replaced :>> comp)

instance
  {-# OVERLAPPING #-}
  ReplaceSystemComponent
    label
    newComponent
    (System (deps :>> (label :-> comp)))
  where
    replace newComponent (System (deps :>> _)) = System (deps :>> newComponent)

-- Labeled component instances
instance
  ( StartComponent stoppedComponent startedComponent runningDeps
  ) =>
  StartComponent
    (label :-> stoppedComponent)
    (label :-> startedComponent)
    runningDeps
  where
    start runningDeps (label :-> stoppedComponent) = do
      startedComponent <- start runningDeps stoppedComponent
      pure $ label :-> startedComponent

instance
  ( StopComponent stoppedComponent startedComponent
  ) =>
  StopComponent
    (label :-> stoppedComponent)
    (label :-> startedComponent)
  where
    stop (label :-> startedComponent) = do
      stoppedComponent <- stop startedComponent
      pure $ label :-> stoppedComponent

-- System instances

-- Start the system - Base case - system with a single labeled component
instance
  ( StartComponent stoppedComponent startedComponent componentDeps
  , runningSystemRow ~ (label .== startedComponent)
  , KnownSymbol label
  ) =>
  StartComponent
    (System (label :-> stoppedComponent))
    (RunningSystem runningSystemRow (label :-> startedComponent))
    componentDeps
  where
    start deps (System (label :-> stoppedComponent)) = do
      startedComponent <- start deps stoppedComponent
      pure $ RunningSystem (label .== startedComponent)
                           (label :-> startedComponent)

-- Start the system - Recursive case - system with parts connected by :>>
instance
  ( StartComponent (System stoppedDepsSystem)
                   (RunningSystem runningDeps stopStack)
                   systemDeps
  , StartComponent stoppedComponent
                   (label :-> startedComponent)
                   (Rec runningDeps)
  , KnownSymbol label
  , startedSystem ~ (RunningSystem (runningDeps .+ label .== startedComponent)
                                   ((label :-> startedComponent) :<< stopStack))
  ) =>
  StartComponent
    (System (stoppedDepsSystem :>> stoppedComponent))
    startedSystem
    systemDeps
  where
    start outerDeps (System (stoppedDeps :>> stoppedComponent)) = do
      RunningSystem deps stopStack <- start outerDeps (System stoppedDeps)
      (label :-> component) <- start deps stoppedComponent
      pure $ RunningSystem (deps .+ label .== component) ((label :-> component) :<< stopStack)

-- Stop system - traversing the :<< stack
instance
  ( StopComponent stoppedComponent startedComponent
  , StopComponent stoppedDeps runningDeps
  ) =>
  StopComponent
    (stoppedDeps :>> stoppedComponent)
    (startedComponent :<< runningDeps)
  where
    stop (startedComponent :<< runningDeps) = do
      stoppedComponent <- stop startedComponent
      stoppedDeps <- stop runningDeps
      pure $ stoppedDeps :>> stoppedComponent

-- Stop system -- root instance - delegates to the instance that traverses the stack
instance
  ( StopComponent stoppedComponents stopStack
  ) =>
  StopComponent
    (System stoppedComponents)
    (RunningSystem components stopStack)
  where
    stop (RunningSystem _ stopStack) = System <$> stop stopStack
