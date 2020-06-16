{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Componenting.System
  ( System (..), (:>>), (~>>), (:->) (..), replace
  , RunningSystem, (:<<), WithMeta, RenamingDep, renamingDep
  , startSystem, stopSystem, getComponents
  ) where

import Componenting.Component
import Data.Row
import Data.Row.Records
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
-- stopWithMeta things in order - first a component, then their dependencies.
data startedComponent :<< runningDeps = startedComponent :<< runningDeps
  deriving (Eq)
infixl 6 :<<

-- | Each item in the :<< stack is annotated with meta
data WithMeta meta component = WithMeta meta component
  deriving (Eq)

newtype RenamingDep (fromLabel :: Symbol) (toLabel :: Symbol) component
  = RenamingDep component
  deriving (Eq)

-- Public helpers
startSystem :: StartComponent (System components) startedSystem meta (Rec Empty)
            => System components -> IO startedSystem
startSystem s@(System _) = start empty s

stopSystem :: StopComponent stoppedComponents stopStack ()
           => RunningSystem startedComponents stopStack -> IO (System stoppedComponents)
stopSystem s@(RunningSystem _ _) = stop s

getComponents :: RunningSystem startedComponents stopStack -> Rec startedComponents
getComponents (RunningSystem comps _) = comps

renamingDep :: forall (fromLabel :: Symbol) (toLabel :: Symbol) component.
               component -> RenamingDep fromLabel toLabel component
renamingDep = RenamingDep

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

-- Wrap a component to rename the dependencies they get

instance
  ( KnownSymbol fromLabel, KnownSymbol toLabel
  , renamedDeps ~ Rename fromLabel toLabel deps
  , StartComponent stoppedComp startedComp meta (Rec renamedDeps)) =>
  StartComponent
    (RenamingDep fromLabel toLabel stoppedComp)
    startedComp
    (RenamingDep toLabel fromLabel meta)
    (Rec deps)
  where
    startWithMeta deps (RenamingDep stoppedComp) = do
      (meta, startedComp) <- startWithMeta renamedDeps stoppedComp
      pure $ (RenamingDep @toLabel @fromLabel meta, startedComp)
      where
        renamedDeps = rename (Label @fromLabel) (Label @toLabel) deps

instance
  ( StopComponent stoppedComp startedComp meta ) =>
  StopComponent
    (RenamingDep fromLabel toLabel stoppedComp)
    startedComp
    (RenamingDep toLabel fromLabel meta)
  where
    stopWithMeta (RenamingDep meta) startedComp = do
      stoppedComp <- stopWithMeta meta startedComp
      pure $ RenamingDep @fromLabel @toLabel stoppedComp

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

-- System instances

-- StartWithMeta the system - Base case - system with a single labeled component
instance
  ( StartComponent stoppedComponent startedComponent meta componentDeps
  , runningSystemRow ~ (label .== startedComponent)
  , KnownSymbol label
  ) =>
  StartComponent
    (System (label :-> stoppedComponent))
    (RunningSystem runningSystemRow (WithMeta (label :-> meta) (label :-> startedComponent)))
    ()
    componentDeps
  where
    startWithMeta deps (System (label :-> stoppedComponent)) = do
      (meta, startedComponent) <- startWithMeta deps stoppedComponent
      pure $ ((), RunningSystem (label .== startedComponent)
                                (WithMeta (label :-> meta) (label :-> startedComponent)))

-- StartWithMeta the system - Recursive case - system with parts connected by :>>
instance
  ( StartComponent (System stoppedDepsSystem)
                   (RunningSystem runningDeps stopStack)
                   ()
                   systemDeps
  , StartComponent stoppedComponent
                   (label :-> startedComponent)
                   meta
                   (Rec runningDeps)
  , KnownSymbol label
  , startedSystem ~ (RunningSystem (runningDeps .+ label .== startedComponent)
                                   (WithMeta meta (label :-> startedComponent) :<< stopStack))
  ) =>
  StartComponent
    (System (stoppedDepsSystem :>> stoppedComponent))
    startedSystem
    ()
    systemDeps
  where
    startWithMeta outerDeps (System (stoppedDeps :>> stoppedComponent)) = do
      ((), RunningSystem deps stopStack) <- startWithMeta outerDeps (System stoppedDeps)
      (meta, label :-> component) <- startWithMeta deps stoppedComponent
      pure $ ((), RunningSystem (deps .+ label .== component)
                                (WithMeta meta (label :-> component) :<< stopStack))

-- StopWithMeta system - base case of the :<< stack
instance
  ( StopComponent stoppedComponent startedComponent meta
  ) =>
  StopComponent
    stoppedComponent
    (WithMeta meta startedComponent)
    ()
  where
    stopWithMeta () (WithMeta meta startedComponent) = do
      stoppedComponent <- stopWithMeta meta startedComponent
      pure $ stoppedComponent

-- StopWithMeta system - traversing the :<< stack
instance
  ( StopComponent stoppedComponent startedComponent meta
  , StopComponent stoppedDeps runningDeps ()
  ) =>
  StopComponent
    (stoppedDeps :>> stoppedComponent)
    ((WithMeta meta startedComponent) :<< runningDeps)
    ()
  where
    stopWithMeta () ((WithMeta meta startedComponent) :<< runningDeps) = do
      stoppedComponent <- stopWithMeta meta startedComponent
      stoppedDeps <- stopWithMeta () runningDeps
      pure $ stoppedDeps :>> stoppedComponent

-- StopWithMeta system -- root instance - delegates to the instance that traverses the stack
instance
  ( StopComponent stoppedComponents stopStack ()
  ) =>
  StopComponent
    (System stoppedComponents)
    (RunningSystem components stopStack)
    ()
  where
    stopWithMeta () (RunningSystem _ stopStack) = System <$> stopWithMeta () stopStack
