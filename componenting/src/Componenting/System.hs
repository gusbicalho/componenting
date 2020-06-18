{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Componenting.System
  ( System, system, unSystem, (:>>), (~>>), (:->) (..), replace
  , RunningSystem, (:<<), WithMeta, RenamingDep, renamingDep
  , startSystem, stopSystem, getComponents
  ) where

import Componenting.Component
import Data.Row
import GHC.TypeLits
import Componenting.Internal.System.Graph
import Componenting.Internal.System.Validation
import Componenting.Internal.System.System

-- | Smart constructor with helpful type-level checks for better error messages
system :: ( AllComponentsLabeled components
          , NoDuplicateLabels components )
       => components -> System components
system = System

unSystem :: System a -> a
unSystem (System components) = components

getComponents :: RunningSystem startedComponents stopStack -> Rec startedComponents
getComponents (RunningSystem comps _) = comps

startSystem :: StartComponent (System components) startedSystem meta (Rec Empty)
            => System components -> IO startedSystem
startSystem = start empty

stopSystem :: StopComponent stoppedComponents stopStack ()
           => RunningSystem startedComponents stopStack -> IO (System stoppedComponents)
stopSystem s@(RunningSystem _ _) = stop s

renamingDep :: forall (fromLabel :: Symbol) (toLabel :: Symbol) component.
               component -> RenamingDep fromLabel toLabel component
renamingDep = RenamingDep
