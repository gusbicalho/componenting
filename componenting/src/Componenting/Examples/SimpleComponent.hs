{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Same use case as ComponentTypeclasses, implemented using SimpleComponent
module Componenting.Examples.SimpleComponent where

import Data.Function ((&))
import Data.Row
import Data.Generics.Labels ()
import qualified Control.Concurrent.Async as Async
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Componenting

-- Config

class Config t where
  configInterval :: t -> Int

-- Stopped component
data ConfigImplDef = ConfigImplDef String
  deriving (Eq)
-- Started component
data ConfigImpl = ConfigImpl String Int
  deriving (Eq)

-- These aren't really necessary, but they help to avoid writing all this stuff
type ConfigComponentDef = SimpleComponentStopped ConfigImplDef ConfigImpl NoDependencies
type ConfigComponent = SimpleComponent ConfigImplDef ConfigImpl NoDependencies

newConfigComponent :: String -> ConfigComponentDef
newConfigComponent configString = simpleComponent startConfig stopConfig stoppedConfig
  where
    stoppedConfig = ConfigImplDef configString
    startConfig _ (ConfigImplDef s) = do
      putStrLn "Starting ConfigImpl"
      pure $ ConfigImpl s (read s)
    stopConfig (ConfigImpl s _) = do
      putStrLn "Stopping ConfigImpl"
      pure $ ConfigImplDef s

-- A SimpleComponent wrapping a ConfigImpl value implements the Config interface
instance Config ConfigComponent where
  configInterval (getComponent -> ConfigImpl _ n) = n
  -- the view pattern here is equivalent to using
  -- configInterval comp =
  --   let (ConfigImpl _ n) = getComponent comp
  --   in n

-- PrintLoop

data PrintLoopDef = PrintLoopDef String
  deriving (Eq)
data PrintLoop = PrintLoop String (Async.Async ())

type PrintLoopComponentDeps = Dependency "config" Config
                              -- If we had several dependencies, we could use the helper below
                              -- Dependencies
                              --   '[ '("config", Config)
                              --    , '("otherDep", OtherConstraint) ]
type PrintLoopComponentDef = SimpleComponentStopped PrintLoopDef PrintLoop PrintLoopComponentDeps
type PrintLoopComponent = SimpleComponent PrintLoopDef PrintLoop PrintLoopComponentDeps

newPrintLoopComponent :: String -> PrintLoopComponentDef
newPrintLoopComponent printString = simpleComponent startPrintLoop stopPrintLoop stoppedComp
  where
    stoppedComp = PrintLoopDef printString
    startPrintLoop deps (PrintLoopDef s) = do
      putStrLn "Starting PrintLoop"
      let config = deps .! #config
      let interval = configInterval config
      task <- Async.async $ forever $ do
        putStrLn s
        threadDelay interval
      pure $ PrintLoop s task
    stopPrintLoop :: PrintLoop -> IO PrintLoopDef
    stopPrintLoop (PrintLoop s task) = do
      putStrLn "Stopping PrintLoop"
      Async.cancel task
      pure $ PrintLoopDef s

-- System

singleLabeledComponentStarted :: IO ("config" :-> ConfigComponent)
singleLabeledComponentStarted = start empty $ #config :-> newConfigComponent "5000000"

systemWithSingleComponent :: System ("config" :-> ConfigComponentDef)
systemWithSingleComponent = system $ #config :-> newConfigComponent "5000000"

-- The full signature for a running system system is complicated, because it includes
-- a stack of initialized components in the order they must be stopped if the system
-- is stopped, along with metadata which must be provided when we stop the component.
-- However, it can be fully inferred, and PartialTypeSignatures can be used to
-- avoid typing them in top-level definitions (see `byeSystem` below)
startedSystemWithSingleComponent :: IO (RunningSystem ("config" .== ConfigComponent)
                                       (WithMeta ("config" :-> ())
                                                 ("config" :-> ConfigComponent)))
startedSystemWithSingleComponent = startSystem $ system $ #config :-> newConfigComponent "5000000"

helloSystem :: System ("myConfig" :-> ConfigComponentDef
                   :>> "printLoop" :-> (RenamingDep "myConfig" "config"
                                          PrintLoopComponentDef))
helloSystem = system $ #myConfig :-> newConfigComponent "5000000"
                   ~>> #printLoop :-> (newPrintLoopComponent "Hello!"
                                       & renamingDep @"myConfig" @"config")

startedHelloSystem :: IO (RunningSystem ("myConfig" .== ConfigComponent
                                      .+ "printLoop" .== PrintLoopComponent)
                                        (WithMeta
                                          ("printLoop" :-> RenamingDep "config" "myConfig" ())
                                          ("printLoop" :-> PrintLoopComponent)
                                        :<< WithMeta
                                              ("myConfig" :-> ())
                                              ("myConfig" :-> ConfigComponent)))
startedHelloSystem = startSystem helloSystem

byeSystem :: _
byeSystem = helloSystem
            & replace (#printLoop :-> (newPrintLoopComponent "Hello!"
                                       & renamingDep @"myConfig" @"config"))

startAndStopSystem :: IO ()
startAndStopSystem = do
  running <- startSystem helloSystem
  let components = getComponents running
  let (ConfigImpl _ n) = getComponent $ components .! #myConfig
  putStrLn $ "We can get data from the config component: " <> show n
  stopSystem running
  pure ()
