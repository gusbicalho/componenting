{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Componenting.Examples.ComponentTypeclasses where

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

instance StartComponent
    ConfigImplDef -- We can call `start` on a ConfigImplDef value
    ConfigImpl    -- To get a ConfigImpl value
    ()            -- And an empty metadata. This metadata is stored in the system
                  -- after we start the component, but is not accessible by client
                  -- code. It is used to `stop` the component when the system is
                  -- stopped.
                  -- Since the metadata here is simply (), we can implement only
                  -- the `start` method of the typeclass, instead of the more
                  -- verbose `startWithMeta`. `startWithMeta` has a default
                  -- implementation when the metadata is ().
    deps          -- We don't require any dependencies to do that, so the `deps`
                  -- parameter can be anything
  where
    start _ (ConfigImplDef s) = do
      putStrLn "Starting ConfigImpl"
      pure $ ConfigImpl s (read s)

instance StopComponent
    ConfigImplDef -- If we `stop` a ConfigImpl value, we get a ConfgiImplDef back
    ConfigImpl
    ()            -- We don't expect any metadata to stop a ConfigImpl
  where
    stopWithMeta _ (ConfigImpl s _) = do
      putStrLn "Stopping ConfigImpl"
      pure $ ConfigImplDef s

-- ConfigImpl values (which correspond to the started component) implement
-- the Config interface
instance Config ConfigImpl where
  configInterval (ConfigImpl _ n) = n

-- PrintLoop

-- Stopped component
data PrintLoopDef = PrintLoopDef String
  deriving (Eq)
-- Started component
data PrintLoop = PrintLoop String (Async.Async ())
  deriving (Eq)

instance
  -- To start a PrintLoopDef value and get a PrintLoop value, we need some dependencies
  -- The `dependencies` parameter can be any Rec (from Data.Row.Record)
  -- which includes a "config" field, as long as the type of
  -- that "config" field implements the Config typeclass.
  ((deps .! "config") ~ config, Config config) =>
  StartComponent
    PrintLoopDef
    PrintLoop
    ()
    (Rec deps)
  where
    start deps (PrintLoopDef s) = do
      putStrLn "Starting PrintLoop"
      let config = deps .! #config
      let interval = configInterval config
      task <- Async.async $ forever $ do
        putStrLn s
        threadDelay interval
      pure $ PrintLoop s task

instance StopComponent
    PrintLoopDef
    PrintLoop
    ()
  where
    stopWithMeta _ (PrintLoop s task) = do
      putStrLn "Stopping PrintLoop"
      Async.cancel task
      pure $ PrintLoopDef s

-- System

singleLabeledComponent :: IO ("config" :-> ConfigImpl)
singleLabeledComponent = start empty $ #config :-> ConfigImplDef "5000000"

systemWithSingleComponent :: System ("config" :-> ConfigImplDef)
systemWithSingleComponent = system $ #config :-> ConfigImplDef "5000000"

-- The full signature for a running system system is complicated, because it includes
-- a stack of initialized components in the order they must be stopped if the system
-- is stopped, along with metadata which must be provided when we stop the component.
-- However, it can be fully inferred, and PartialTypeSignatures can be used to
-- avoid typing them in top-level definitions (see `byeSystem` below)
startedSystemWithSingleComponent :: IO (RunningSystem ("config" .== ConfigImpl)
                                       (WithMeta ("config" :-> ()) ("config" :-> ConfigImpl)))
startedSystemWithSingleComponent = startSystem $ system $ #config :-> ConfigImplDef "5000000"

helloSystem :: System ("myConfig" :-> ConfigImplDef
                   :>> "printLoop" :-> (RenamingDep
                                         "myConfig"
                                         "config"
                                         PrintLoopDef))
helloSystem = system $ #myConfig :-> ConfigImplDef "5000000"
                   ~>> #printLoop :-> (PrintLoopDef "Hello!"
                                       & renamingDep @"myConfig" @"config")

startedHelloSystem :: IO (RunningSystem ("myConfig" .== ConfigImpl
                                      .+ "printLoop" .== PrintLoop)
                                        (WithMeta
                                          ("printLoop" :-> RenamingDep "config" "myConfig" ())
                                          ("printLoop" :-> PrintLoop)
                                        :<< WithMeta
                                              ("myConfig" :-> ())
                                              ("myConfig" :-> ConfigImpl)))
startedHelloSystem = startSystem helloSystem

byeSystem :: _
byeSystem = helloSystem
            & replace (#printLoop :-> PrintLoopDef "Bye!")

startedByeSystem :: _
startedByeSystem = helloSystem
                   & replace (#printLoop :-> (PrintLoopDef "Bye!"
                                              & renamingDep @"myConfig" @"config"))
                   & startSystem

startAndStopSystem :: IO Bool
startAndStopSystem = do
  running <- startSystem helloSystem
  let components = getComponents running
  let (ConfigImpl _ n) = components .! #myConfig
  putStrLn $ "We can get data from the config component: " <> show n
  stopped <- stopSystem running
  pure $ stopped == helloSystem
  -- True
