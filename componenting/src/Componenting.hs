{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Componenting where

import Data.Function ((&))
import Data.Row
import Data.Generics.Labels ()
import qualified Control.Concurrent.Async as Async
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Componenting.Component
import Componenting.System

-- Config

class Config t where
  configInterval :: t -> Int

data ConfigImplDef = ConfigImplDef String
  deriving (Eq)
data ConfigImpl = ConfigImpl String Int
  deriving (Eq)

instance StartComponent
    ConfigImplDef
    ConfigImpl
    ()
    deps
  where
    start _ (ConfigImplDef s) = do
      putStrLn "Starting ConfigImpl"
      pure $ ConfigImpl s (read s)

instance StopComponent
    ConfigImplDef
    ConfigImpl
    ()
  where
    stopWithMeta _ (ConfigImpl s _) = do
      putStrLn "Stopping ConfigImpl"
      pure $ ConfigImplDef s

instance Config ConfigImpl where
  configInterval (ConfigImpl _ n) = n

-- PrintLoop

data PrintLoopDef = PrintLoopDef String
  deriving (Eq)
data PrintLoop = PrintLoop String (Async.Async ())
  deriving (Eq)

instance
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
systemWithSingleComponent = System $ #config :-> ConfigImplDef "5000000"

startedSystemWithSingleComponent :: IO (RunningSystem ("config" .== ConfigImpl)
                                       (WithMeta ("config" :-> ()) ("config" :-> ConfigImpl)))
startedSystemWithSingleComponent = startSystem $ System $ #config :-> ConfigImplDef "5000000"

helloSystem :: System ("config" :-> ConfigImplDef
                   :>> "printLoop" :-> PrintLoopDef)
helloSystem = System $ #config :-> ConfigImplDef "5000000"
                   ~>> #printLoop :-> PrintLoopDef "Hello!"

startedHelloSystem :: IO (RunningSystem ("config" .== ConfigImpl
                                      .+ "printLoop" .== PrintLoop)
                                      (WithMeta ("printLoop" :-> ()) ("printLoop" :-> PrintLoop)
                                       :<< WithMeta ("config" :-> ()) ("config" :-> ConfigImpl)))
startedHelloSystem = startSystem helloSystem

byeSystem :: System ("config" :-> ConfigImplDef
                 :>> "printLoop" :-> PrintLoopDef)
byeSystem = helloSystem
            & replace (#printLoop :-> PrintLoopDef "Bye!")

startAndStopSystem :: IO Bool
startAndStopSystem = do
  running <- startSystem helloSystem
  let components = getComponents running
  let (ConfigImpl _ n) = components .! #config
  putStrLn $ "We can get data from the config component: " <> show n
  stopped <- stopSystem running
  pure $ stopped == helloSystem
  -- True
