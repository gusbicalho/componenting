{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Componenting where

import Data.Kind
import Data.Row
import qualified Data.Row.Records as Rec
import qualified Data.Row.Variants as Var
import Data.Row.Internal (Subset)
import Data.Generics.Labels ()
import Control.Lens
import qualified Control.Concurrent.Async as Async
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits
import Componenting.Component
import Componenting.System

a :: ((r .! "x") ~ Int) => Rec r -> Int
a p = p .! #x :: Int

-- b :: Rec ("x" .== Int)
-- b :: Rec ('Data.Row.Internal.R '[ "x" 'Data.Row.Internal.:-> Int])
b = #x .== (42 :: Int)

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
    deps
  where
    start _ (ConfigImplDef s) = do
      putStrLn "Starting ConfigImpl"
      pure $ ConfigImpl s (read s)

instance StopComponent
    ConfigImplDef
    ConfigImpl
  where
    stop (ConfigImpl s _) = do
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
  where
    stop (PrintLoop s task) = do
      putStrLn "Stopping PrintLoop"
      Async.cancel task
      pure $ PrintLoopDef s

-- System

singleLabeledComponent :: IO ("config" :-> ConfigImpl)
singleLabeledComponent = start empty $ #config :-> ConfigImplDef "5000000"

systemWithSingleComponent :: IO (RunningSystem ("config" .== ConfigImpl)
                                               ("config" :-> ConfigImpl))
systemWithSingleComponent = start empty $ System $ #config :-> ConfigImplDef "5000000"

helloSystem :: System (("config" :-> ConfigImplDef) :>> ("printLoop" :-> PrintLoopDef))
helloSystem = System $ #config :-> ConfigImplDef "5000000"
                   ~>> #printLoop :-> PrintLoopDef "Hello!"

byeSystem :: System (("config" :-> ConfigImplDef) :>> ("printLoop" :-> PrintLoopDef))
byeSystem = helloSystem
            & replace (#printLoop :-> PrintLoopDef "Bye!")

startAndStopSystem :: IO Bool
startAndStopSystem = do
  running <- start empty helloSystem
  stopped <- stop running
  pure $ stopped == helloSystem
  -- True
