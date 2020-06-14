{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Componenting where

import Data.Constraint (top)
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

a :: ((r .! "x") ~ Int) => Rec r -> Int
a p = p .! #x :: Int

-- b :: Rec ("x" .== Int)
-- b :: Rec ('Data.Row.Internal.R '[ "x" 'Data.Row.Internal.:-> Int])
b = #x .== (42 :: Int)

newtype Some (constraint :: Type -> Constraint) =
  Some { getSome :: forall x. (constraint x) => x }

class StartComponent
    stoppedComp
    runningComp
    dependenciesRowConstraint
    | stoppedComp -> runningComp
    , stoppedComp -> dependenciesRowConstraint
  where
  start :: dependenciesRowConstraint dependenciesRow => Rec dependenciesRow -> stoppedComp -> IO runningComp

class StopComponent
    stoppedComp
    runningComp
  where
  stop :: runningComp -> IO stoppedComp

class AtLeast (minRow :: Row Type) (row :: Row Type) where
instance (Subset minRow row) => AtLeast minRow row where

-- Config

class Config t where
  configInterval :: t -> Int

data ConfigImplDef = ConfigImplDef String
data ConfigImpl = ConfigImpl String Int

instance StartComponent
    ConfigImplDef
    ConfigImpl
    (AtLeast Empty)
  where
    start _ (ConfigImplDef s) = pure $ ConfigImpl s (read s)

instance StopComponent
    ConfigImplDef
    ConfigImpl
  where
    stop (ConfigImpl s _) = pure $ ConfigImplDef s

instance Config ConfigImpl where
  configInterval (ConfigImpl _ n) = n

-- PrintLoop

data PrintLoopDef = PrintLoopDef String
data PrintLoop = PrintLoop String (Async.Async ())

instance
  (deps ~ AtLeast ("config" .== Some Config)) =>
  StartComponent
    PrintLoopDef
    PrintLoop
    deps
  where
    start deps (PrintLoopDef s) = do
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
      Async.cancel task
      pure $ PrintLoopDef s

-- System

data (label :: Symbol) :-> component = Label label :-> component

data system :>> component = system :>> component
infixl 6 :>>

data runningComponent :<< runningDeps = runningComponent :<< runningDeps
infixl 6 :<<

data RunningSystem runningComponents stopStack =
  RunningSystem (Rec runningComponents) stopStack

data EmptySystem = EmptySystem

instance StartComponent
    EmptySystem
    (RunningSystem Empty EmptySystem)
    (AtLeast Empty)
  where
    start _ _ = pure $ RunningSystem Empty EmptySystem

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
  ( StartComponent stoppedDepsSystem (RunningSystem runningDeps stopStack) (AtLeast Empty)
  , StartComponent stoppedComponent (label :-> runningComponent) (AtLeast runningDeps)
  , startedSystem ~ (RunningSystem (runningDeps .+ label .== runningComponent)
                                   (runningComponent :<< stopStack))
  ) =>
  StartComponent
    (stoppedDepsSystem :>> stoppedComponent)
    startedSystem
    (AtLeast Empty)
  where
    -- start :: () -> (stoppedDepsSystem :>> stoppedComponent) -> IO startedSystem
    start outerDeps (stoppedDeps :>> stoppedComponent) = do
      RunningSystem deps stopStack <- start outerDeps stoppedDeps
      (label :-> component) <- start deps stoppedComponent
      pure $ RunningSystem (deps .+ label .== component) (component :<< stopStack)


-- s1 :: IO (RunningSystem ("config" .== ConfigImpl) (ConfigImpl :<< ()))
-- s1 :: IO (Rec ("config" .== ConfigImpl))
s1 :: IO ("config" :-> ConfigImpl)
s1 = start Empty $ #config :-> ConfigImplDef "5000000"

s2 :: IO (RunningSystem ("config" .== ConfigImpl)
                        (ConfigImpl :<< EmptySystem))
s2 = start Empty $ EmptySystem
                :>> #config :-> ConfigImplDef "5000000"

system :: _
system = EmptySystem
     :>> #config :-> ConfigImplDef "5000000"
     :>> #printLoop :-> PrintLoopDef "Hello!"

-- Now all I have to do is find a way to wrap ConfigImpl in Some with a helper
system' = start Empty system
