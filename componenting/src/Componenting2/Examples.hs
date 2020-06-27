{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Componenting2.Examples where

import Componenting2.Internal.Component
import Componenting2.Internal.System
import Data.Row (empty, (.!), (.==), type (.==), (.+), type (.+))

data Msg = Msg String
data StartedMsg = StartedMsg String deriving (Show)
instance StartComponent Msg
  where
  type Started Msg = StartedMsg
  type ComponentMeta Msg = ()
  type DependenciesSpec Msg = '[]
  start _ (Msg s) = do
    putStrLn $ "Msg> start " <> s
    pure $ StartedMsg s
instance StopComponent StartedMsg () where
  type Stopped StartedMsg () = Msg
  stopWithMeta _ (StartedMsg s) = do
    putStrLn "Msg> stop"
    pure (Msg s)

data Foo = Foo
data StartedFoo = StartedFoo deriving (Show)
instance StartComponent Foo
  where
  type Started Foo = StartedFoo
  type ComponentMeta Foo = ()
  type DependenciesSpec Foo = '[ '("show", Show) ]
  start deps Foo = do
    putStrLn "Foo> start"
    putStrLn $ "Foo> show: " <> show (deps .! #show)
    pure StartedFoo
instance StopComponent StartedFoo () where
  type Stopped StartedFoo () = Foo
  stopWithMeta () _ = do
    putStrLn "Foo> stop"
    pure Foo

data Bar = Bar
data StartedBar = StartedBar
instance StartComponent Bar
  where
  type Started Bar = StartedBar
  type ComponentMeta Bar = ()
  type DependenciesSpec Bar = '[ '("show", Show), '("foo", Show) ]
  start deps Bar = do
    putStrLn "Bar> start"
    putStrLn $ "Bar> show: " <> show (deps .! #show)
    putStrLn $ "Bar> foo: " <> show (deps .! #show)
    pure StartedBar
instance StopComponent StartedBar () where
  type Stopped StartedBar () = Bar
  stopWithMeta _ StartedBar = do
    putStrLn "Bar> stop"
    pure Bar

sys1 :: System ( "show" .== Msg
              .+ "foo" .== Foo
              .+ "bar" .== Bar)
sys1 = System $ #show .== Msg "lol"
             .+ #foo .== Foo
             .+ #bar .== Bar

startedSys1 :: IO (RunningSystem ( "show" .== Msg
                                .+ "foo" .== Foo
                                .+ "bar" .== Bar))
startedSys1 = start empty sys1

-- TODO:
-- Rename dependencies
-- Optional dependencies
