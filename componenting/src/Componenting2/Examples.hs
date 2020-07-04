{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Componenting2.Examples where

import Componenting2.Internal.Component
import Componenting2.Internal.System
import Data.Row (Rec, Empty, empty, (.!), (.==), type (.==), (.+), type (.+))

data Msg = Msg String
data StartedMsg = StartedMsg String deriving (Show)
instance StartComponent Msg
  where
  type Started Msg = StartedMsg
  type ComponentMeta Msg = ()
  type DependenciesSpec Msg = Empty
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
  type DependenciesSpec Foo = "show" .== Show
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
  type DependenciesSpec Bar = "show1" .== Show
                           .+ "show2" .== Show
  start deps Bar = do
    putStrLn "Bar> start"
    putStrLn $ "Bar> show1: " <> show (deps .! #show1)
    putStrLn $ "Bar> show2: " <> show (deps .! #show2)
    pure StartedBar
instance StopComponent StartedBar () where
  type Stopped StartedBar () = Bar
  stopWithMeta _ StartedBar = do
    putStrLn "Bar> stop"
    pure Bar

sys1 :: System ( "msg" .== Msg
              .+ "foo" .== (RenameDependency "msg" "show" Foo)
              .+ "bar" .== (RenameDependency "msg" "show1"
                            (RenameDependency "foo" "show2"
                             Bar)))
sys1 = System $ #msg .== Msg "lol"
             .+ #foo .== ( RenameDependency @"msg" @"show"
                         $ Foo)
             .+ #bar .== ( RenameDependency @"msg" @"show1"
                         $ RenameDependency @"foo" @"show2"
                         $ Bar)

startedComponents :: IO (Rec ( "msg" .== StartedMsg
                            .+ "foo" .== StartedFoo
                            .+ "bar" .== StartedBar))
startedComponents = getComponents <$> start empty sys1

-- TODO:
-- Optional dependencies
