{-# LANGUAGE UndecidableInstances #-}

module Componenting2.Internal.System
  ( System (..)
  , RunningSystem, getComponents
  )where

import Componenting2.Internal.DependencyLayers ( ToDependencyStartLayers, StartLayers (..)
                                              , ToDependencyStopLayers, StopLayers (..))
import Componenting2.Internal.Component (StartComponent (..), StopComponent (..))
import Componenting2.Internal.ComponentsRow ( MapRowSndT, MapRowSnd (..)
                                            , SelectKeysT
                                            , AllComponentsStartedWithMeta
                                            , AllComponentsWithMetaStopped
                                            )
import Componenting2.Internal.Util (Concat)
import Data.Kind (Type)
import Data.Row (Row, Rec, Empty, empty)

data System
    (components :: Row Type)
  where
  System :: ( StartComponent (System components)
            , StopComponent (RunningSystem components) ())
         => Rec components -> System components

data RunningSystem (components :: Row Type)
  = RunningSystem
      (Rec (MapRowSndT (AllComponentsStartedWithMeta components)))
      (Rec (AllComponentsStartedWithMeta components))

getComponents :: RunningSystem components -> Rec (MapRowSndT (AllComponentsStartedWithMeta components))
getComponents (RunningSystem runningComponents _) = runningComponents

instance
  ( layers ~ ToDependencyStartLayers components
  , StartLayers layers Empty components
  , components
    ~ SelectKeysT (Concat layers) components
  , MapRowSnd (AllComponentsStartedWithMeta components)
  ) =>
  StartComponent
    (System components)
  where
  type Started (System components) = RunningSystem components
  type ComponentMeta (System components) = ()
  type DependenciesSpec (System components) = '[]
  start _ (System components) = do
    startedWithMeta <- startLayers @layers empty components
    pure $ RunningSystem @components (mapRowSnd startedWithMeta) startedWithMeta

instance
  ( StartComponent (System components)
  , runningCompsWithMeta ~ AllComponentsStartedWithMeta components
  , AllComponentsWithMetaStopped runningCompsWithMeta ~ components
  , layers ~ ToDependencyStopLayers runningCompsWithMeta
  , runningCompsWithMeta
    ~ SelectKeysT (Concat layers) runningCompsWithMeta
  , StopLayers layers runningCompsWithMeta
  ) =>
  StopComponent
    (RunningSystem components) ()
  where
  type Stopped (RunningSystem components) ()
    = System components
  stopWithMeta () (RunningSystem _ runningWithMeta) = do
    stopped <- stopLayers @layers runningWithMeta
    pure $ System stopped
