
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Componenting2.Internal.DependencyLayers
  ( ToDependencyStartLayers
  , DependencyStartLayers
  , ToDependencyStopLayers
  , DependencyStopLayers
  , StartLayers (..)
  , StopLayers (..)
  ) where

import Componenting2.Internal.ComponentsRow ( DependenciesLabelsMap
                                            , ToDependenciesLabelsMap
                                            , MapRowSndT, MapRowSnd (..)
                                            , SelectKeys (..), SelectKeysT
                                            , StartAllComponents (..), AllComponentsStartedWithMeta
                                            , StopAllComponentsWithMeta (..), AllComponentsWithMetaStopped
                                            )
import Componenting2.Internal.Util (If, AllItemsKnown, RemoveLabels, type (++), Reverse, Concat)
import Data.Kind (Type)
import Data.Row (Rec, type (.\\), type (.+), (.+), empty, Empty)
import Data.Row.Internal (Row (..), LT (..), Labels)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage (..))

type Layer = [Symbol]
type DependencyStartLayers = [Layer]
type DependencyStopLayers = [Layer]

-- | Process a Row Type of components to find the order in which they can be
-- started (and later stopped), expressed as a type-level list of Layers, where
-- each Layer is a list of names of components that can be started after all
-- previous layers are started.
type family ToDependencyStartLayers
    (systemDef :: Row Type)
    :: DependencyStartLayers
  where
    ToDependencyStartLayers systemDef =
      DependenciesLabelsMapToStartLayers
        '[]
        (ToDependenciesLabelsMap systemDef)

type family ToDependencyStopLayers
    (runningCompsWithMeta :: Row Type)
    :: DependencyStopLayers
  where
  ToDependencyStopLayers runningCompsWithMeta =
    Reverse (ToDependencyStartLayers (AllComponentsWithMetaStopped runningCompsWithMeta))

type family DependenciesLabelsMapToStartLayer
    (providedDeps :: [Symbol])
    (compDepsMap :: DependenciesLabelsMap)
    :: Layer
  where
  DependenciesLabelsMapToStartLayer providedDeps ('R '[]) = '[]
  DependenciesLabelsMapToStartLayer providedDeps ('R ((compLabel ':-> requiredDeps) ': morePairs)) =
    If (AllItemsKnown providedDeps requiredDeps)
      (compLabel ': DependenciesLabelsMapToStartLayer providedDeps ('R morePairs))
      (DependenciesLabelsMapToStartLayer providedDeps ('R morePairs))

type family DependenciesLabelsMapToStartLayers
    (providedDeps :: [Symbol])
    (compDepsMap :: DependenciesLabelsMap)
    :: DependencyStartLayers
  where
    DependenciesLabelsMapToStartLayers providedDeps Empty = '[]
    DependenciesLabelsMapToStartLayers providedDeps compDepsMap =
      NextStartLayer
        providedDeps
        compDepsMap
        (DependenciesLabelsMapToStartLayer providedDeps compDepsMap)

type family NextStartLayer
    (providedDeps :: [Symbol])
    (compDepsMap :: DependenciesLabelsMap)
    (startableComponents :: Layer)
    :: DependencyStartLayers
  where
  NextStartLayer providedDeps compDepsMap '[] =
    TypeError
      ('Text "Unsolvable dependencies in system. These components have missing dependencies: " ':$$:
       'ShowType (Labels compDepsMap) ':$$:
       'Text "even after starting these dependencies: " ':<>: 'ShowType providedDeps)
  NextStartLayer providedDeps compDepsMap startableComponents =
    startableComponents ': (DependenciesLabelsMapToStartLayers
                              (providedDeps ++ startableComponents)
                              (RemoveLabels startableComponents compDepsMap))

-- Start a Layer

class StartLayer
    (layer :: [Symbol])
    (deps :: Row Type)
    (components :: Row Type)
  where
    startLayer :: Rec deps
               -> Rec components
               -> IO (Rec (AllComponentsStartedWithMeta
                            (SelectKeysT layer components)))

instance
  ( SelectKeys layer components
  , StartAllComponents deps (SelectKeysT layer components)) =>
  StartLayer
    layer
    deps
    components
  where
    startLayer deps components = startAllComponents deps (selectKeys @layer components)

-- | Start a series of Layers
class StartLayers
    (layers :: DependencyStartLayers)
    (deps :: Row Type)
    (components :: Row Type)
  where
    startLayers :: Rec deps
                   -> Rec components
                   -> IO (Rec (AllComponentsStartedWithMeta
                                (SelectKeysT (Concat layers) components)))

instance StartLayers '[] deps components where
  startLayers _ _ = pure empty

instance
  ( StartLayer layer deps components
  , startedWithMeta ~ AllComponentsStartedWithMeta (SelectKeysT layer components)
  , MapRowSnd startedWithMeta
  , StartLayers moreLayers (deps .+ (MapRowSndT startedWithMeta)) components
  , AllSelectedComponentsStartedWithMetaDistributesOverConcat layer moreLayers components
  ) =>
  StartLayers
    (layer ': moreLayers)
    deps
    components
  where
    startLayers deps components = do
      startedWithMeta <- startLayer @layer deps components
      let started = mapRowSnd startedWithMeta
      moreStartedWithMeta <- startLayers @moreLayers (deps .+ started) components
      pure $ startedWithMeta .+ moreStartedWithMeta

type AllSelectedComponentsStartedWithMetaDistributesOverConcat layer moreLayers components =
  (AllComponentsStartedWithMeta (SelectKeysT layer components)
    .+ AllComponentsStartedWithMeta
        (SelectKeysT (Concat moreLayers) components))
  ~ AllComponentsStartedWithMeta
      (SelectKeysT (layer ++ Concat moreLayers) components)

-- | Stop a Layer
class StopLayer
    (layer :: [Symbol])
    (runningComponentsWithMeta :: Row Type)
  where
    stopLayer :: Rec runningComponentsWithMeta
              -> IO ( Rec (AllComponentsWithMetaStopped
                            (SelectKeysT layer runningComponentsWithMeta))
                    , Rec (runningComponentsWithMeta
                            .\\ SelectKeysT layer runningComponentsWithMeta))

instance
  ( SelectKeys layer runningComponentsWithMeta
  , StopAllComponentsWithMeta (SelectKeysT layer runningComponentsWithMeta)) =>
  StopLayer
    layer
    runningComponentsWithMeta
  where
    stopLayer components = do
      let (stoppable, stillRunning) = splitOnKeys @layer components
      stopped <- stopAllComponentsWithMeta stoppable
      pure (stopped, stillRunning)

-- | Stop a series of Layers
class StopLayers
    (layers :: DependencyStopLayers)
    (components :: Row Type)
  where
    stopLayers :: Rec components
               -> IO (Rec (AllComponentsWithMetaStopped
                            (SelectKeysT (Concat layers) components)))

instance StopLayers '[] components where
  stopLayers _ = pure empty

instance
  ( StopLayer layer runningComponentsWithMeta
  , StopLayers moreLayers (runningComponentsWithMeta
                            .\\ SelectKeysT layer runningComponentsWithMeta)
  , StoppingOneLayerAtATimeIsTheSameAsStoppingAllLayers layer moreLayers runningComponentsWithMeta
  ) =>
  StopLayers
    (layer ': moreLayers)
    runningComponentsWithMeta
  where
    stopLayers runningComponentsWithMeta = do
      (stopped, stillRunning) <- stopLayer @layer runningComponentsWithMeta
      moreStopped <- stopLayers @moreLayers stillRunning
      pure $ stopped .+ moreStopped

type StoppingOneLayerAtATimeIsTheSameAsStoppingAllLayers layer moreLayers runningComponentsWithMeta =
  (AllComponentsWithMetaStopped
    (SelectKeysT layer runningComponentsWithMeta)
  .+ AllComponentsWithMetaStopped
      (SelectKeysT
        (Concat moreLayers)
        (runningComponentsWithMeta .\\ SelectKeysT layer runningComponentsWithMeta)))
  ~
  (AllComponentsWithMetaStopped
    (SelectKeysT (layer ++ Concat moreLayers) runningComponentsWithMeta))
