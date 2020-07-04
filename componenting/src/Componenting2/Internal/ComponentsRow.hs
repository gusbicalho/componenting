{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Componenting2.Internal.ComponentsRow
  ( StartAllComponents (..), AllComponentsStartedWithMeta
  , StopAllComponentsWithMeta (..), AllComponentsWithMetaStopped
  , MapRowSndT, MapRowSnd (..)
  , SelectKeysT, SelectKeys (..)
  , DependenciesLabelsMap, ToDependenciesLabelsMap
  ) where

import Componenting2.Internal.Component (StartComponent (..), StopComponent (..), StartableWithDependencies)
import Componenting2.Internal.Util (FstT, SndT)
import Data.Kind (Type)
import Data.Row ( Rec, Forall, Label (..), empty, (.!), type (.!), (.==)
                , type (.==), (.+), type (.+), (.-), type (.\\)
                )
import Data.Row.Internal (Row (..), LT (..), Labels, Empty, Unconstrained1, Subset)
import Data.Row.Records (split)
import GHC.TypeLits (Symbol, KnownSymbol)

-- | Association list matching component labels to their dependencies' labels
type DependenciesLabelsMap = Row [Symbol]

-- | Extract labels of all components in a row, alongside their dependencies' labels
type family ToDependenciesLabelsMap
    (components :: Row Type)
    :: DependenciesLabelsMap where
  ToDependenciesLabelsMap ('R pairs) = 'R (GoToDependenciesLabelsMap pairs)

type family GoToDependenciesLabelsMap
    (pairs :: [LT Type])
    :: [LT [Symbol]]
  where
  GoToDependenciesLabelsMap '[] = '[]
  GoToDependenciesLabelsMap ((label ':-> compDef) ': pairs) =
    (label ':-> Labels (DependenciesSpec compDef))
      ': GoToDependenciesLabelsMap pairs

-- | Select a set of labels from a row
class
  ( Forall (SelectKeysT labels row) Unconstrained1
  , Subset (SelectKeysT labels row) row ) =>
  SelectKeys (labels :: [Symbol]) (row :: Row Type) where
  selectKeys :: Rec row -> Rec (SelectKeysT labels row)
  selectKeys r = fst $ splitOnKeys @labels r
  splitOnKeys :: Rec row -> ( Rec (SelectKeysT labels row)
                            , Rec (row .\\ SelectKeysT labels row) )

instance
  ( Forall (SelectKeysT labels row) Unconstrained1
  , Subset (SelectKeysT labels row) row
  ) =>
  SelectKeys labels row where
  splitOnKeys row = split @(SelectKeysT labels row) row

type family SelectKeysT
    (labels :: [Symbol])
    (row :: Row a)
    :: Row a
  where
  SelectKeysT '[] row = Empty
  SelectKeysT (label ': labels) row = (label .== (row .! label)) .+ SelectKeysT labels row

-- | Version of the row with all components Started
type family AllComponentsStartedWithMeta
    (components :: Row Type)
    :: Row Type
  where
  AllComponentsStartedWithMeta ('R '[]) = 'R '[]
  AllComponentsStartedWithMeta ('R ((label ':-> comp) ': morePairs)) =
    label .== (ComponentMeta comp, Started comp)
    .+ (AllComponentsStartedWithMeta ('R morePairs))

class
  StartAllComponents
    deps
    components
  where
  startAllComponents :: Rec deps
           -> Rec components
           -> IO (Rec (AllComponentsStartedWithMeta components))

instance StartAllComponents deps Empty where
  startAllComponents _ _ = pure empty

instance
  ( KnownSymbol label
  , StartableWithDependencies deps comp
  , StartAllComponents deps ('R restOfComponents)
  ) =>
  StartAllComponents
    deps
    ('R ((label ':-> comp) ': restOfComponents))
  where
  startAllComponents deps components = do
    let label = Label @label
    let comp = components .! label
    (meta, started) <- startWithMeta deps comp
    let rest = components .- label
    startedRestWithMeta <- startAllComponents deps rest
    pure $ label .== (meta, started) .+ startedRestWithMeta

-- | Takes the type of a row with where all values are (meta, runningComponent) pairs,
-- returns a row with stopped components
type family AllComponentsWithMetaStopped
    (runningComponentsWithMeta :: Row Type)
    :: Row Type
  where
  AllComponentsWithMetaStopped ('R '[]) = 'R '[]
  AllComponentsWithMetaStopped ('R ((label ':-> comp) ': morePairs)) =
    label .== (Stopped (SndT comp) (FstT comp))
    .+ AllComponentsWithMetaStopped ('R morePairs)

class StopAllComponentsWithMeta
    (runningComponentsWithMeta :: Row Type)
  where
  -- | Take a row where all values are (meta, runningComponent) pairs, stop all
  -- those components, and return a row with the stopped components
  stopAllComponentsWithMeta :: Rec runningComponentsWithMeta -> IO (Rec (AllComponentsWithMetaStopped runningComponentsWithMeta))

instance {-# OVERLAPPING #-} StopAllComponentsWithMeta Empty where
  stopAllComponentsWithMeta _ = pure empty

instance
  {-# OVERLAPPABLE #-}
  ( KnownSymbol label
  , StopComponent runningComponent meta
  , StopAllComponentsWithMeta ('R moreRunningComponentssWithMeta)
  ) =>
  StopAllComponentsWithMeta
    ('R ((label ':-> (meta, runningComponent)) ': moreRunningComponentssWithMeta))
  where
  stopAllComponentsWithMeta comps = do
    let label = Label @label
    let (meta, startedC) = comps .! label
    stopped <- stopWithMeta meta startedC
    stoppedRest <- stopAllComponentsWithMeta (comps .- label)
    pure $ label .== stopped .+ stoppedRest

-- | Picks the Snd thing from each entry in a row where all values are pairs
type family MapRowSndT (row :: Row Type) :: Row Type where
  MapRowSndT ('R '[]) = 'R '[]
  MapRowSndT ('R ((label ':-> value) ': morePairs)) =
    (label .== SndT value) .+ MapRowSndT ('R morePairs)

class MapRowSnd
    (row :: Row Type)
  where
  -- | Take a row where all values are (a, b) pairs, return a row with all b's
  mapRowSnd :: Rec row -> Rec (MapRowSndT row)

instance {-# OVERLAPPING #-} MapRowSnd Empty where
  mapRowSnd _ = empty

instance
  {-# OVERLAPPABLE #-}
  ( KnownSymbol label
  , MapRowSnd ('R morePairs)
  ) =>
  MapRowSnd
    ('R ((label ':-> (a, b)) ': morePairs))
  where
  mapRowSnd comps =
    let label = Label @label
    in label .== (snd $ comps .! label)
       .+ mapRowSnd (comps .- label)
