{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Componenting2.Component where

import Data.Row
import qualified Data.Row.Internal as RI
import Data.Row.Records
import Data.Kind
import GHC.TypeLits

class StartComponent
  (stoppedC :: Type)
  where
  type Started stoppedC :: Type
  type ComponentMeta stoppedC :: Type
  type DependenciesSpec stoppedC :: [(Symbol, Type -> Constraint)]

  startWithMeta :: Dependencies (DependenciesSpec stoppedC) row
                => Rec row -> stoppedC -> IO (ComponentMeta stoppedC, Started stoppedC)
  default startWithMeta :: ( Dependencies (DependenciesSpec stoppedC) row
                           , ComponentMeta stoppedC ~ () )
                        => Rec row -> stoppedC -> IO (ComponentMeta stoppedC, Started stoppedC)
  startWithMeta deps stoppedC = ((), ) <$> start deps stoppedC

  start :: Dependencies (DependenciesSpec stoppedC) row
        => Rec row -> stoppedC -> IO (Started stoppedC)
  start deps stoppedC = snd <$> startWithMeta deps stoppedC

class
  (StartComponent (Stopped startedC meta)
  , meta ~ ComponentMeta (Stopped startedC meta) ) =>
  StopComponent
    (startedC :: Type)
    (meta :: Type)
  where
  type Stopped startedC meta :: Type
  stopWithMeta :: meta -> startedC -> IO (Stopped startedC meta)

stop :: StopComponent startedC ()
     => startedC -> IO (Stopped startedC ())
stop comp = stopWithMeta () comp

-- test comps

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

sys1 :: _
sys1 = System $ #show .== Msg "lol"
             .+ #foo .== Foo
             .+ #bar .== Bar

startedSys1 :: _
startedSys1 = start empty sys1

-- startAndStopSys1 = do
--   started

-- System

data System
    (systemDef :: Row Type)
  where
  System :: ( StartComponent (System systemDef)
            , StopComponent (RunningSystem systemDef) ())
         => Rec systemDef -> System systemDef

data RunningSystem (systemDef :: Row Type)
  = RSys { getComponents :: Rec (AllStarted systemDef)
         , getComponentsWithMeta :: Rec (AllStartedWithMeta systemDef)
         }

instance
  ( systemDef ~ (Empty .+ systemDef)
  , StartAllLayers (ToSystemStartLayers systemDef) Empty systemDef
  , systemDef
    ~ SelectMatchingLabels (Concat (ToSystemStartLayers systemDef)) systemDef
  ) =>
  StartComponent
    (System systemDef)
  where
  type Started (System systemDef) = RunningSystem systemDef
  type ComponentMeta (System systemDef) = ()
  type DependenciesSpec (System systemDef) = '[]
  start _ (System systemDef) = do
    (startedWithMeta, started) <- startAllLayers @(ToSystemStartLayers systemDef) empty systemDef
    pure $ RSys @systemDef started startedWithMeta

instance
  ( StartComponent (System systemDef)
  , runningCompsWithMeta ~ AllStartedWithMeta systemDef
  , AllComponentsWithMetaStopped runningCompsWithMeta ~ systemDef
  , runningCompsWithMeta
    ~ SelectMatchingLabels (Concat (ToSystemStopLayers runningCompsWithMeta)) runningCompsWithMeta
  , StopAllLayers (ToSystemStopLayers runningCompsWithMeta) runningCompsWithMeta ) =>
  StopComponent
    (RunningSystem systemDef) ()
  where
  type Stopped (RunningSystem systemDef) ()
    = System systemDef
  stopWithMeta () (RSys _ runningWithMeta) = do
    stopped <- stopAllLayers @(ToSystemStopLayers runningCompsWithMeta) runningWithMeta
    pure $ System stopped

noDeps :: RunningSystem Empty
noDeps = RSys empty empty

type family ToSystemStartLayers
    (systemDef :: Row Type)
    :: SystemStartLayers
  where
    ToSystemStartLayers systemDef =
      DependencyNamesMapToSystemStartLayers
        '[]
        (SystemDefToDependencyNamesMap systemDef)

type family ToSystemStopLayers
    (runningCompsWithMeta :: Row Type)
    :: SystemStopLayers
  where
  ToSystemStopLayers runningCompsWithMeta =
    Reverse (ToSystemStartLayers (AllComponentsWithMetaStopped runningCompsWithMeta))

type DependencyNamesMap = [(Symbol, [Symbol])]

-- Turn a Row Type into a list of (componentName, [dependencyNames]) pairs
type family SystemDefToDependencyNamesMap
    (systemDef :: Row Type)
    :: DependencyNamesMap where
  SystemDefToDependencyNamesMap Empty = '[]
  SystemDefToDependencyNamesMap ('RI.R ((label 'RI.:-> compDef) ': pairs)) =
    '(label, ListLabels (DependenciesSpec compDef))
      ': SystemDefToDependencyNamesMap ('RI.R pairs)

type StartableComponents = [Symbol]
type SystemStartLayers = [StartableComponents]
type StoppableComponents = [Symbol]
type SystemStopLayers = [StoppableComponents]

type family DependencyNamesMapToStartableComponents
    (providedDeps :: [Symbol])
    (compDepsMap :: DependencyNamesMap)
    :: StartableComponents
  where
  DependencyNamesMapToStartableComponents providedDeps '[] = '[]
  DependencyNamesMapToStartableComponents providedDeps ('(compLabel, requiredDeps) ': morePairs) =
    If (AllItemsKnown providedDeps requiredDeps)
      (compLabel ': DependencyNamesMapToStartableComponents providedDeps morePairs)
      (DependencyNamesMapToStartableComponents providedDeps morePairs)

type family DependencyNamesMapToSystemStartLayers
    (providedDeps :: [Symbol])
    (compDepsMap :: DependencyNamesMap)
    :: SystemStartLayers
  where
    DependencyNamesMapToSystemStartLayers providedDeps '[] = '[]
    DependencyNamesMapToSystemStartLayers providedDeps compDepsMap =
      NextSystemStartLayer
        providedDeps
        compDepsMap
        (DependencyNamesMapToStartableComponents providedDeps compDepsMap)

type family NextSystemStartLayer
    (providedDeps :: [Symbol])
    (compDepsMap :: DependencyNamesMap)
    (startableComponents :: StartableComponents)
    :: SystemStartLayers
  where
  NextSystemStartLayer providedDeps compDepsMap '[] =
    TypeError
      ('Text "Unsolvable dependencies in system. These components have missing dependencies: " ':$$:
       'ShowType (ListLabels compDepsMap) ':$$:
       'Text "even after starting these dependencies: " ':<>: 'ShowType providedDeps)
  NextSystemStartLayer providedDeps compDepsMap startableComponents =
    startableComponents ': (DependencyNamesMapToSystemStartLayers
                              (providedDeps ++ startableComponents)
                              (RemoveLabels startableComponents compDepsMap))


-- Starting all values in a Row with a single dependencies record

class
  ( StartComponent comp
  , Dependencies (DependenciesSpec comp) depsRow ) =>
  StartableWithDependencies depsRow comp
instance
  ( StartComponent comp
  , Dependencies (DependenciesSpec comp) depsRow ) =>
  StartableWithDependencies depsRow comp

class
  ( Forall systemDefRow (StartableWithDependencies depsRow) ) =>
  StartAll
    depsRow
    systemDefRow
  where
  startAll :: Rec depsRow
           -> Rec systemDefRow
           -> IO ( Rec (AllStartedWithMeta systemDefRow)
                 , Rec (AllStarted systemDefRow))

instance StartAll depsRow Empty where
  startAll _ _ = pure (empty, empty)

instance
  ( KnownSymbol label
  , StartableWithDependencies depsRow comp
  , StartAll depsRow ('RI.R restOfSystemDefRow)
  , AllStartedIsAssociative label comp restOfSystemDefRow
  , AllStartedWithMetaIsAssociative label comp restOfSystemDefRow
  ) =>
  StartAll
    depsRow
    ('RI.R ((label 'RI.:-> comp) ': restOfSystemDefRow))
  where
  startAll deps systemDef = do
    let label = Label @label
    let comp = systemDef .! label
    (meta, started) <- startWithMeta deps comp
    let rest = systemDef .- label
    (startedRestWithMeta, startedRest) <- startAll deps rest
    pure $ ( label .== (meta, started) .+ startedRestWithMeta
           , label .== started .+ startedRest)

class StartAllLabels
    (labels :: [Symbol])
    (deps :: Row Type)
    (systemDef :: Row Type)
  where
    startAllLabels :: Rec deps
                   -> Rec systemDef
                   -> IO (Rec (AllStartedWithMeta
                                (SelectMatchingLabels labels systemDef))
                         ,Rec (AllStarted
                                (SelectMatchingLabels labels systemDef)))

instance
  ( SelectAll labels systemDef
  , StartAll deps (SelectMatchingLabels labels systemDef)) =>
  StartAllLabels
    labels
    deps
    systemDef
  where
    startAllLabels deps systemDef = startAll deps (selectAll @labels systemDef)

-- Traversing the SystemStartLayers
class StartAllLayers
    (layers :: SystemStartLayers)
    (deps :: Row Type)
    (systemDef :: Row Type)
  where
    startAllLayers :: Rec deps
                   -> Rec systemDef
                   -> IO (Rec (AllStartedWithMeta
                                (SelectMatchingLabels (Concat layers) systemDef))
                         ,Rec (AllStarted
                                (SelectMatchingLabels (Concat layers) systemDef)))

instance StartAllLayers '[] deps systemDef where
  startAllLayers _ _ = pure (empty, empty)

instance
  ( StartAllLabels layer deps systemDef
  , StartAllLayers moreLayers (deps .+ AllStarted (SelectMatchingLabels layer systemDef)) systemDef
  , AllStartedDistributesOverConcat layer moreLayers systemDef
  , AllStartedWithMetaDistributesOverConcat layer moreLayers systemDef
  ) =>
  StartAllLayers
    (layer ': moreLayers)
    deps
    systemDef
  where
    startAllLayers deps systemDef = do
      (startedWithMeta, started) <- startAllLabels @layer deps systemDef
      (moreStartedWithMeta, moreStarted) <- startAllLayers @moreLayers (deps .+ started) systemDef
      pure $ ( startedWithMeta .+ moreStartedWithMeta
             , started .+ moreStarted )

-- Stop a full row of components

class
  StopAll
    (runningCompsWithMeta :: Row Type)
  where
  stopAll :: Rec runningCompsWithMeta -> IO (Rec (AllComponentsWithMetaStopped runningCompsWithMeta))

instance {-# OVERLAPPING #-} StopAll Empty where
  stopAll _ = pure empty

instance
  {-# OVERLAPPABLE #-}
  ( KnownSymbol label
  , StopComponent comp meta
  , AllComponentsWithMetaStoppedIsAssociative label meta comp moreRunningCompsWithMeta
  , StopAll ('RI.R moreRunningCompsWithMeta)
  ) =>
  StopAll
    ('RI.R ((label 'RI.:-> (meta, comp)) ': moreRunningCompsWithMeta))
  where
  stopAll comps = do
    let label = Label @label
    let (meta, startedC) = comps .! label
    stopped <- stopWithMeta meta startedC
    stoppedRest <- stopAll (comps .- label)
    pure $ label .== stopped .+ stoppedRest

class StopAllLabels
    (labels :: [Symbol])
    (runningCompsWithMeta :: Row Type)
  where
    stopAllLabels :: Rec runningCompsWithMeta
                  -> IO ( Rec (AllComponentsWithMetaStopped
                                (SelectMatchingLabels labels runningCompsWithMeta))
                        , Rec (runningCompsWithMeta .\\ SelectMatchingLabels labels runningCompsWithMeta) )

instance
  ( StopAll (SelectMatchingLabels labels runningComps)
  , Forall (SelectMatchingLabels labels runningComps) RI.Unconstrained1
  , RI.Subset (SelectMatchingLabels labels runningComps) runningComps ) =>
  StopAllLabels
    labels
    runningComps
  where
    stopAllLabels runningComps = do
      let (stoppable, notStoppable) = split @(SelectMatchingLabels labels runningComps) runningComps
      stopped <- stopAll stoppable
      pure (stopped, notStoppable)

-- Traversing the SystemStopLayers
class StopAllLayers
    (layers :: SystemStopLayers)
    (runningCompsWithMeta :: Row Type)
  where
    stopAllLayers :: Rec runningCompsWithMeta
                   -> IO (Rec (AllComponentsWithMetaStopped
                                (SelectMatchingLabels (Concat layers) runningCompsWithMeta)))

instance StopAllLayers '[] runningCompsWithMeta where
  stopAllLayers _ = pure empty

instance
  ( StopAllLabels layer runningCompsWithMeta
  , StopAllLayers moreLayers (runningCompsWithMeta .\\ SelectMatchingLabels layer runningCompsWithMeta)
  , ( AllComponentsWithMetaStopped
        (SelectMatchingLabels layer runningCompsWithMeta)
    .+
      AllComponentsWithMetaStopped
        (SelectMatchingLabels
          (Concat moreLayers)
          (runningCompsWithMeta .\\ SelectMatchingLabels layer runningCompsWithMeta))
    ) ~ AllComponentsWithMetaStopped
          (SelectMatchingLabels
            (layer ++ Concat moreLayers)
            runningCompsWithMeta)
  ) =>
  StopAllLayers
    (layer ': moreLayers)
    runningCompsWithMeta
  where
    stopAllLayers runningCompsWithMeta = do
      (stopped, stillRunning) <- stopAllLabels @layer runningCompsWithMeta
      moreStopped <- stopAllLayers @moreLayers stillRunning
      pure $ stopped .+ moreStopped

-- Processing rows of components

class SelectAll (labels :: [Symbol]) (components :: Row Type) where
  selectAll :: Rec components -> Rec (SelectMatchingLabels labels components)

instance SelectAll '[] components where
  selectAll _ = empty

instance
  ( KnownSymbol label
  , SelectAll labels components
  ) =>
  SelectAll (label ': labels) components where
  selectAll components =
    let l = Label @label
    in (l .== components .! l)
       .+ selectAll @labels components

type family SelectMatchingLabels
    (labels :: [Symbol])
    (row :: Row a)
    :: Row a
  where
  SelectMatchingLabels '[] row = Empty
  SelectMatchingLabels (label ': labels) row = (label .== (row .! label)) .+ SelectMatchingLabels labels row

-- type family PairsMatchingLabels
--     (labels :: [Symbol])
--     (pairs :: [RI.LT a])
--     :: [RI.LT a]
--   where
--     PairsMatchingLabels '[]  = '[]
--     PairsMatchingLabels labels '[] = '[]
--     PairsMatchingLabels labels ((label 'RI.:-> v) ': morePairs) =
--       If (ItemKnown labels label)
--         ((label 'RI.:-> v) ': PairsMatchingLabels labels morePairs)
--         (PairsMatchingLabels labels morePairs)

type family AllStartedWithMeta
    (components :: Row Type)
    :: Row Type
  where
  AllStartedWithMeta ('RI.R pairs) = 'RI.R (GoAllStartedWithMeta pairs)

type family GoAllStartedWithMeta (pairs :: [RI.LT a]) :: [RI.LT a]
  where
  GoAllStartedWithMeta '[] = '[]
  GoAllStartedWithMeta ((label 'RI.:-> comp) ': morePairs) =
    (label 'RI.:-> (ComponentMeta comp, Started comp))
      ': GoAllStartedWithMeta morePairs

type AllStartedWithMetaIsAssociative label comp restOfRow =
  AllStartedWithMeta ('RI.R ((label 'RI.:-> comp) ': restOfRow))
  ~ ((label .== (ComponentMeta comp, Started comp)) .+ (AllStartedWithMeta ('RI.R restOfRow)))

type AllStartedWithMetaDistributesOverConcat layer moreLayers systemDef =
  (AllStartedWithMeta
    (SelectMatchingLabels layer systemDef)
  .+ AllStartedWithMeta
      (SelectMatchingLabels (Concat moreLayers) systemDef))
  ~ AllStartedWithMeta
      (SelectMatchingLabels (layer ++ Concat moreLayers) systemDef)

type family AllStarted
    (components :: Row Type)
    :: Row Type
  where
  AllStarted ('RI.R pairs) = 'RI.R (GoAllStarted pairs)

type family GoAllStarted (pairs :: [RI.LT a]) :: [RI.LT a]
  where
  GoAllStarted '[] = '[]
  GoAllStarted ((label 'RI.:-> comp) ': morePairs) =
    (label 'RI.:-> Started comp)
      ': GoAllStarted morePairs

type AllStartedIsAssociative label comp restOfRow =
  AllStarted ('RI.R ((label 'RI.:-> comp) ': restOfRow))
  ~ ((label .== Started comp) .+ (AllStarted ('RI.R restOfRow)))

type AllStartedDistributesOverConcat layer moreLayers systemDef =
  (AllStarted
    (SelectMatchingLabels layer systemDef)
  .+ AllStarted
      (SelectMatchingLabels (Concat moreLayers) systemDef))
  ~ AllStarted
      (SelectMatchingLabels (layer ++ Concat moreLayers) systemDef)

type family AllComponentsWithMetaStopped
    (runningCompsWithMeta :: Row Type)
    :: Row Type
  where
  AllComponentsWithMetaStopped ('RI.R pairs) = 'RI.R (GoAllComponentsWithMetaStopped pairs)

type family GoAllComponentsWithMetaStopped (pairs :: [RI.LT a]) :: [RI.LT a]
  where
  GoAllComponentsWithMetaStopped '[] = '[]
  GoAllComponentsWithMetaStopped ((label 'RI.:-> (meta, comp)) ': morePairs) =
    (label 'RI.:-> (Stopped comp meta))
      ': GoAllComponentsWithMetaStopped morePairs
  GoAllComponentsWithMetaStopped ((label 'RI.:-> something) ': _) =
    TypeError ('Text "Value found under label " ':<>: 'ShowType label ':<>: 'Text " is not stoppable:"
               ':$$: 'ShowType something)

type AllComponentsWithMetaStoppedIsAssociative label meta comp restOfRow =
  AllComponentsWithMetaStopped ('RI.R ((label 'RI.:-> (meta, comp)) ': restOfRow))
  ~ ((label .== Stopped comp meta) .+ (AllComponentsWithMetaStopped ('RI.R restOfRow)))

type AllComponentsWithMetaStoppedDistributesOverConcat layer moreLayers runningComps =
  (AllComponentsWithMetaStopped
    (SelectMatchingLabels layer runningComps)
  .+ AllComponentsWithMetaStopped
      (SelectMatchingLabels (Concat moreLayers) runningComps))
  ~ AllComponentsWithMetaStopped
      (SelectMatchingLabels (layer ++ Concat moreLayers) runningComps)

type family ComponentLabels (components :: Row Type) :: [Symbol]
  where
  ComponentLabels ('RI.R pairs) = GoComponentLabels pairs

type family GoComponentLabels (pairs :: [RI.LT a]) :: [Symbol]
  where
  GoComponentLabels '[] = '[]
  GoComponentLabels ((label 'RI.:-> value) ': morePairs) =
    label ': GoComponentLabels morePairs

type family ComponentsWithSatisfiedDeps
    (providedDeps :: [Symbol])
    (components :: Row Type)
    :: Row Type
  where
  ComponentsWithSatisfiedDeps providedDeps ('RI.R pairs) = 'RI.R (GoComponentsWithSatisfiedDeps providedDeps pairs)

type family GoComponentsWithSatisfiedDeps
    (providedDeps :: [Symbol])
    (componentPairs :: [RI.LT Type])
    :: [RI.LT Type]
  where
  GoComponentsWithSatisfiedDeps providedDeps '[] = '[]
  GoComponentsWithSatisfiedDeps providedDeps ((label 'RI.:-> value) ': morePairs) =
    If (AllDepsProvided providedDeps (DependenciesSpec value))
      ((label 'RI.:-> value) ': GoComponentsWithSatisfiedDeps providedDeps morePairs)
      (GoComponentsWithSatisfiedDeps providedDeps morePairs)

type family ComponentsWithMissingDeps
    (providedDeps :: [Symbol])
    (components :: Row Type)
    :: Row Type
  where
  ComponentsWithMissingDeps providedDeps ('RI.R pairs) = 'RI.R (GoComponentsWithMissingDeps providedDeps pairs)

type family GoComponentsWithMissingDeps
    (providedDeps :: [Symbol])
    (componentPairs :: [RI.LT Type])
    :: [RI.LT Type]
  where
  GoComponentsWithMissingDeps providedDeps '[] = '[]
  GoComponentsWithMissingDeps providedDeps ((label 'RI.:-> value) ': morePairs) =
    If (AllDepsProvided providedDeps (DependenciesSpec value))
      (GoComponentsWithMissingDeps providedDeps morePairs)
      ((label 'RI.:-> value) ': GoComponentsWithMissingDeps providedDeps morePairs)

type family AllDepsProvided
    (providedDeps :: [Symbol])
    (dependenciesSpec :: [(Symbol, k)])
    :: Bool
  where
    AllDepsProvided providedDeps '[] = 'True
    AllDepsProvided providedDeps ('(label, v) ': moreDepsSpec) =
      If (ItemKnown providedDeps label)
        (AllDepsProvided providedDeps moreDepsSpec)
        'False

-- Dependencies constraints

class NoDependencies (t :: Row Type) where
instance NoDependencies t where

class
  (constraint (row .! label)) =>
  Dependency
    (label :: Symbol)
    (constraint :: Type -> Constraint)
    (row :: Row Type)
  where

instance
  (constraint (row .! label)) =>
  Dependency label constraint row where

class (constraint1 row, constraint2 row) => And constraint1 constraint2 (row :: Row Type) where
instance (constraint1 row, constraint2 row) => And constraint1 constraint2 row where

type family Dependencies (deps :: [(Symbol, Type -> Constraint)]) :: Row Type -> Constraint where
  Dependencies '[] = NoDependencies
  Dependencies ('(label, constraint) ': deps) = And (Dependency label constraint)
                                                    (Dependencies deps)

type family ListLabels (labeledList :: [(Symbol, k)]) :: [Symbol] where
  ListLabels '[] = '[]
  ListLabels ('(label, v) ': morePairs) = AddNonDuplicate (ListLabels morePairs) label

type family RemoveLabels
    (labelsToRemove :: [Symbol])
    (labeledList :: [(Symbol, k)])
    :: [(Symbol, k)]
  where
    RemoveLabels _ '[] = '[]
    RemoveLabels labelsToRemove ('(label, v) ': morePairs) =
      If (ItemKnown labelsToRemove label)
        (RemoveLabels labelsToRemove morePairs)
        ('(label, v) ': RemoveLabels labelsToRemove morePairs)

-- Helper type families

type family If (cond :: Bool) (thenT :: k) (elseT :: k) :: k where
  If 'True thenT _ = thenT
  If 'False _ elseT = elseT

infixr 5 ++
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
    '[]       ++ ys = ys
    (x ': xs) ++ ys = x ': xs ++ ys

-- This has horrible perf but oh well
type family Concat (xss :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat (xs ': xss) = xs ++ Concat xss

type family Reverse (xs :: [k]) :: [k] where
  Reverse xs = GoReverse xs '[]

type family GoReverse (xs :: [k]) (rs :: [k]) :: [k] where
  GoReverse '[] rs = rs
  GoReverse (x ': xs) rs = GoReverse xs (x ': rs)

type family Fst (pair :: (k, l)) :: k where
  Fst '(a, b) = a

type family Snd (pair :: (k, l)) :: l where
  Snd '(a, b) = b

type family FstT (pair :: Type) :: Type where
  FstT (a, b) = a
  FstT t = TypeError ('Text "Not a tuple type: " ':<>: 'ShowType t)

type family SndT (pair :: Type) :: Type where
  SndT (a, b) = b
  SndT t = TypeError ('Text "Not a tuple type: " ':<>: 'ShowType t)

type family IsEmpty (list :: [k]) :: Bool where
  IsEmpty '[] = 'True
  IsEmpty _ = 'False

-- TODO use some type-level set to avoid n^2
type family ItemKnown (knownItems :: [k]) (item :: k) :: Bool where
  ItemKnown '[] _ = 'False
  ItemKnown (l ': ls) l = 'True
  ItemKnown (_ ': ls) l = ItemKnown ls l

type family AddNonDuplicate (knownItems :: [k]) (item :: k) :: [k] where
  AddNonDuplicate knownItems item =
    If (ItemKnown knownItems item)
      knownItems
      (item : knownItems)

type family AllItemsKnown (knownItems :: [k]) (items :: [k]) :: Bool where
  AllItemsKnown _ '[] = 'True
  AllItemsKnown '[] _ = 'False
  AllItemsKnown knownItems (item ': moreItems) =
    If (ItemKnown knownItems item)
      (AllItemsKnown knownItems moreItems)
      'False
