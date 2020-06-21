{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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
  (StartComponent (Stopped startedC)) =>
  StopComponent
    (startedC :: Type)
  where
  type Stopped startedC :: Type
  stopWithMeta :: ComponentMeta (Stopped startedC) -> startedC -> IO (Stopped startedC)

stop :: ( StopComponent startedC
        , ComponentMeta (Stopped startedC) ~ () )
      => startedC -> IO (Stopped startedC)
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
instance StopComponent StartedMsg where
  type Stopped StartedMsg = Msg
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
instance StopComponent StartedFoo where
  type Stopped StartedFoo = Foo
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
instance StopComponent StartedBar where
  type Stopped StartedBar = Bar
  stopWithMeta _ StartedBar = do
    putStrLn "Bar> stop"
    pure Bar

-- sys1 :: System ("bar" .== Bar
            --  .+ "foo" .== Foo
            --  .+ "show" .== Msg)
-- TODO: problem, if a dep is missing, we get a stupid error message
sys1Def = #show .== Msg "lol"
       .+ #foo .== Foo
       .+ #bar .== Bar

a = toSystemStartStack @'[] $ #show .== Msg "lol"
                           .+ #foo .== Foo
                           .+ #bar .== Bar

sys1 = System sys1Def

startedSys1 :: IO (RunningSystem
                      ("bar" .== Bar
                    .+ "foo" .== Foo
                    .+ "show" .== Msg)
                      ("bar" .== StartedBar
                    .+ "foo" .== StartedFoo
                    .+ "show" .== StartedMsg)
                     '[ ("bar" .== ((), StartedBar))
                      , ("foo" .== ((), StartedFoo))
                      , ("show" .== ((), StartedMsg)) ]
                  )
startedSys1 = start empty sys1

-- startAndStopSys1 = do
--   started

-- System

data System
    (systemDef :: Row Type)
    (runningComps :: Row Type)
    (systemStack :: [Row Type])
  where
  System :: ( StartSystem Empty '[] systemDef runningComps systemStack
            , StopStack systemStack Empty systemDef )
         => Rec systemDef -> System systemDef runningComps systemStack

data SystemStartStack (stack :: [Row Type]) where
  EmptySystemStartStack :: SystemStartStack '[]
  (:>>) :: (Forall row StartComponent)
        => Rec row -> SystemStartStack rows -> SystemStartStack (row ': rows)

data SystemStopStack (stack :: [Row Type]) where
  EmptySystemStopStack :: SystemStopStack '[]
  (:<<) :: Rec row -> SystemStopStack rows -> SystemStopStack (row ': rows)

data RunningSystem (systemDef :: Row Type) (componentsRow :: Row Type) (systemStack :: [Row Type])
  = RSys { getComponents :: Rec componentsRow
         , getStopSystemStack :: SystemStopStack systemStack
         }

instance StartComponent
    (System systemDef runningComps systemStack)
  where
  type Started (System systemDef runningComps systemStack)
    = RunningSystem systemDef runningComps systemStack
  type ComponentMeta (System systemDef runningComps systemStack) = ()
  type DependenciesSpec (System systemDef runningComps systemStack) = '[]
  start _ (System systemDef) = startSystem noDeps systemDef

instance
  ( StartSystem Empty '[] systemDef runningComps systemStack
  , StopStack systemStack Empty systemDef) =>
  StopComponent
    (RunningSystem systemDef runningComps systemStack)
  where
  type Stopped (RunningSystem systemDef runningComps systemStack)
    = System systemDef runningComps systemStack
  stopWithMeta () (RSys _ stack) = do
    stopped <- stopStack empty stack
    pure $ System stopped

noDeps :: RunningSystem systemDef Empty '[]
noDeps = RSys empty EmptySystemStopStack

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
    startedSystemRow startedCompsWithMetaRow
    | depsRow systemDefRow -> startedSystemRow startedCompsWithMetaRow
  where
  startAll :: Rec depsRow -> Rec systemDefRow -> IO (Rec startedCompsWithMetaRow, Rec startedSystemRow)

instance StartAll depsRow Empty Empty Empty where
  startAll _ _ = pure (empty, empty)

instance
  ( KnownSymbol label
  , StartableWithDependencies depsRow comp
  , StartAll depsRow ('RI.R restOfSystemDefRow) ('RI.R restOfStartedSystemRow) ('RI.R restOftartedCompsWithMetaRow)
  , startedCompsWithMetaRow ~ ((label .== (ComponentMeta comp, Started comp)) .+ ('RI.R restOftartedCompsWithMetaRow))
  , startedSystemRow ~ ((label .== Started comp) .+ ('RI.R restOfStartedSystemRow))
  ) =>
  StartAll
    depsRow
    ('RI.R ((label 'RI.:-> comp) ': restOfSystemDefRow))
    startedSystemRow startedCompsWithMetaRow
  where
  startAll deps systemDef = do
    let label = Label @label
    let comp = systemDef .! label
    (meta, started) <- startWithMeta deps comp
    let rest = systemDef .- label
    (startedRestWithMeta, startedRest) <- startAll deps rest
    pure $ ( label .== (meta, started) .+ startedRestWithMeta
           , label .== started .+ startedRest)

-- Take all startable components from a components map

class
  ( Forall systemDef StartComponent
  , Forall startableComponents StartComponent
  , Forall (systemDef .\\ startableComponents) StartComponent ) =>
  SplitStartableComponents
    (depsLabels :: [Symbol])
    (systemDef :: Row Type)
    (startableComponents :: Row Type)
    | depsLabels systemDef -> startableComponents
  where
    splitStartableComponents :: Rec systemDef
                             -> (Rec startableComponents, Rec (systemDef .\\ startableComponents))

instance
  SplitStartableComponents depsLabels Empty Empty
  where
    splitStartableComponents _ = (empty, empty)

instance
  ( systemDef ~ 'RI.R (pair ': morePairs)
  , Forall systemDef StartComponent
  , startableComponents ~ ComponentsWithSatisfiedDeps depsLabels systemDef
  , RI.Subset startableComponents systemDef
  , Forall startableComponents RI.Unconstrained1
  , Forall startableComponents StartComponent
  , Forall (systemDef .\\ startableComponents) StartComponent ) =>
  SplitStartableComponents
    depsLabels
    ('RI.R (pair ': morePairs))
    startableComponents
  where
    splitStartableComponents systemDef =
      split @startableComponents systemDef

-- Turn a component map into a SystemStartStack

class ToSystemStartStack
    (depsLabels :: [Symbol])
    (systemDef :: Row Type)
    (systemStartStack :: [Row Type])
    | depsLabels systemDef -> systemStartStack
  where
    toSystemStartStack :: Rec systemDef -> SystemStartStack systemStartStack

instance ToSystemStartStack depsLabels Empty '[] where
  toSystemStartStack _ = EmptySystemStartStack

instance
  ( systemDef ~ 'RI.R (pair ': morePairs)
  , SplitStartableComponents depsLabels systemDef startableComponents
  -- , CheckForUnsolvableDependencies depsLabels systemDef startableComponents
  , CheckForUnsolvableDependencies
      depsLabels systemDef startableComponents
      (ComponentLabels startableComponents)
      ~ startableLabels
  , nextDepsLabels ~ (depsLabels ++ startableLabels)
  , ToSystemStartStack nextDepsLabels (systemDef .\\ startableComponents) nextSystemStartStack
  ) =>
  ToSystemStartStack
    depsLabels
    ('RI.R (pair ': morePairs))
    (startableComponents ': nextSystemStartStack)
  where
    toSystemStartStack systemDef =
      let (startable, leftover) = splitStartableComponents @depsLabels systemDef
          nextSystemStartStack = toSystemStartStack @nextDepsLabels leftover
      in startable :>> nextSystemStartStack

type family CheckForUnsolvableDependencies
    (depsLabels :: [Symbol])
    (systemDef :: Row Type)
    (startableComponents :: Row Type)
    (result :: k)
    :: k
  where
    CheckForUnsolvableDependencies depsLabels systemDef Empty result =
      TypeError ('Text "Unsolvable dependencies in system. Cannot start these components: " ':<>:
                 'ShowType systemDef ':<>:
                 'Text "even after starting these dependencies: " ':<>: 'ShowType depsLabels)
    CheckForUnsolvableDependencies depsLabels systemDef startableComponents result
      = result

-- Starting system

class StartSystem
    depsRow depsStopSystemStack
    systemDefRow
    startedSystemRow fullStopSystemStack
    | depsRow depsStopSystemStack systemDefRow -> startedSystemRow fullStopSystemStack
  where
  startSystem :: RunningSystem systemDef depsRow depsStopSystemStack -> Rec systemDefRow -> IO (RunningSystem systemDef startedSystemRow fullStopSystemStack)

instance
  {-# OVERLAPPING #-}
  StartSystem deps depsStopSystemStack Empty deps depsStopSystemStack where
  startSystem system _ = pure system

instance
  {-# OVERLAPPABLE #-}
  ( providedDeps ~ ComponentLabels depsRow
  , startableComps ~ 'RI.R (pair ': morePairs)
  , startableComps ~ ComponentsWithSatisfiedDeps providedDeps systemDefRow
  , Forall startableComps RI.Unconstrained1
  , RI.Subset startableComps systemDefRow
  , StartAll depsRow startableComps startedComps startedCompsWithMeta
  , intermediateSystem ~ (depsRow .+ startedComps)
  , leftoverComps ~ (systemDefRow .\\ startableComps)
  , StartSystem
      intermediateSystem (startedCompsWithMeta ': depsStopSystemStack)
      leftoverComps
      startedSystemRow fullStopSystemStack
  ) =>
  StartSystem
    depsRow depsStopSystemStack
    systemDefRow
    startedSystemRow fullStopSystemStack
  where
  startSystem (RSys deps depsStack) systemDef = do
    let (startable, leftover) = split @startableComps systemDef
    (startedWithMeta, started) <- startAll deps startable
    startSystem (RSys (deps .+ started) (startedWithMeta :<< depsStack)) leftover

class StopAll
    (componentsWithMeta :: Row Type)
    (componentsDef :: Row Type)
    | componentsWithMeta -> componentsDef
  where
  stopAll :: Rec componentsWithMeta -> IO (Rec componentsDef)

instance {-# OVERLAPPING #-} StopAll Empty Empty where
  stopAll _ = pure empty

instance
  {-# OVERLAPPABLE #-}
  ( KnownSymbol label
  , StopComponent comp
  , runningComps ~ 'RI.R ((label 'RI.:-> (ComponentMeta (Stopped comp), comp)) ': restOfStartedComponents)
  , componentsDef ~ 'RI.R ((label 'RI.:-> Stopped comp) ': restOfComponentDef)
  , componentsDef ~ (label .== Stopped comp .+ ('RI.R restOfComponentDef))
  , StopAll ('RI.R restOfStartedComponents) ('RI.R restOfComponentDef)
  ) =>
  StopAll
    runningComps
    componentsDef
  where
  stopAll comps = do
    let label = Label @label
    let (meta, startedC) = comps .! label
    stopped <- stopWithMeta meta startedC
    stoppedRest <- stopAll (comps .- label) :: IO (Rec ('RI.R restOfComponentDef))
    pure $ label .== stopped .+ stoppedRest

class StopStack
    (stack :: [Row Type])
    (stoppedSoFar :: Row Type)
    (systemDef :: Row Type)
    | stack stoppedSoFar -> systemDef
  where
  stopStack :: Rec stoppedSoFar -> SystemStopStack stack -> IO (Rec systemDef)

instance
  {-# OVERLAPPING #-}
  StopStack '[] stoppedSoFar stoppedSoFar where
  stopStack stoppedComps EmptySystemStopStack = pure stoppedComps

instance
  {-# OVERLAPPABLE #-}
  ( StopAll comps stoppedComps
  , StopStack moreComps (stoppedSoFar .+ stoppedComps) systemDef ) =>
  StopStack
    (comps ': moreComps)
    stoppedSoFar
    systemDef
  where
    stopStack stoppedComps (comps :<< moreComps) = do
      stopped <- stopAll comps
      stopStack (stoppedComps .+ stopped) moreComps

stopSystem :: StopStack stack Empty systemDef => RunningSystem systemDef componentsRow stack -> IO (Rec systemDef)
stopSystem (RSys _ stack) = stopStack empty stack

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

type family DependenciesNames (deps :: [(Symbol, Type -> Constraint)]) :: [Symbol] where
  DependenciesNames '[] = '[]
  DependenciesNames ('(label, constr) ': moreDeps) = AddNonDuplicate (DependenciesNames moreDeps) label

-- Helper type families

type family If (cond :: Bool) (thenT :: k) (elseT :: k) :: k where
  If 'True thenT _ = thenT
  If 'False _ elseT = elseT

infixr 5 ++
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
    '[]       ++ ys = ys
    (x ': xs) ++ ys = x ': xs ++ ys

type family Fst (pair :: (k, l)) :: k where
  Fst '(a, b) = a

type family Snd (pair :: (k, l)) :: l where
  Snd '(a, b) = b

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
