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

class ComponentLifecycle
  (stoppedC :: Type)
  where
  type Started stoppedC :: Type
  type ComponentMeta stoppedC :: Type
  type DependenciesSpec stoppedC :: [(Symbol, Type -> Constraint)]
  startWithMeta :: Dependencies (DependenciesSpec stoppedC) row
                => Rec row -> stoppedC -> IO (ComponentMeta stoppedC, Started stoppedC)
  stopWithMeta :: ComponentMeta stoppedC -> Started stoppedC -> IO stoppedC

-- test comps

data Msg = Msg String
data StartedMsg = StartedMsg String deriving (Show)
instance ComponentLifecycle Msg
  where
  type Started Msg = StartedMsg
  type ComponentMeta Msg = ()
  type DependenciesSpec Msg = '[]
  startWithMeta _ (Msg s) = do
    putStrLn $ "Msg> start " <> s
    pure ((), StartedMsg s)
  stopWithMeta _ (StartedMsg s) = pure (Msg s)

data Foo = Foo
data StartedFoo = StartedFoo deriving (Show)
instance ComponentLifecycle Foo
  where
  type Started Foo = StartedFoo
  type ComponentMeta Foo = ()
  type DependenciesSpec Foo = '[ '("show", Show) ]
  startWithMeta deps Foo = do
    putStrLn "Foo> start"
    putStrLn $ "Foo> show: " <> show (deps .! #show)
    pure ((), StartedFoo)
  stopWithMeta () _ = pure Foo

data Bar = Bar
data StartedBar = StartedBar
instance ComponentLifecycle Bar
  where
  type Started Bar = StartedBar
  type ComponentMeta Bar = ()
  type DependenciesSpec Bar = '[ '("show", Show), '("foo", Show) ]
  startWithMeta deps Bar = do
    putStrLn "Bar> start"
    putStrLn $ "Bar> show: " <> show (deps .! #show)
    putStrLn $ "Bar> foo: " <> show (deps .! #show)
    pure ((), StartedBar)
  stopWithMeta _ StartedBar = pure Bar

sys1 :: Rec ("bar" .== Bar
          .+ "foo" .== Foo
          .+ "show" .== Msg)
sys1 = #show .== Msg "lol"
    .+ #foo .== Foo
    .+ #bar .== Bar

startedSys1 :: IO (RunningSystem
                      ("bar" .== StartedBar
                    .+ "foo" .== StartedFoo
                    .+ "show" .== StartedMsg)
                     '[ ("bar" .== ((), StartedBar))
                      , ("foo" .== ((), StartedFoo))
                      , ("show" .== ((), StartedMsg)) ]
                  )
startedSys1 = startSystem noDeps sys1

-- Starting system

data RunningSystem (componentsRow :: Row Type) (stopStack :: [Row Type])
  = RSys { getComponents ::  Rec componentsRow
         , getStopStack :: StopStack stopStack
         }

noDeps :: RunningSystem Empty '[]
noDeps = RSys empty EmptyStopStack

data StopStack (stack :: [Row Type]) where
  EmptyStopStack :: StopStack '[]
  (:<<) :: Rec row -> StopStack rows -> StopStack (row ': rows)

class StartAll
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
  , ComponentLifecycle comp
  , Dependencies (DependenciesSpec comp) depsRow
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

class StartSystem
    depsRow depsStopStack
    systemDefRow
    startedSystemRow fullStopStack
    | depsRow depsStopStack systemDefRow -> startedSystemRow fullStopStack
  where
  startSystem :: RunningSystem depsRow depsStopStack -> Rec systemDefRow -> IO (RunningSystem startedSystemRow fullStopStack)

instance
  {-# OVERLAPPING #-}
  StartSystem deps depsStopStack Empty deps depsStopStack where
  startSystem system _ = pure system

instance
  {-# OVERLAPPABLE #-}
  ( providedDeps ~ ComponentLabels depsRow
  , startableComps ~ ComponentsWithSatisfiedDeps providedDeps systemDefRow
  , Forall startableComps RI.Unconstrained1
  , RI.Subset startableComps systemDefRow
  , StartAll depsRow startableComps startedComps startedCompsWithMeta
  , intermediateSystem ~ (depsRow .+ startedComps)
  , leftoverComps ~ (systemDefRow .\\ startableComps)
  , StartSystem
      intermediateSystem (startedCompsWithMeta ': depsStopStack)
      leftoverComps
      startedSystemRow fullStopStack
  ) =>
  StartSystem
    depsRow depsStopStack
    systemDefRow
    startedSystemRow fullStopStack
  where
  startSystem (RSys deps depsStack) systemDef = do
    let (startable, leftover) = split @startableComps systemDef
    (startedWithMeta, started) <- startAll deps startable
    startSystem (RSys (deps .+ started) (startedWithMeta :<< depsStack)) leftover

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
