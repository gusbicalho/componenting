{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Componenting2.Internal.Util
  ( All
  , FstT
  , SndT
  , type (++)
  , Concat
  , Reverse
  , RemoveLabels
  , AllItemsKnown
  , ItemKnown
  , If
  , MaybeConstrained (..)
  , MaybeSome (..), maybeWith
  ) where

import Data.Kind (Constraint, Type)
import Data.Row.Records (Rec, Label, (.!), type (.!))
import Data.Row.Internal (Row (..), type (.-))
import Data.Void (Void, absurd)
import GHC.TypeLits (Symbol, KnownSymbol, TypeError, ErrorMessage(..))

class Anything (t :: k) where
instance Anything t where

class (constraint1 row, constraint2 row) => And constraint1 constraint2 (row :: k) where
instance (constraint1 row, constraint2 row) => And constraint1 constraint2 row where

type family All (constraints :: [k -> Constraint]) :: k -> Constraint where
  All '[] = Anything
  All '[constraint] = constraint
  All (constraint ': moreConstraints) = And constraint
                                            (All moreConstraints)

class ToMaybe t where
  type MaybeContent t :: Type
  toMaybe :: t -> Maybe (MaybeContent t)

instance ToMaybe (Maybe t) where
  type MaybeContent (Maybe t) = t
  toMaybe = id

instance ToMaybe Void where
  type MaybeContent Void = Void
  toMaybe = absurd

data MaybeSome (constraint :: Type -> Constraint) =
  None
  | forall t. constraint t => Some t

maybeWith :: r -> (forall t. constraint t => t -> r) -> MaybeSome constraint -> r
maybeWith onNone _ None = onNone
maybeWith _ onSome (Some t) = onSome t

class
  MaybeConstrained
    (constraint :: Type -> Constraint)
    (t :: Type)
  where
  toMaybeSome :: t -> Maybe (MaybeSome constraint)
instance
  (constraint t) =>
  MaybeConstrained
    constraint
    (Maybe t)
  where
  toMaybeSome Nothing = Nothing
  toMaybeSome (Just t) = Just (Some t)
instance
  MaybeConstrained
    constraint
    ()
  where
  toMaybeSome () = Nothing

type family If (cond :: Bool) (thenT :: k) (elseT :: k) :: k where
  If 'True thenT _ = thenT
  If 'False _ elseT = elseT

infixr 5 ++
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
    '[]       ++ ys = ys
    (x ': xs) ++ ys = x ': xs ++ ys

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

type family IsVoid (t :: Type) :: Bool where
  IsVoid Void = 'True
  IsVoid _ = 'False

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

type family ListLabels (labeledList :: [(Symbol, k)]) :: [Symbol] where
  ListLabels '[] = '[]
  ListLabels ('(label, v) ': morePairs) = AddNonDuplicate (ListLabels morePairs) label

type family RemoveLabels
    (labelsToRemove :: [Symbol])
    (row :: Row k)
    :: Row k
  where
    RemoveLabels '[] row = row
    RemoveLabels (label ': moreLabels) row =
      RemoveLabels moreLabels (row .- label)
