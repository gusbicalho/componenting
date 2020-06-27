{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Componenting2.Internal.Util where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))

class Anything (t :: k) where
instance Anything t where

class (constraint1 row, constraint2 row) => And constraint1 constraint2 (row :: k) where
instance (constraint1 row, constraint2 row) => And constraint1 constraint2 row where

type family All (constraints :: [k -> Constraint]) :: k -> Constraint where
  All '[] = Anything
  All '[constraint] = constraint
  All (constraint ': moreConstraints) = And constraint
                                            (All moreConstraints)

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
    (labeledList :: [(Symbol, k)])
    :: [(Symbol, k)]
  where
    RemoveLabels _ '[] = '[]
    RemoveLabels labelsToRemove ('(label, v) ': morePairs) =
      If (ItemKnown labelsToRemove label)
        (RemoveLabels labelsToRemove morePairs)
        ('(label, v) ': RemoveLabels labelsToRemove morePairs)

type family RenameLabel
    (fromLabel :: Symbol)
    (toLabel :: Symbol)
    (labeledList :: [(Symbol, k)])
    :: [(Symbol, k)]
  where
  RenameLabel fromLabel toLabel '[] = '[]
  RenameLabel fromLabel toLabel ('(fromLabel, v) ': morePairs) =
    '(toLabel, v) ': RenameLabel fromLabel toLabel morePairs
  RenameLabel fromLabel toLabel (pair ': morePairs) =
    pair ': RenameLabel fromLabel toLabel morePairs
