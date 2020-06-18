{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Componenting.Internal.System.Validation where

import Data.Kind
import GHC.TypeLits
import Componenting.Internal.System.Types

type family AllComponentsLabeled t :: Constraint where
  AllComponentsLabeled (a :>> b) = (AllComponentsLabeled a, AllComponentsLabeled b)
  AllComponentsLabeled (_ :-> _) = ()
  AllComponentsLabeled c =
    TypeError ('Text "All components in a System must be labeled, but this is not: " ':<>:
               'ShowType c)

type family NoDuplicateLabels t :: Constraint where
  NoDuplicateLabels t = NoDuplicateLabelsGo '[] (CollectLabels t)

-- TODO get this from some lib
type family If (cond :: Bool) (thenT :: k) (elseT :: k) :: k where
  If 'True thenT _ = thenT
  If 'False _ elseT = elseT

-- TODO use some type-level set to avoid n^2
infixr 5 ++
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
    '[]       ++ ys = ys
    (x ': xs) ++ ys = x ': xs ++ ys

type family CollectLabels t :: [Symbol] where
  CollectLabels (label :-> _) = '[label]
  CollectLabels (a :>> b) = CollectLabels a ++ CollectLabels b
  CollectLabels _ = '[]

type family LabelKnown (knownLabels :: [Symbol]) (label :: Symbol) where
  LabelKnown '[] _ = 'False
  LabelKnown (l ': ls) l = 'True
  LabelKnown (_ ': ls) l = LabelKnown ls l

type family NoDuplicateLabelsGo (knownLabels :: [Symbol]) (moreLabels :: [Symbol]) :: Constraint where
  NoDuplicateLabelsGo knownLs '[] = ()
  NoDuplicateLabelsGo knownLs (l ': ls) =
    If (LabelKnown knownLs l)
      (TypeError ('Text "Duplicate label found: " ':<>: 'ShowType l))
      (NoDuplicateLabelsGo (l : knownLs) ls)
