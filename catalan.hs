{-# LANGUAGE
  GADTs
, TypeFamilies
, DataKinds
, PolyKinds
, FlexibleInstances
, TypeOperators #-}

module Catalan where

import Data.Foldable
import Data.Semigroup

-- | Type-level natural numbers...
data Nat = Z | S Nat

type family (m :: Nat) :+: (n :: Nat) :: Nat
type instance  Z    :+: n = n
type instance (S m) :+: n = S (m :+: n)

type family Pred (m :: Nat) :: Nat
type instance Pred (S m) = m

-- | NNPath (non-negative path) are paths consisting of up (U) and down (D) segments, which start at zero and never go below zero
data NNPath (height :: Nat) where
   E :: NNPath Z
   U :: NNPath m -> NNPath (S m)
   D :: NNPath (S m) -> NNPath m

instance Eq (NNPath x) where
   U m == U n = m == n
   D m == D n = m == n
   U m == D n = False
   D m == U n = False
   E == E     = True

instance Show (NNPath x) where
   show E = "E"
   show (U n) = "(U " ++ show n ++ ")"
   show (D n) = "(D " ++ show n ++ ")"

-- | A Dyck path is a non-negative path ending at zero
type Dyck = NNPath Z

-- | Non-negative paths may be concatenated, resulting in another non-negative path
(|+|) :: NNPath m -> NNPath n -> NNPath (m :+: n)
E     |+| y = y
(U x) |+| y = U (x |+| y)
(D x) |+| y = D (x |+| y)

-- | This operation forms a monoid on Dyck paths only
instance Monoid Dyck where
   mempty  = E
   mappend = (|+|)

-- Non-empty lists...

infixr 5 :::

data NonEmptyList a = Final a
                    | a ::: (NonEmptyList a)
                    deriving ( Eq )

instance Show a => Show (NonEmptyList a) where
   show (Final x)    = "Final " ++ show x
   show (x ::: rest) = show x ++ " ::: " ++ show rest

instance Foldable NonEmptyList where
   foldMap f (Final x)    = f x
   foldMap f (x ::: rest) = f x `mappend` foldMap f rest

instance Semigroup (NonEmptyList a) where
   Final a   <> list = a ::: list
   (a ::: b) <> list = a ::: (b <> list)

-- Trees with any number of child nodes...

data Tree = Leaf
          | Node (NonEmptyList Tree)
          deriving ( Eq )

instance Show Tree where
   show Leaf        = "Leaf"
   show (Node list) = "Node (" ++ show list ++ ")"

class Catalan a where
   toDyck   :: a -> Dyck
   fromDyck :: Dyck -> a

instance Catalan Tree where
   toDyck  Leaf         = E
   toDyck (Node leaves) =
      foldMap (D . (|+| U E) . toDyck) leaves

class MaybeD x where
   maybeD :: NNPath x -> Maybe (NNPath (Pred x))

instance MaybeD (S x) where
   maybeD = Just . D

instance MaybeD Z where
   maybeD = const Nothing
