{-# LANGUAGE GADTs , TypeFamilies , FlexibleInstances , TypeOperators #-}

module Catalan where

import Data.Semigroup
import Data.Foldable

-- | Natural numbers...
data Nat = Z | S Nat

type family (m :: Nat) :+: (n :: Nat) :: Nat
type instance Z   :+: n = n
type instance S m :+: n = S (m :+: n)

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

-- Some utilities...

tack :: x -> [x] -> [x]
tack x [] = [x]
tack x (y : rest) = y : tack x rest

bump :: Dyck -> Dyck
bump = D . (|+| U E)

-- | Typeclass for things countable by Catalan numbers
-- | toDyck . fromDyck === id
-- | fromDyck . toDyck === id
class Catalan a where
   toDyck   :: a -> Dyck
   fromDyck :: Dyck -> a

-- | Trees with any number of child nodes can be counted by Catalan numbers
data Tree = N [Tree] deriving ( Eq, Show )

instance Catalan Tree where
   toDyck (N [])     = E
   toDyck (N leaves) =
      foldMap (bump . toDyck) (reverse leaves)
   fromDyck = fromNNPath
      where
         fromNNPath :: NNPath x -> Tree
         fromNNPath E = N []
         fromNNPath (U n) = zipU (fromNNPath n)
         fromNNPath (D n) = zipD (fromNNPath n)

         zipU = N . (:[])

         zipD (N []) = undefined -- can't zip down from a leaf; b/c we're using Dyck paths, this never occurs
         zipD (N ((N gs) : cs)) = N (tack (N cs) gs)

class MaybeD x where
   maybeD :: NNPath x -> Maybe (NNPath (Pred x))
instance MaybeD (S x) where
   maybeD = Just . D
instance MaybeD Z where
   maybeD = const Nothing

class EnsureDyck x where
   ensureDyck :: NNPath x -> Maybe Dyck
instance EnsureDyck Z where
   ensureDyck = Just
instance EnsureDyck (S n) where
   ensureDyck = const Nothing

data Dir = Up | Down

pathToDyck :: [Dir] -> Maybe Dyck
pathToDyck = undefined

class CutZ x where
   cut :: NNPath x -> (NNPath x,Dyck)
instance CutZ Z where
   cut p = (E,p)
instance CutZ n => CutZ (S n) where
   cut (U x) = let (f,r) = cut x in (U f,r)
   cut (D x) = let (f,r) = cut x in (D f,r)

-- | A Dyck path may be separated into a list of Dyck paths
-- | The following identities hold:
-- | foldr1 (|+|) . split === id
-- | split . foldr1 (|+|) === id
split :: Dyck -> [Dyck]
split E = []
split n = let (f,r) = splitFirst n in f : split r
   where
      splitFirst E     = (E,E)
      splitFirst (D n) = let (f,r) = cut n in (D f,r)


