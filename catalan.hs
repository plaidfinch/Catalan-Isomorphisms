{-# LANGUAGE GADTs , TypeFamilies , FlexibleInstances , TypeOperators , MultiParamTypeClasses #-}

module Catalan where

import Prelude hiding ( all , foldl )
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

-- | Class to allow slicing paths at the zero crossing; used in split below
class CutZ x where
   cut :: NNPath x -> (NNPath x,Dyck)
instance CutZ Z where
   cut p = (E,p)
instance CutZ n => CutZ (S n) where
   cut (U x) = let (f,r) = cut x in (U f,r)
   cut (D x) = let (f,r) = cut x in (D f,r)

-- | A Dyck path may be separated into a list of Dyck paths.
-- | This operation guarantees that each Dyck path in the resulting list will be a *prime* Dyck path (i.e. no zero-crossings other than start and end), but we don't yet have a way of meaningfully encoding this in the type system so that we can take advantage of it to type-safely implement an "unbump" operation for prime Dyck paths.
-- | The following identities hold:
-- | foldr1 (|+|) . split === id
-- | split . foldr1 (|+|) === id
split :: Dyck -> [Dyck]
split E = []
split n = let (f,r) = splitFirst n in f : split r
   where
      splitFirst E     = (E,E)
      splitFirst (D n) = let (f,r) = cut n in (D f,r)

-- Alternatively, we can define split via the tree isomorphism:
-- split :: Dyck -> [Dyck]
-- split = map (bump . toDyck) . reverse . children . fromDyck

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
data Tree = Node [Tree] deriving ( Eq, Show )

children :: Tree -> [Tree]
children (Node cs) = cs

instance Catalan Tree where
   toDyck (Node [])     = E
   toDyck (Node leaves) =
      foldMap (bump . toDyck) (reverse leaves)

   fromDyck = fromNNPath
      where
         fromNNPath :: NNPath x -> Tree
         fromNNPath E     = Node []
         fromNNPath (U n) = zipU (fromNNPath n)
         fromNNPath (D n) = zipD (fromNNPath n)

         zipU = Node . (: [])

         zipD (Node []) = undefined -- can't zip down from a leaf; b/c we're using Dyck paths, this never occurs
         zipD (Node (Node gs : cs)) = Node (tack (Node cs) gs)

-- | A direction is either up (Up) or down (Dn)
data Dir = Up | Dn deriving ( Eq, Ord, Show )

-- | Adds either 1 or -1 based on the direction given
plusDir :: Enum a => a -> Dir -> a
plusDir n Up = succ n
plusDir n Dn = pred n

-- | Determines if a list of directions constitutes a Dyck path
isDyckList :: [Dir] -> Bool
isDyckList dirs =
   (0     ==     foldl plusDir 0 rdirs) &&
   (all (>= 0) $ scanl plusDir 0 rdirs)
   where rdirs = reverse dirs

-- | We can take a purely value-level, no-nonsense list of ordinary directions and (maybe) parse it into a Dyck path -- if and only if it represents a Dyck path. This uses the same tree-zipper technique used in implementing the fromDyck method of trees
parseDyck :: [Dir] -> Maybe Dyck
parseDyck dirs =
   if isDyckList dirs
   then case treeFromDir dirs of
      Nothing   -> Nothing
      Just tree -> Just (toDyck tree)
   else Nothing
   where
      treeFromDir []          = Just (Node [])
      treeFromDir (Up : rest) = zipU =<< (treeFromDir rest)
      treeFromDir (Dn : rest) = zipD =<< (treeFromDir rest)

      zipU = Just . Node . (: [])

      zipD (Node [])             = Nothing
      zipD (Node (Node gs : cs)) = Just $ Node (tack (Node cs) gs)

-- | Takes a non-negative path and "unparses" it, returning just a sequence of directions
unparseNNPath :: NNPath x -> [Dir]
unparseNNPath E = []
unparseNNPath (U n) = Up : unparseNNPath n
unparseNNPath (D n) = Dn : unparseNNPath n

-- | We can safely try to prepend a D to an NNPath, returning nothing or a new NNPath
class MaybeD x where
   maybeD :: NNPath x -> Maybe (NNPath (Pred x))
instance MaybeD (S x) where
   maybeD = Just . D
instance MaybeD Z where
   maybeD = const Nothing

-- | By calling ensureHeight on an NNPath, it is possible to assert that a path has a particular height
class EnsureHeight (x :: Nat) (h :: Nat) where
   ensureHeight :: NNPath x -> Maybe (NNPath h)
instance EnsureHeight Z Z where
   ensureHeight = Just
instance EnsureHeight n n => EnsureHeight (S n) (S n) where
   ensureHeight = Just
instance EnsureHeight n n => EnsureHeight n (S n) where
   ensureHeight = const Nothing
instance EnsureHeight n n => EnsureHeight (S n) n where
   ensureHeight = const Nothing
