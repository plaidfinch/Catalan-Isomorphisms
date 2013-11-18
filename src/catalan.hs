{-# LANGUAGE GADTs , DataKinds , TypeFamilies , FlexibleInstances , LambdaCase #-}

module Catalan where

import Prelude hiding ( (-) )
import Data.Semigroup
import Control.Applicative

-- | Natural numbers...
data Nat = Z | S Nat

-- | Type family allowing us to add natural numbers at the type level
type family Plus (m :: Nat) (n :: Nat) :: Nat
type instance Plus Z n = n
type instance Plus (S m) n = S (Plus m n)

-- | NNPath (non-negative path) are paths consisting of up (U) and down (D) segments, which start at zero and never go below zero
data NNPath (height :: Nat) where
   End :: NNPath Z
   U :: NNPath m -> NNPath (S m)
   D :: NNPath (S m) -> NNPath m

-- | NNPaths are compared for equality lazily
instance Eq (NNPath x) where
   U m == U n = m == n
   D m == D n = m == n
   U m == D n = False
   D m == U n = False
   End == End = True

-- | This operator lets you construct paths in a left-to-right manner, e.g. End-U-D-U-U-D-D
(-) :: NNPath x -> (NNPath x -> NNPath y) -> NNPath y
(-) = flip ($)

-- | The Show instance for NNPaths uses the (-) operator defined above, as it's more intuitive
instance Show (NNPath x) where
   --show End   = "End"
   --show (U n) = "(U " ++ show n ++ ")"
   --show (D n) = "(D " ++ show n ++ ")"
   show = (++ ")") . ("(End" ++) . concatMap (\case Up -> "-U"; Dn -> "-D") . unparseNNPath

-- | A Dyck path is a non-negative path ending at zero
type Dyck = NNPath Z

-- | Non-negative paths may be concatenated, resulting in another non-negative path
infixr 5 |+|
(|+|) :: NNPath n -> NNPath m -> NNPath (Plus m n)
y |+| End   = y
y |+| (U x) = U (y |+| x)
y |+| (D x) = D (y |+| x)

-- | This operation forms a monoid on Dyck paths only
instance Monoid Dyck where
   mempty  = End
   mappend = (|+|)

-- | Class to allow slicing paths at the zero crossing; used in split below
class CutZ (x :: Nat) where
   cut :: NNPath x -> (NNPath x,Dyck)
instance CutZ Z where
   cut p = (End,p)
instance CutZ n => CutZ (S n) where
   cut (U x) = let (f,r) = cut x in (U f,r)
   cut (D x) = let (f,r) = cut x in (D f,r)

-- | A Dyck path may be separated into a list of Dyck paths.
-- This operation guarantees that each Dyck path in the resulting list will be a *prime* Dyck path (i.e. no zero-crossings other than start and end), but we don't yet have a way of meaningfully encoding this in the type system so that we can take advantage of it to type-safely implement an "unbump" operation for prime Dyck paths.

-- | The following identities hold:
--
-- > foldr1 (|+|) . split === id
--
-- > split . foldr1 (|+|) === id
split :: Dyck -> [Dyck]
split End = []
split n = reverse $ let (f,r) = splitFirst n in f : split r
   where
      splitFirst End     = (End,End)
      splitFirst (D n) = let (f,r) = cut n in (D f,r)

-- Alternatively, we can define split via the tree isomorphism:
-- > split :: Dyck -> [Dyck]
-- > split = map (bump . toDyck) . reverse . children . fromDyck

-- Some utilities...

-- | Tack an element onto the end of a list
tack :: x -> [x] -> [x]
tack x [] = [x]
tack x (y : rest) = y : tack x rest

-- | Take a Dyck path and return it with an up before it and a down after it, thus bumping it upwards
bump :: Dyck -> Dyck
bump = D . (End-U |+|)

-- | Typeclass for things countable by Catalan numbers
--
-- > toDyck . fromDyck === id
--
-- > fromDyck . toDyck === id
class Catalan a where
   toDyck   :: a -> Dyck
   fromDyck :: Dyck -> a

-- | Trees with any number of child nodes can be counted by Catalan numbers
data Tree = Node [Tree] deriving ( Eq, Show )

-- | Returns the list of children for a tree
children :: Tree -> [Tree]
children (Node cs) = cs

-- | Trees are isomorphic to Dyck paths
instance Catalan Tree where
   toDyck (Node [])     = End
   toDyck (Node leaves) =
      foldr (flip (|+|) . bump . toDyck) End (reverse leaves)

   fromDyck = fromNNPath
      where
         fromNNPath :: NNPath x -> Tree
         fromNNPath End   = Node []
         fromNNPath (U n) = zipU (fromNNPath n)
         fromNNPath (D n) = zipD (fromNNPath n)

         zipU tree                  = Node [tree]
         zipD (Node [])             = undefined -- can't zip down from a leaf; this never occurs w/ Dyck paths
         zipD (Node (Node gs : cs)) = Node (tack (Node cs) gs)

-- | A direction is either up (Up) or down (Dn)
data Dir = Up | Dn deriving ( Eq, Ord, Show )

-- | Adds either 1 or -1 based on the direction given
plusDir :: Enum a => Dir -> a -> a
plusDir Up = succ
plusDir Dn = pred

-- | Determines if a list of directions constitutes a Dyck path
isDyckList :: [Dir] -> Bool
isDyckList dirs =
   (0     ==     foldr plusDir 0 dirs) &&
   (all (>= 0) $ scanr plusDir 0 (reverse dirs))

-- | We can take a purely value-level, no-nonsense list of ordinary directions and (maybe) parse it into a Dyck path -- if and only if it represents a Dyck path. This uses the same tree-zipper technique used in implementing the fromDyck method of trees
--
-- Note that (for Dyck paths ONLY -- other inputs yield bottom):
--
-- > unparseNNPath . fromJust . parseDyck === id
--
-- > fromJust . parseDyck . unparseNNPath === id
parseDyck :: [Dir] -> Maybe Dyck
parseDyck dirs =
   if not (isDyckList dirs) then Nothing
   else toDyck <$> treeFromDirs dirs
   where
      treeFromDirs = foldr (=<<) (Just $ Node []) .
         reverse . map (\case Up -> zipU; Dn -> zipD)

      zipU tree                  = Just $ Node [tree]
      zipD (Node [])             = Nothing
      zipD (Node (Node gs : cs)) = Just $ Node (tack (Node cs) gs)

-- | Takes a non-negative path and unparses it, returning just a sequence of directions
unparseNNPath :: NNPath x -> [Dir]
unparseNNPath = reverse . unparseNNPath'
   where
      unparseNNPath' :: NNPath x -> [Dir]
      unparseNNPath' End = []
      unparseNNPath' (U n) = Up : unparseNNPath' n
      unparseNNPath' (D n) = Dn : unparseNNPath' n
