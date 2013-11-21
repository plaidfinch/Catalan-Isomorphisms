{-# LANGUAGE GADTs , DataKinds , TypeFamilies , FlexibleInstances #-}

module CountedCollections where

import Data.Foldable
import Data.Monoid
import Data.List
import Control.Applicative

-- | Natural numbers...
data Nat = Z | S Nat

infixr 5 :::

data CountedList (l :: Nat) x where
   Nil   :: CountedList Z x
   (:::) :: x -> CountedList n x -> CountedList (S n) x

instance Show x => Show (CountedList l x) where
   show = (\x -> "(" ++ x ++ " ::: Nil)") . intercalate " ::: " . map show . toList

class MaybeFromList (l :: Nat) where
   maybeFromList :: [a] -> Maybe (CountedList l a)

instance MaybeFromList Z where
   maybeFromList _ = Just Nil

instance (MaybeFromList n) => MaybeFromList (S n) where
   maybeFromList [] = Nothing
   maybeFromList (x : xs) = (x :::) <$> maybeFromList xs

instance Foldable (CountedList l) where
   foldMap _ Nil        = mempty
   foldMap f (x ::: xs) = f x `mappend` foldMap f xs

---------------------------------------------------------------------------

-- | Type family allowing us to add natural numbers at the type level
type family Plus (m :: Nat) (n :: Nat) :: Nat
type instance Plus Z n = n
type instance Plus (S m) n = S (Plus m n)

-- | Trees...
data Tree = Node [Tree] deriving ( Eq, Show )

data CountedTree (size :: Nat) where
   N :: CountedForest n -> CountedTree (S n)

infixr 5 :-:
data CountedForest (size :: Nat) where
   E :: CountedForest Z
   (:-:) :: CountedTree m -> CountedForest n -> CountedForest (Plus m n)

instance Show (CountedTree x) where
   show (N children) = "(N " ++ show children ++ ")"

instance Show (CountedForest x) where
   show E = "E"
   show (x :-: rest) = show x ++ " :-: " ++ show rest

class MaybeFromTree (size :: Nat) where
   maybeFromTree :: Tree -> Maybe (CountedTree size)

class MaybeFromForest (size :: Nat) where
   maybeFromForest :: [Tree] -> Maybe (CountedForest size)

instance MaybeFromTree (S Z) where
   maybeFromTree (Node []) = Just (N E)
   maybeFromTree _         = Nothing

instance (MaybeFromForest n) => MaybeFromTree (S (S n)) where
   maybeFromTree (Node []) = Nothing
   maybeFromTree (Node cs) = N <$> maybeFromForest cs

instance MaybeFromForest Z where
   maybeFromForest [] = Just E
   maybeFromForest _  = Nothing

instance (MaybeFromForest n) => MaybeFromForest (S n) where
   maybeFromForest [] = Nothing
   --maybeFromForest (x : xs) = (:-:) <$> maybeFromTree x <*> maybeFromForest xs
