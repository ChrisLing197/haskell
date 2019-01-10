{-

Name:
Time spent on assignment:
Collaborators/Acknowledgements:

-}

{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}

module BinaryRandomAccessList where

{- ************************************************************ -}
{- ************************************************************ -}

-- Stack interface
class Stack stack where
  push :: a -> stack a -> stack a
  pop :: stack a -> Maybe (a, stack a)

-- Array interface
class Array arr where
  new  :: Int -> a -> arr a    
  size :: arr a -> Int
  idx  :: arr a -> Int -> Maybe a
  upd  :: arr a -> Int -> a -> arr a

{- ************************************************************ -}
{- ************************************************************ -}

data BRAList a = Nil | ConsOne a (BRAList (a,a)) | ConsZero (BRAList (a,a))
  deriving (Read, Show, Eq)

bralistToString :: (a -> String) -> BRAList a -> String
bralistToString aToString xs =
  case xs of
    Nil           -> ""
    ConsOne  y ys -> aToString y ++ ',' ?: bralistToString pairToString ys
    ConsZero   ys -> bralistToString pairToString ys
  where pairToString (z0,z1) = aToString z0 ++ ',' : aToString z1
        _ ?: [] = []
        z ?: zs = z:zs

instance Stack BRAList where
  -- push :: a -> BRAList a -> BRAList a
  push = undefined

  -- pop :: BRAList a -> Maybe (a, BRAList a)
  pop = undefined


instance Array BRAList where
  -- new :: Int -> a -> BRAList a
  new = undefined

  -- size :: BRAList a -> Int
  size = undefined

  -- idx :: BRAList a -> Int -> Maybe a
  idx = undefined

  -- upd :: BRAList a -> Int -> a -> BRAList a
  upd = undefined


instance Functor BRAList where
  -- fmap :: (a -> b) -> BRAList a -> BRAList b
  fmap = undefined


instance Foldable BRAList where
  -- foldr :: (a -> b -> b) -> b -> BRAList a -> b
  foldr = undefined

  -- foldl :: (b -> a -> b) -> b -> BRAList a -> b
  foldl = undefined


{-

Explain why the direct implementation(s) of the additional method(s)
of the `Foldable` type class will be more efficient than the default
implementation(s).

-----

<<Your answer here.>>

-}
