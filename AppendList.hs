{-

Name: Christian Ling
Time spent on assignment: 5.5 hours
Collaborators/Acknowledgements: counting time needed for procrastination

-}

{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall -Wno-unused-imports #-}

module AppendList where

-- Import the Data.List.NonEmpty module;
-- use types from the module like `Data.List.NonEmpty.NonEmpty`.
import qualified Data.List.NonEmpty
-- Also import the Data.List.NonEmpty module under the name LNE;
-- use functions from the module like `LNE.length`.
import qualified Data.List.NonEmpty as LNE

{- ************************************************************ -}
{- ************************************************************ -}

data AListNonEmpty a = NEList (Data.List.NonEmpty.NonEmpty a)
                     | Append (AListNonEmpty a) (AListNonEmpty a)
  deriving (Show, Read, Eq)
data AList a = Empty | NonEmpty (AListNonEmpty a)
  deriving (Show, Read, Eq)


alistFromList :: [a] -> AList a
alistFromList a = 
  case c of
    Nothing -> Empty
    Just d  -> NonEmpty (NEList d)
  where c = LNE.nonEmpty a


alistEmpty :: AList a
alistEmpty = Empty

alistAppend :: AList a -> AList a -> AList a
alistAppend a b= case (a,b) of
  (Empty,NonEmpty b)      -> NonEmpty b
  (NonEmpty a,Empty)      -> NonEmpty a
  (NonEmpty a,NonEmpty b) -> NonEmpty (Append a b)
  
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 800
instance Monoid (AList a) where
  mempty = alistEmpty
  mappend = alistAppend
#elif defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 800
instance Semigroup (AList a) where
  (<>) = alistAppend
instance Monoid (AList a) where
  mempty = alistEmpty
#endif


alistCons :: a -> AList a -> AList a
alistCons a b = 
  case c of
    Just d  -> alistAppend (NonEmpty (NEList d)) b
    Nothing -> b
  where c = LNE.nonEmpty [a]


alistUncons :: AList a -> Maybe (a, AList a)
alistUncons Empty                 = Nothing
alistUncons (NonEmpty (NEList a)) = Just (head b, alistFromList (tail b))
  where b=LNE.toList a


alistSnoc :: AList a -> a -> AList a
alistSnoc a b =
  case c of
    Just d  -> alistAppend a (NonEmpty NEList d))
    Nothing -> a
  where c = LNE.nonEmpty [b]


alistUnsnoc :: AList a -> Maybe (AList a, a)
alistUnsnoc Empty                 = Nothing
alistunsnoc (NonEmpty (NEList a)) = Just (alistFromList (tail b),head b)
  where b = LNE.toList a


alistMap :: (a -> b) -> AList a -> AList b
alistMap f Empty = Empty
alistMap f a     =
  case a of
    Empty               -> Empty
    NonEmpty (NEList b) -> case c of
                             Nothing -> Empty
                             Just d  -> NonEmpty (NEList d)
                           where c = LNE.nonEmpty (map f (LNE.toList a))

instance Functor AList where
  fmap = alistMap


alistFilter :: (a -> Bool) -> AList a -> AList a
alistFilter _ Empty                 = Empty
alistFilter f (NonEmpty (NEList a)) = undefined


alistFoldr :: (a -> b -> b) -> b -> AList a -> b
alistFoldr _ b Empty                 = b
alistFoldr f b (NonEmpty (NEList a)) = f (head (LNE.toList a)) b

alistFoldl :: (b -> a -> b) -> b -> AList a -> b
alistFoldl _ b Empty                 = b
alistFoldl f b (NonEmpty (NEList a)) = f b (last (LNE.toList a))

instance Foldable AList where
  foldr = alistFoldr
  foldl = alistFoldl


{-

Explain why the definition of `AList a` given above is better than
this seemingly simpler definition:

data AList a = Empty | List [a] | Append (AList a) (AList a)

-----

Declaring the AList as NonEmpty is better for enforcing types so removing from a list with 1 element does not allow you to declare NonEmpty [].

-}
