{-

Name: Christian Ling
Time spent on assignment: 1 hour
Collaborators/Acknowledgements: None

-}

{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}

module SizeBalancedBinaryTreeArray where

{- ************************************************************ -}
{- ************************************************************ -}

-- Array interface
class Array arr where
  new  :: Int -> a -> arr a    
  size :: arr a -> Int
  idx  :: arr a -> Int -> Maybe a
  upd  :: arr a -> Int -> a -> arr a

{- ************************************************************ -}
{- ************************************************************ -}

data SBBTree a = Leaf
               | Node Int (SBBTree a) a (SBBTree a)
  deriving (Read, Show, Eq)
-- Informal invariant:: A "SBBTree a" is a 'size-balanced' binary tree.
-- Invariant:: forall (Node s l x r). abs (size l - size r) <= 1
-- Invariant:: forall (Node s l x r). s == 1 + size l + size r


valid :: SBBTree a -> Bool
valid Leaf           = True
valid (Node s l _ r) =
  valid l && valid r && abs (sl - sr) <= 1 && s == 1 + sl + sr
  where sl = size l
        sr = size r


instance Array SBBTree where
  -- new :: Int -> a -> SBBTree a
  new 0 _ = Leaf
  new i a = Node i (new (i-1) a) a (new (i-1) a)

  -- size :: SBBTree -> Int
  size Leaf            = 0
  size (Node s _ _ _)  = s

  -- idx :: SBBTree a -> Int -> Maybe a
  idx Leaf              _  = Nothing
  idx (Node i1 lt a1 _) i2 =
    if i1 == i2
      then Just a1 
      else idx lt i2

  -- upd :: SBBTree a -> Int -> a -> SBBTree a
  upd Leaf               _  _  = Leaf
  upd (Node i1 lt a1 rt) i2 a2 = 
    if i1 == i2
      then (Node i1 lt a2 rt)
      else (Node i1 (upd lt i2 a2) a1 (upd rt i2 a2)) 

instance Functor SBBTree where
  -- fmap :: (a -> b) -> SBBTree a -> SBBTree b
  fmap _ Leaf             = Leaf
  fmap f (Node i lt a rt) = (Node i (fmap f lt) (f a) (fmap f rt)) 

instance Foldable SBBTree where
  -- foldr :: (a -> b -> b) -> b -> SBBTree a -> b
  foldr _ b Leaf = b
  foldr f b (Node _ lt a rt) = foldr f b' rt
    where b'  = f a b''
          b'' = foldr f b lt
  -- foldl :: (b -> a -> b) -> b -> SBBTree a -> b
  foldl _ b Leaf             = b
  foldl f b (Node _ lt a rt) = foldl f b' lt
    where b'  = f b'' a
          b'' = foldl f b rt

{-

Explain why the direct implementation(s) of the additional method(s)
of the `Foldable` type class will be more efficient than the default
implementation(s).

-----

Since Haskell uses lazy evaluation, the foldr and foldl will prevent stackoverflows from occurring for large SBB trees. 

-}
