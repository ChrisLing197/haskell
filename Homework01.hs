{-

Name: Christian Ling
Time spent on assignment: 1 week. completed 9/11 at 11:50 PM.
Collaborators/Acknowledgements: gave up on last test case

-}

{-# OPTIONS -Wall -Wno-type-defaults #-}

module Homework01 where

import Test.HUnit
import Data.List ((\\))

{- ************************************************************ -}
{- ************************************************************ -}

{- List Functions -}

-- Haskell lists are "linked lists"
--   either  "nil/empty"  ( [] )
--   or "cons/non-empty"  ( x:xs )

listIsEmpty :: [a] -> Bool
listIsEmpty []    = True
listIsEmpty (_:_) = False
-- equivalent to 'null'

listLength :: [a] -> Integer
listLength []     = 0
listLength (_:xs) = 1 + listLength xs
-- equivalent to "length"

-- listSum :: [Integer] -> Integer
listSum :: Num a => [a] -> a
listSum []     = 0
listSum (n:ns) = n + listSum ns
-- equivalent to "sum"

-- listProd :: [Integer] -> Integer
listProd :: Num a => [a] -> a
listProd []     = 1
listProd (n:ns) = n * listProd ns
-- equivalent to "product"

listTake :: Integer -> [a] -> [a]
listTake 0 _      = []
listTake _ []     = []
listTake n (x:xs) = x : listTake (n-1) xs
-- equivalent to "take"

listDrop :: Integer -> [a] -> [a]
listDrop 0 xs     = xs
listDrop _ []     = []
listDrop n (_:xs) = listDrop (n-1) xs
-- equivalent to "drop"

-- listZip [1,2,3] ['A','B','C'] ~~> [(1,'A'),(2,'B'),(3,'C')]
listZip :: ([a],[b]) -> [(a,b)]
listZip (_,     []) = []
listZip ([],    _)  = []
listZip (x:xs,y:ys) = (x,y) : listZip (xs, ys)
-- equivalent to "zip"

listUnzip :: [(a,b)] -> ([a],[b])
listUnzip []          = ([],[])
listUnzip ((x,y):xys) = (x:xs,y:ys)
                        where (xs,ys) = listUnzip xys
-- equivalent to "unzip"

listAppend :: [a] -> [a] -> [a]
listAppend []     ys = ys
listAppend (x:xs) ys = x : listAppend xs ys
-- equivalent to "++"

listReverse :: [a] -> [a]
listReverse xs = listRevApp xs []
  where listRevApp :: [a] -> [a] -> [a]
        listRevApp []     zs = zs
        listRevApp (y:ys) zs = listRevApp ys (y:zs)
-- equivalent to "reverse"

{- ************************************************************ -}

listSwizzle :: [a] -> [a] -> [a]
listSwizzle a [] = a
listSwizzle [] b = b
listSwizzle (a:as) (y:ys) = a:y:listSwizzle as ys

listSwizzleTests :: Test
listSwizzleTests =
  TestList [ listSwizzle [1, 2, 3] [4, 5, 6] ~?= [1,4,2,5,3,6],
             listSwizzle [1, 2, 3, 4] [5, 6] ~?= [1,5,2,6,3,4] ]


listHasElem :: Eq a => [a] -> a -> Bool
listHasElem [] _ = False
listHasElem (a:as) b = if (a == b) then True else listHasElem as b

listHasElemTests :: Test
listHasElemTests =
  TestList [ listHasElem [1, 2, 3] 2 ~?= True,
             listHasElem [1, 2, 3] 4 ~?= False,
             listHasElem ['A', 'B', 'C'] 'B' ~?= True,
             listHasElem ['A', 'B', 'C'] 'D' ~?= False ]


listHasDuplicates :: Eq a => [a] -> Bool
listHasDuplicates [] = False
listHasDuplicates (a:as) = if (listHasElem as a) then True else listHasDuplicates as

listHasDuplicatesTests :: Test
listHasDuplicatesTests =
  TestList [ listHasDuplicates [1, 2, 3] ~?= False,
             listHasDuplicates [1, 2, 1] ~?= True,
             listHasDuplicates ['A', 'B', 'C'] ~?= False,
             listHasDuplicates ['A', 'B', 'A'] ~?= True ]

listRemoveElement :: Eq a => a -> [a] -> [a]
listRemoveElement _ [] = []
listRemoveElement a (b:bs) = if (a == b) then bs else b:(listRemoveElement a bs)

listDiff :: Eq a => [a] -> [a] -> [a]
listDiff [] _ = []
listDiff (a:as) b = if (listHasElem b a) then listDiff as (listRemoveElement a b) else a:(listDiff as b)

listDiffTests :: Test
listDiffTests =
  TestList [ listDiff [1, 2, 3] [3, 2, 1] ~?= [],
             listDiff [1, 2, 3] [3, 4, 5] ~?= [1, 2],
             listDiff [1, 1, 2, 2, 3, 3] [3, 2, 1] ~?= [1, 2, 3],
             listDiff ['A', 'B', 'C'] ['B', 'B'] ~?= ['A', 'C'],
             listDiff ['A', 'A', 'B', 'B', 'C', 'C'] ['B', 'B'] ~?= ['A', 'A', 'C', 'C'] ]

listCheckElement:: Eq a => [a] -> [a] -> Bool
listCheckElement _ [] = True
listCheckElement [] _ = False
listCheckElement (a:as) (b:bs) = if (a == b) then listCheckElement as bs else False

listHasSublist :: Eq a => [a] -> [a] -> Bool
listHasSublist _ [] = True
listHasSublist [] _ = False
listHasSublist (a:as) (b:bs) = if (a == b) then (listCheckElement (a:as) (b:bs)) else listHasSublist (as) (b:bs)

listHasSublistTests :: Test
listHasSublistTests =
  TestList [ listHasSublist [1, 2, 3, 4, 5] [2, 3] ~?= True,
             listHasSublist [1, 2, 3, 4, 5] [2, 4] ~?= False,
             listHasSublist [1, 2, 3] [1, 2, 3] ~?= True,
             listHasSublist [1, 2, 3] [] ~?= True,
             listHasSublist ['A', 'B', 'C', 'D', 'E'] ['B', 'C'] ~?= True,
             listHasSublist ['A', 'B', 'C', 'D', 'E'] ['B', 'D'] ~?= False,
             listHasSublist ['A', 'B', 'C'] ['A', 'B', 'C'] ~?= True,
             listHasSublist ['A', 'B', 'C'] [] ~?= True ]

listRotate :: Integer -> [a] -> [ [a] ]
listRotate 0 a = []
listRotate n a = if n > 0 then ((listDrop n a)++(listTake n a)):(listRotate (n-1) a) else []

listRotations :: [a] -> [[a]]
listRotations [] = [[]]
listRotations a = listRotate (listLength a) a

listRotationsTests :: Test
listRotationsTests =
  TestList [ cmp (listRotations [1, 2, 3]) [[1,2,3],[2,3,1],[3,1,2]] ~?= True,
             cmp (listRotations []) ([[]] :: [[Bool]]) ~?= True,
             cmp (listRotations ['A', 'B', 'C']) ["ABC","BCA","CAB"] ~?= True ]
  where cmp xss yss = null (xss \\ yss) && null (yss \\ xss)
  -- Use helper 'cmp' to compare, because order doesn't matter

listSubs :: [a] -> [[a]]
listSubs [] = [[]]
listSubs (a:as) = if (listLength (a:as)) == 0 then [[]] else ([[a]]++(listSubs as))++[listTake ((listLength (a:as))) (a:as)]

listRaise :: [a] -> Integer -> [[a]]
listRaise [] _ = []
listRaise (a:as) n = if n==0 then [(a:as)] else [(listTake n (a:as))]++listRaise as (n-1)

listSublists :: Eq a => [a] -> [[a]]
listSublists [] = [[]]
listSublists a = (listRemoveDuplicates ((listSubs a)++(listRaise a ((listLength a)-1))))

listRemoveDuplicates :: Eq a => [[a]] -> [[a]]
listRemoveDuplicates [] = []
listRemoveDuplicates [[a]] = [[a]]
listRemoveDuplicates ([]:a)=[[]]++(listRemoveDuplicates a)
listRemoveDuplicates ((a):b) = if (listHasElem b a) then (listRemoveDuplicates b) else [a]++(listRemoveDuplicates b)


listSublistsTests :: Test
listSublistsTests =
  TestList [ cmp (listSublists [1, 2, 3]) [[],[1],[2],[3],[1,2],[2,3],[1,2,3]] ~?= True,
             cmp (listSublists []) ([[]] :: [[Bool]]) ~?= True,
             cmp (listSublists ['A', 'B', 'C']) ["","A","B","C","AB","BC","ABC"] ~?= True ]
  where cmp xss yss = null (xss \\ yss) && null (yss \\ xss)
  -- Use helper 'cmp' to compare, because order doesn't matter
listFold :: [a] -> [a] -> [[a]]
listFold _ [] = []
listFold [] _ = []
listFold (a:as) (b:bs) = [a,b]:(listFold as bs)

listTranspose :: [[a]] -> [[a]]
listTranspose [] = []
listTranspose ([]:_) = []
listTranspose [(_:_)]=[]
listTranspose ((_:_):[]:_)=[]
listTranspose ((a:as):((ab:abs):aby)) = if ((listLength aby) == 0) then listFold (a:as) (ab:abs) else []

listTransposeTests :: Test
listTransposeTests =
  TestList [ listTranspose [[1, 2, 3], [4, 5, 6]] ~?= [[1, 4], [2, 5], [3, 6]],
             listTranspose [[1, 2], [3, 4, 5]] ~?= [[1, 3], [2, 4]],
             listTranspose [[1, 2, 3], [4, 5]] ~?= [[1, 4], [2, 5]],
             listTranspose [[1, 2], [], [4, 5]] ~?= ([] :: [[Integer]]),
             listTranspose [[]] ~?= ([] :: [[Integer]]),
             listTranspose [] ~?= ([] :: [[Integer]]) ]


{- ************************************************************ -}
{- ************************************************************ -}

{- Tree Functions -}

-- The following defines a type of binary trees:
--   either a leaf (Leaf)
--   or a node with a left sub-tree, an element, and a right sub-tree  (Node tl x rt)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

treeSize :: Tree a -> Integer
treeSize Leaf           = 0
treeSize (Node lt _ rt) = 1 + treeSize lt + treeSize rt

treeHeight :: Tree a -> Integer
treeHeight Leaf           = 0
treeHeight (Node lt _ rt) = 1 + max (treeHeight lt) (treeHeight rt)

treeFlip :: Tree a -> Tree a
treeFlip Leaf         = Leaf
treeFlip (Node l x r) = Node (treeFlip r) x (treeFlip l)

{- ************************************************************ -}

treeSum :: Num a => Tree a -> a
treeSum Leaf = 0
treeSum (Node l c r) = ((treeSum l + c) +treeSum r)

treeSumTests :: Test
treeSumTests =
  TestList [ treeSum (Node (Node Leaf 7 Leaf) 8 (Node Leaf 9 Leaf)) ~?= 24,
             treeSum Leaf ~?= 0,
             treeSum (Node (Node Leaf 7.5 Leaf) 8.5 (Node Leaf 9.5 Leaf)) ~?= 25.5 ]


treeProduct :: Num a => Tree a -> a
treeProduct Leaf = 1
treeProduct (Node l c r) = ((treeProduct l * c) * treeProduct r)

treeProductTests :: Test
treeProductTests =
  TestList [ treeProduct (Node (Node Leaf 7 Leaf) 8 (Node Leaf 9 Leaf)) ~?= 504,
             treeProduct Leaf ~?= 1,
             treeProduct (Node (Node Leaf 7.5 Leaf) 8.5 (Node Leaf 9.5 Leaf)) ~?= 605.625 ]


treeHasElem :: Eq a => Tree a -> a -> Bool
treeHasElem Leaf _ = False
treeHasElem (Node l c r) target = if (c == target) then True else (treeHasElem l target) || (treeHasElem r target)

treeHasElemTests :: Test
treeHasElemTests =
  TestList [ treeHasElem (Node (Node Leaf 7 Leaf) 8 (Node Leaf 9 Leaf)) 8 ~?= True,
             treeHasElem (Node (Node Leaf 7 Leaf) 8 (Node Leaf 9 Leaf)) 9 ~?= True,
             treeHasElem (Node (Node Leaf 7 Leaf) 8 (Node Leaf 9 Leaf)) 6 ~?= False,
             treeHasElem (Node (Node Leaf 'A' Leaf) 'B' (Node Leaf 'C' Leaf)) 'B' ~?= True,
             treeHasElem (Node (Node Leaf 'A' Leaf) 'B' (Node Leaf 'C' Leaf)) 'A' ~?= True,
             treeHasElem (Node (Node Leaf 'A' Leaf) 'B' (Node Leaf 'C' Leaf)) 'Z' ~?= False ]


treeHasSubtree :: Eq a => Tree a -> Tree a -> Bool
treeHasSubtree _ Leaf = True
treeHasSubtree Leaf (Node _ _ _) = False
treeHasSubtree (Node l c r) (Node lt ct rt) = if ct == c then True else (treeHasSubtree l (Node lt ct rt) ) || (treeHasSubtree r (Node lt ct rt) )

treeHasSubtreeTests :: Test
treeHasSubtreeTests =
  TestList [ treeHasSubtree t Leaf ~?= True,
             treeHasSubtree t (Node Leaf 8 Leaf) ~?= True,
             treeHasSubtree t (Node Leaf 9 Leaf) ~?= True,
             treeHasSubtree t (Node Leaf 6 Leaf) ~?= False,
             treeHasSubtree t (Node (Node Leaf 7 Leaf) 8 Leaf) ~?= True,
             treeHasSubtree t (Node (Node Leaf 7 Leaf) 8 (Node Leaf 9 Leaf)) ~?= True,
             treeHasSubtree t (Node Leaf 8 (Node Leaf 9 Leaf)) ~?= True,
             treeHasSubtree t (Node (Node Leaf 7 Leaf) 88 (Node Leaf 9 Leaf)) ~?= False ]
  where t = Node (Node Leaf 7 Leaf) 8 (Node Leaf 9 Leaf)


treeReverse :: Tree a -> Tree a
treeReverse Leaf = Leaf
treeReverse (Node Leaf a Leaf) = (Node Leaf a Leaf)
treeReverse (Node (Node ll lc lr) c Leaf) = (Node (Node (treeReverse ll) c (treeReverse lr )) lc Leaf)
treeReverse (Node Leaf c (Node rl rc rr)) = (Node Leaf rc (Node (treeReverse rl) c (treeReverse rr) ))
--treeReverse (Node (Node ll lc lr) c (Node rl rc rr)) = (Node (Node (treeReverse rr) rc (treeReverse rl)) c (Node (treeReverse lr) lc (treeReverse ll)))
treeReverse (Node (Node ll lc lr) c (Node rl rc rr)) = (Node (treeReverse rr) rc (Node (treeReverse rl) c (Node (treeReverse lr) lc (treeReverse ll) ) ) )

treeReverseTests :: Test
treeReverseTests =
  TestList [ treeReverse Leaf ~?= (Leaf :: Tree Integer),
             treeReverse (Node Leaf 8 Leaf) ~?= Node Leaf 8 Leaf,
             treeReverse (Node (Node Leaf 7 Leaf) 8 Leaf) ~?= Node (Node Leaf 8 Leaf) 7 Leaf,
             treeReverse t ~?= tr ]
  where t  = Node (Node Leaf 1 (Node (Node Leaf 2 Leaf) 3 Leaf)) 4 (Node Leaf 5 (Node (Node Leaf 6 Leaf) 7 (Node (Node Leaf 8 Leaf) 9 Leaf)))
        tr = Node (Node Leaf 9 (Node (Node Leaf 8 Leaf) 7 Leaf)) 6 (Node Leaf 5 (Node (Node Leaf 4 Leaf) 3 (Node (Node Leaf 2 Leaf) 1 Leaf)))

{- ************************************************************ -}
{- ************************************************************ -}

{-

At the `ghci` prompt, can run individual tests like:

    *Homework01> runTestTT listSwizzleTests

At the `ghci` prompt, can run all tests like:

    *Homework01> main

At the shell prompt, can run all tests like:

    $ runhaskell Homework01.hs

-}

homework01Tests :: Test
homework01Tests =
  TestList [ listSwizzleTests,
             listHasElemTests,
             listHasDuplicatesTests,
             listDiffTests,
             listHasSublistTests,
             listRotationsTests,
             listSublistsTests,
             listTransposeTests,
             treeSumTests,
             treeProductTests,
             treeHasElemTests,
             treeHasSubtreeTests,
             treeReverseTests ]

main :: IO ()
main = do _ <- runTestTT homework01Tests
          return ()