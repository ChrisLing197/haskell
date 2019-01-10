{-

Name: Christian Ling
Time spent on assignment: Way too much time
Collaborators/Acknowledgements: isDigit function

-}

{-# OPTIONS -Wall -Wno-unused-imports -Wno-type-defaults #-}

module Homework02 where

import Data.Char (isDigit,ord)
import Test.HUnit
import Test.QuickCheck

{- ************************************************************ -}
{- ************************************************************ -}

fnRepeat :: Integer -> (a -> a) -> (a -> a)
fnRepeat 0 _ n = n
fnRepeat k f n = 
  (fnRepeat (k-1) f (f n))

fnRepeatTests :: Test
fnRepeatTests =
  TestList [ fnRepeat 10 (+ 1) 10 ~?= 20,
             fnRepeat 5 (+ 1) 10 ~?= 15,
             fnRepeat 0 (+ 1) 10 ~?= 10,
             fnRepeat 4 (\ x -> x + x) 2 ~?= 32,
             fnRepeat 0 (\ x -> x + x) 2 ~?= 2,
             fnRepeat 3 (\ x -> x ++ x) "*" ~?= "********",
             fnRepeat 0 (\ x -> x ++ x) "*" ~?= "*" ]

{- ************************************************************ -}
{- ************************************************************ -}


-- Haskell lists are "linked lists"
--   either  "nil/empty"  ( [] )
--   or "cons/non-empty"  ( x:xs )

listIsEmpty :: [a] -> Bool
listIsEmpty []    = True
listIsEmpty (_:_) = False
-- equivalent to 'null'

listFoldr :: (a -> b -> b) -> b -> [a] -> b
listFoldr _ b []     = b
listFoldr f b (x:xs) = f x (listFoldr f b xs)
-- equivalent to "foldr"

listMap :: (a -> b) -> [a] -> [b]
listMap f = listFoldr (\ x ys -> f x : ys) []
-- equivalent to "map"

listFilter :: (a -> Bool) -> [a] -> [a]
listFilter p = listFoldr (\ x ys -> if p x then x : ys else ys) []
-- equivalent to "filter"

listAppend :: [a] -> [a] -> [a]
listAppend xs ys = listFoldr (:) ys xs
-- equivalent to "++"

listFoldl :: (b -> a -> b) -> b -> [a] -> b
listFoldl _ b []     = b
listFoldl f b (x:xs) = listFoldl f (f b x) xs
-- equivalent to "foldl"

listReverse :: [a] -> [a]
listReverse = listFoldl (flip (:)) []
-- equivalent to "reverse"

listAll :: (a -> Bool) -> [a] -> Bool
listAll p = listFoldl (\ b x -> p x && b) True
-- equivalent to "all"

listAny :: (a -> Bool) -> [a] -> Bool
listAny p = listFoldl (\ b x -> p x || b) False
-- equivalent to "any"


listUnfoldl :: (b -> Maybe (a, b)) -> b -> [a]
listUnfoldl f b =
  case f b of
    Nothing      -> []
    Just (x, b') -> x : listUnfoldl f b'

listReplicate :: Integer -> a -> [a]
listReplicate n x = listUnfoldl (\ m -> if m <= 0 then Nothing else Just (x, pred m)) n

listUnfoldr :: (b -> Maybe (b, a)) -> b -> [a]
listUnfoldr = listUnfoldrAux []
  where listUnfoldrAux :: [a] -> (b -> Maybe (b, a)) -> b -> [a]
        listUnfoldrAux xs f b =
          case f b of
            Nothing       -> xs
            Just (b', x) -> listUnfoldrAux (x:xs) f b'

{- ************************************************************ -}

listIntersperse :: a -> [a] -> [a]
listIntersperse _ []     = []
listIntersperse _ [a]    = [a]
listIntersperse c (a:as) =
  a:c:(listIntersperse c as)

listIntersperseTests :: Test
listIntersperseTests =
  TestList [ listIntersperse 0 [] ~?= [],
             listIntersperse 0 [1] ~?= [1],
             listIntersperse 0 [1,2] ~?= [1,0,2],
             listIntersperse 0 [1,2,3,4] ~?= [1,0,2,0,3,0,4],
             listIntersperse ',' "" ~?= "",
             listIntersperse ',' "a" ~?= "a",
             listIntersperse ',' "abcd" ~?= "a,b,c,d" ]


listConcatMap :: (a -> [b]) -> [a] -> [b]
listConcatMap _ []     = []
listConcatMap f (a:as) =
  (f a) ++ (listConcatMap f as)

listConcat :: [[a]] -> [a]
listConcat = listConcatMap id

listConcatMapTests :: Test
listConcatMapTests =
  TestList [ listConcatMap (`listReplicate` '*') [] ~?= "",
             listConcatMap (`listReplicate` '*') [1] ~?= "*",
             listConcatMap (`listReplicate` '*') [1,2,3] ~?= "******",
             listConcatMap id [[1],[2,3],[4,5,6]] ~?= [1,2,3,4,5,6] ]


listIsPrefix :: Eq a => [a] -> [a] -> Bool
listIsPrefix _ []          = True
listIsPrefix [] _          = False
listIsPrefix (a:as) (b:bs) = 
  if (a == b) then
    (listIsPrefix as bs)
    else False

listIsPrefixTests :: Test
listIsPrefixTests =
  TestList [ listIsPrefix [1,2,3] [] ~?= True,
             listIsPrefix [1,2,3] [1] ~?= True,
             listIsPrefix [1,2,3] [1,2] ~?= True,
             listIsPrefix [1,2,3] [2] ~?= False,
             listIsPrefix [1,2,3] [1,2,3] ~?= True,
             listIsPrefix [1,2,3] [1,2,3,4] ~?= False,
             listIsPrefix [] [1,2,3] ~?= False ]


listParar :: (a -> [a] -> b -> b) -> b -> [a] -> b
listParar _ b []     = b
listParar f b (a:as) =
  (f as (listParar f b as))

listPararTests :: Test
listPararTests =
  TestList [ listParar (\ x xs xss -> (x:xs):xss) [] [] ~?= ([] :: [[Integer]]),
             listParar (\ x xs xss -> (x:xs):xss) [] [1] ~?= [[1]],
             listParar (\ x xs xss -> (x:xs):xss) [] [1,2] ~?= [[1,2],[2]],
             listParar (\ x xs xss -> (x:xs):xss) [] [1,2,3,4,5] ~?= [[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5]],
             listParar (\ x xs xss -> (x:xs) ++ xss) [] [1,2,3,4,5] ~?= [1,2,3,4,5,2,3,4,5,3,4,5,4,5,5],
             listParar (\ x xs c -> if listIsPrefix (x:xs) "oo" then c + 1 else c) 0 "foobar" ~?= 1,
             listParar (\ x xs c -> if listIsPrefix (x:xs) "oo" then c + 1 else c) 0 "foooobarfooo" ~?= 5 ]


huttonListUnfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
huttonListUnfold f g h n = 
  if (f n) then []
    else [g n]++(huttonListUnfold f g h (h n))

huttonListUnfoldTests :: Test
huttonListUnfoldTests =
  TestList [ huttonListUnfold (<= 0) id pred 5 ~?= [5,4,3,2,1],
             huttonListUnfold (<= 0) id pred 0 ~?= [],
             huttonListUnfold (<= 0) (\ b -> b * b) (\ b -> b - 2) 25 ~?= [625,529,441,361,289,225,169,121,81,49,25,9,1],
             huttonListUnfold (<= 0) (`listReplicate` True) (\ b -> b - 2) 25 ~?= [[True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True],[True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True],[True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True],[True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True],[True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True],[True,True,True,True,True,True,True,True,True,True,True,True,True,True,True],[True,True,True,True,True,True,True,True,True,True,True,True,True],[True,True,True,True,True,True,True,True,True,True,True],[True,True,True,True,True,True,True,True,True],[True,True,True,True,True,True,True],[True,True,True,True,True],[True,True,True],[True]] ]

{- ************************************************************ -}
{- ************************************************************ -}

{- Tree Functions -}

-- The following defines a type of binary trees:
--   either a leaf (Leaf)
--   or a node with a left sub-tree, an element, and a right sub-tree  (Node tl x rt)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Leaf         = Leaf
treeMap f (Node l x r) = Node (treeMap f l) (f x) (treeMap f r)

treeFold :: (b -> a -> b -> b) -> b -> Tree a -> b
treeFold _ b Leaf         = b
treeFold f b (Node l x r) = f (treeFold f b l) x (treeFold f b r)

treeUnfold :: (b -> Maybe (b, a, b)) -> b -> Tree a
treeUnfold f b =
    case f b of
      Nothing -> Leaf
      Just (bl, x, br) -> Node (treeUnfold f bl) x (treeUnfold f br)

treeSize :: Tree a -> Integer
treeSize = treeFold (\ l _ r -> 1 + l + r) 0

treeHeight :: Tree a -> Integer
treeHeight = treeFold (\ l _ r -> 1 + max l r) 0

treeEx1, treeEx2, treeEx3 :: Tree Integer
treeEx1 = Node (Node Leaf 7 Leaf) 8 (Node Leaf 9 Leaf)
treeEx2 = Node Leaf 8 Leaf
treeEx3 = Leaf

treeEx4, treeEx5, treeEx6 :: Tree Char
treeEx4 = Node (Node Leaf 'A' Leaf) 'B' (Node Leaf 'C' Leaf)
treeEx5 = Node Leaf 'B' Leaf
treeEx6 = Leaf

treeEx9 :: Tree Integer
treeEx9 = Node (Node (Node Leaf 5 Leaf) 4 (Node Leaf 1 Leaf)) 3 (Node (Node Leaf 2 Leaf) 8 (Node Leaf 3 Leaf))

{- ************************************************************ -}

treeToList :: Tree a -> [a]
treeToList Leaf         = []
treeToList (Node a b c) =
  (treeToList a)++[b]++(treeToList c)

treeToListTests :: Test
treeToListTests =
  TestList [ treeToList treeEx1 ~?= [7, 8, 9],
             treeToList treeEx2 ~?= [8],
             treeToList treeEx3 ~?= [],
             treeToList treeEx4 ~?= "ABC",
             treeToList treeEx5 ~?= "B",
             treeToList treeEx6 ~?= "" ]


treeTakeWhile :: (a -> Bool) -> Tree a -> Tree a
treeTakeWhile _ Leaf         = Leaf
treeTakeWhile f (Node a b c) =
  if (f b) then (Node (treeTakeWhile a) b (treeTakeWhile c))
    else Leaf   

treeTakeWhileTests :: Test
treeTakeWhileTests =
  TestList [
             treeTakeWhile (<= 2) treeEx9 ~?= Leaf,
             treeTakeWhile (<= 3) treeEx9 ~?= Node Leaf 3 Leaf,
             treeTakeWhile (<= 4) treeEx9 ~?= Node (Node Leaf 4 (Node Leaf 1 Leaf)) 3 Leaf,
             treeTakeWhile (<= 5) treeEx9 ~?= Node (Node (Node Leaf 5 Leaf) 4 (Node Leaf 1 Leaf)) 3 Leaf,
             treeTakeWhile (<= 6) treeEx9 ~?= Node (Node (Node Leaf 5 Leaf) 4 (Node Leaf 1 Leaf)) 3 Leaf,
             treeTakeWhile (<= 7) treeEx9 ~?= Node (Node (Node Leaf 5 Leaf) 4 (Node Leaf 1 Leaf)) 3 Leaf,
             treeTakeWhile (<= 8) treeEx9 ~?= Node (Node (Node Leaf 5 Leaf) 4 (Node Leaf 1 Leaf)) 3 (Node (Node Leaf 2 Leaf) 8 (Node Leaf 3 Leaf))
           ]


treeFoldr :: (a -> b -> b) -> b -> Tree a -> b
treeFoldr _ d Leaf         = d
treeFoldr f d (Node a b c) =
  (treeFoldr f (f b (treeFoldr f d c)) a)

treeFoldrTests :: Test
treeFoldrTests =
  TestList [ treeFoldr (+) 0 treeEx1 ~?= 24,
             treeFoldr (+) 0 treeEx2 ~?= 8,
             treeFoldr (+) 0 treeEx3 ~?= 0,
             treeFoldr (*) 1 treeEx1 ~?= 504,
             treeFoldr (*) 1 treeEx2 ~?= 8,
             treeFoldr (*) 1 treeEx3 ~?= 1,
             treeFoldr (\ x y -> y - x * x) 1 treeEx1 ~?= -193,
             treeFoldr (\ x y -> y - x * x) 1 treeEx2 ~?= -63,
             treeFoldr (\ x y -> y - x * x) 1 treeEx3 ~?= 1,
             treeFoldr (:) [] treeEx4 ~?= "ABC",
             treeFoldr (:) [] treeEx5 ~?= "B",
             treeFoldr (:) [] treeEx6 ~?= "" ]


treeFoldl :: (b -> a -> b) -> b -> Tree a -> b
treeFoldl _ d Leaf         = d
treeFoldl f d (Node a b c) = 
  (treeFoldl f (f (treeFoldl f d a) b) c)

treeFoldlTests :: Test
treeFoldlTests =
  TestList [ treeFoldl (+) 0 treeEx1 ~?= 24,
             treeFoldl (+) 0 treeEx2 ~?= 8,
             treeFoldl (+) 0 treeEx3 ~?= 0,
             treeFoldl (*) 1 treeEx1 ~?= 504,
             treeFoldl (*) 1 treeEx2 ~?= 8,
             treeFoldl (*) 1 treeEx3 ~?= 1,
             treeFoldl (\ x y -> y - x * x) 1 treeEx1 ~?= -775,
             treeFoldl (\ x y -> y - x * x) 1 treeEx2 ~?= 7,
             treeFoldl (\ x y -> y - x * x) 1 treeEx3 ~?= 1,
             treeFoldl (flip (:)) [] treeEx4 ~?= "CBA",
             treeFoldl (flip (:)) [] treeEx5 ~?= "B",
             treeFoldl (flip (:)) [] treeEx6 ~?= ""
             ]


treeAdjust :: Tree Integer -> Integer -> Tree Integer
treeAdjust Leaf         _ = Leaf
treeAdjust (Node a b c) n = 
  (Node (treeAdjust a n) (b+n) (treeAdjust c n))
treeBuild :: Integer -> Tree Integer
treeBuild 0 = Leaf
treeBuild n = 
  if (n <= 2) then
    Node (treeBuild (n - 1)) (n - 1) (treeAdjust (treeBuild (n - 1)) n)
  else
    Node (treeBuild (n - 1)) n (treeAdjust (treeBuild (n - 1)) (n+1))

treeBuildTests :: Test
treeBuildTests =
  TestList [ treeBuild 0 ~?= Leaf,
             treeBuild 1 ~?= Node Leaf 0 Leaf,
             treeBuild 2 ~?= Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf),
             treeBuild 3 ~?= Node (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) 3 (Node (Node Leaf 4 Leaf) 5 (Node Leaf 6 Leaf)) ]

{- ************************************************************ -}
{- ************************************************************ -}

-- The following useful functions have been imported from Data.Char
--   isDigit :: Char -> Bool
--   ord :: Char -> Int
allNumbers :: String -> Bool
allNumbers ""     = True
allNumbers (a:as) = 
  (isDigit a) && (allNumbers as)

luhnSum :: String -> Int
luhnSum ""         = 0
luhnSum (a:"")     =
  (ord a)-48
luhnSum (a:as:aas) = 
  ((ord a)-48)+(div (((ord as)-48)*2) 10)+(mod (((ord as)-48)*2) 10)+(luhnSum aas)

luhnCheck :: String -> Bool
luhnCheck "" = False
luhnCheck s  =
  (allNumbers s) && ((mod (luhnSum (reverse s)) 10) == 0)

luhnCheckTests :: Test
luhnCheckTests =
  TestList [ luhnCheck "1784" ~?= True,
             luhnCheck "4783" ~?= False,
             luhnCheck "79927398713" ~?= True,
             luhnCheck "79927398712" ~?= False,
             luhnCheck "79972398713" ~?= False,
             luhnCheck "Acct# 79927398713" ~?= False,
             luhnCheck "7992739871x" ~?= False,
             luhnCheck "" ~?= False ]

{- ************************************************************ -}
{- ************************************************************ -}

type Var = String

data Fmla = FmlaConst Bool | FmlaVar Var
          | FmlaNot Fmla | FmlaOr Fmla Fmla | FmlaAnd Fmla Fmla
          deriving (Eq, Show)

fmlaEval :: (Var -> Bool) -> Fmla -> Bool
fmlaEval _ (FmlaConst b)   = b
fmlaEval v (FmlaVar x)     = v x
fmlaEval v (FmlaNot f)     = not (fmlaEval v f)
fmlaEval v (FmlaOr f1 f2)  = fmlaEval v f1 || fmlaEval v f2
fmlaEval v (FmlaAnd f1 f2) = fmlaEval v f1 && fmlaEval v f2

fmlaToCNF :: Fmla -> CNF
fmlaToCNF = nnfToCNF . fmlaToNNF

{- ************************************************************ -}

type Lit = (Bool, Var)

litToFmla :: (Bool, Var) -> Fmla
litToFmla (True, x)  = FmlaVar x
litToFmla (False, x) = FmlaNot (FmlaVar x)

litEval :: (Var -> Bool) -> (Bool, Var) -> Bool
litEval v (True,  x) = v x
litEval v (False, x) = not (v x)

{- ************************************************************ -}

data NNF = NNFLit Lit | NNFOr [NNF] | NNFAnd [NNF] deriving (Eq, Show)

nnfToFmla :: NNF -> Fmla
nnfToFmla (NNFLit lit)  = litToFmla lit
nnfToFmla (NNFOr nnfs)  = foldr (FmlaOr . nnfToFmla) (FmlaConst False) nnfs
nnfToFmla (NNFAnd nnfs) = foldr (FmlaAnd . nnfToFmla) (FmlaConst True) nnfs


nnfEval :: (Var -> Bool) -> NNF -> Bool
nnfEval f nnf =
  (fmlaEval f (nnfToFmla nnf)) 

nnfEvalTests :: Test
nnfEvalTests = TestList (map (\ (s,nnf) -> TestLabel ("nnfEvalCheck " ++ s) (nnfEvalCheck nnf)) nnfExs)

nnfEvalCheck :: NNF -> Test
nnfEvalCheck nnf = all (\ v -> fmlaEval v p == nnfEval v nnf) vs ~?= True
  where p = nnfToFmla nnf
        vs = mkValuations (fmlaVars p)


fmlaToNNF :: Fmla -> NNF
fmlaToNNF (FmlaVar a)             = NNFLit (True, a)
fmlaToNNF (FmlaConst a)           = if a then NNFAnd [] else NNFOr []
fmlaToNNF (FmlaAnd a b)           = NNFAnd [fmlaToNNF a, fmlaToNNF b]
fmlaToNNF (FmlaOr a b)            = NNFOr [fmlaToNNF a, fmlaToNNF b]
fmlaToNNF (FmlaNot (FmlaVar a))   = NNFLit (False, a)
fmlaToNNF (FmlaNot (FmlaConst a)) = if a then NNFOr [] else NNFAnd []
fmlaToNNF (FmlaNot (FmlaOr a b))  = fmlaToNNF (FmlaAnd (FmlaNot a) (FmlaNot b))
fmlaToNNF (FmlaNot (FmlaAnd a b)) = fmlaToNNF (FmlaOr (FmlaNot a) (FmlaNot b))
fmlaToNNF (FmlaNot (FmlaNot a))   = fmlaToNNF a
 

fmlaToNNFTests :: Test
fmlaToNNFTests = TestList (map (\ (s,p) -> TestLabel ("fmlaToNNFCheck " ++ s) (fmlaToNNFCheck p)) fmlaExs)

fmlaToNNFCheck :: Fmla -> Test
fmlaToNNFCheck p = all (\ v -> fmlaEval v p == fmlaEval v q) vs ~?= True
  where nnf = fmlaToNNF p
        q = nnfToFmla nnf
        vs = mkValuations (fmlaVars (FmlaOr p q))

{- ************************************************************ -}

type CNF = [[(Bool, Var)]]

cnfToFmla :: CNF -> Fmla
cnfToFmla = conjToFmla
  where conjToFmla = foldr (FmlaAnd . disjToFmla) (FmlaConst True)
        disjToFmla = foldr (FmlaOr . litToFmla) (FmlaConst False)


cnfEval :: (Var -> Bool) -> CNF -> Bool
cnfEval f cnf =
  (fmlaEval f (cnfToFmla cnf)) 

cnfEvalTests :: Test
cnfEvalTests = TestList (map (\ (s,cnf) -> TestLabel ("cnfEvalCheck " ++ s) (cnfEvalCheck cnf)) cnfExs)

cnfEvalCheck :: CNF -> Test
cnfEvalCheck cnf = all (\ v -> fmlaEval v p == cnfEval v cnf) vs ~?= True
  where p = cnfToFmla cnf
        vs = mkValuations (fmlaVars p)


nnfToCNF :: NNF -> CNF
nnfToCNF (NNFLit l)      = [[l]]
nnfToCNF (NNFOr  [])     = [[]]
nnfToCNF (NNFOr  (a:as)) = (nnfToCNF a) ++ (nnfToCNF (NNFAnd as))
nnfToCNF (NNFAnd [])     = []
nnfToCNF (NNFAnd (a:as)) =  (nnfToCNF a) ++ (nnfToCNF (NNFAnd as))

nnfToCNFTests :: Test
nnfToCNFTests = TestList (map (\ (s,nnf) -> TestLabel ("nnfToCNFCheck " ++ s) (nnfToCNFCheck nnf)) nnfExs)

nnfToCNFCheck :: NNF -> Test
nnfToCNFCheck nnf = all (\ v -> fmlaEval v p == fmlaEval v q) vs ~?= True
  where p = nnfToFmla nnf
        cnf = nnfToCNF nnf
        q = cnfToFmla cnf
        vs = mkValuations (fmlaVars (FmlaOr p q))

fmlaToCNFCheck :: Fmla -> Test
fmlaToCNFCheck p = all (\ v -> fmlaEval v p == fmlaEval v q) vs ~?= True
  where cnf = fmlaToCNF p
        q = cnfToFmla cnf
        vs = mkValuations (fmlaVars (FmlaOr p q))

{- ************************************************************ -}
{- ************************************************************ -}

{-

At the `ghci` prompt, can run individual tests like:

    *Homework02> runTestTT fnRepeatTests

At the `ghci` prompt, can run all tests like:

    *Homework02> main

At the shell prompt, can run all tests like:

    $ runhaskell Homework02.hs

-}

homework02Tests :: Test
homework02Tests =
  TestList [ fnRepeatTests,
             listIntersperseTests,
             listConcatMapTests,
             listIsPrefixTests,
             listPararTests,
             huttonListUnfoldTests,
             treeToListTests,
             treeTakeWhileTests,
             treeFoldrTests,
             treeFoldlTests,
             treeBuildTests,
             luhnCheckTests,
             nnfEvalTests,
             fmlaToNNFTests,
             cnfEvalTests,
             nnfToCNFTests ]

main :: IO ()
main = do _ <- runTestTT homework02Tests
          return ()

{- ************************************************************ -}
{- ************************************************************ -}

fmlaVars :: Fmla -> [Var]
fmlaVars fmla = fmlaVarsAux fmla []
  where fmlaVarsAux (FmlaConst _)   ys = ys
        fmlaVarsAux (FmlaVar x)     ys = if listAny (x==) ys then ys else x:ys
        fmlaVarsAux (FmlaNot f)     ys = fmlaVarsAux f ys
        fmlaVarsAux (FmlaOr f1 f2)  ys = fmlaVarsAux f2 (fmlaVarsAux f1 ys)
        fmlaVarsAux (FmlaAnd f1 f2) ys = fmlaVarsAux f2 (fmlaVarsAux f1 ys)

mkValuations :: [Var] -> [Var -> Bool]
mkValuations []     = [const False]
mkValuations (x:xs) = mk True ++ mk False
  where vs = mkValuations xs
        mk b = map (\ v y -> if x == y then b else v y) vs



fmlaEx01 :: Fmla
fmlaEx01 = FmlaConst False
fmlaEx02 :: Fmla
fmlaEx02 = FmlaConst True
fmlaEx03 :: Fmla
fmlaEx03 = FmlaVar "a"
fmlaEx04 :: Fmla
fmlaEx04 = FmlaOr (FmlaNot (FmlaVar "a")) (FmlaVar "b")
fmlaEx05 :: Fmla
fmlaEx05 = FmlaOr (FmlaNot (FmlaOr (FmlaVar "h") (FmlaNot (FmlaVar "e")))) (FmlaOr (FmlaVar "c") (FmlaConst False))
fmlaEx06 :: Fmla
fmlaEx06 = FmlaNot (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaNot (FmlaVar "e")))
fmlaEx07 :: Fmla
fmlaEx07 = FmlaOr (FmlaVar "e") (FmlaVar "h")
fmlaEx08 :: Fmla
fmlaEx08 = FmlaNot (FmlaVar "c")
fmlaEx09 :: Fmla
fmlaEx09 = FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "a")) (FmlaNot (FmlaNot (FmlaVar "d")))
fmlaEx10 :: Fmla
fmlaEx10 = FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaOr (FmlaVar "d") (FmlaVar "a"))) (FmlaVar "b")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaConst False) (FmlaVar "b")) (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "e") (FmlaVar "a"))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "c")) (FmlaOr (FmlaVar "e") (FmlaVar "d"))) (FmlaOr (FmlaVar "e") (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "c")) (FmlaOr (FmlaVar "d") (FmlaVar "a"))) (FmlaNot (FmlaVar "b")))))))) (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "d")) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaConst True) (FmlaVar "d")) (FmlaNot (FmlaNot (FmlaConst True))))) (FmlaVar "d")) (FmlaNot (FmlaOr (FmlaConst True) (FmlaOr (FmlaNot (FmlaOr (FmlaConst True) (FmlaVar "b"))) (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "a")) (FmlaVar "c")) (FmlaVar "e"))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "e") (FmlaVar "b")))) (FmlaOr (FmlaVar "a") (FmlaNot (FmlaNot (FmlaVar "e"))))) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "d")) (FmlaVar "c"))))) (FmlaVar "e")))))))) (FmlaVar "a"))) (FmlaVar "c")) (FmlaVar "a")) (FmlaVar "a"))
fmlaEx11 :: Fmla
fmlaEx11 = FmlaOr (FmlaOr (FmlaConst True) (FmlaVar "a")) (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaVar "b") (FmlaNot (FmlaNot (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaNot (FmlaVar "c")))))) (FmlaOr (FmlaVar "b") (FmlaVar "b")))) (FmlaVar "d"))
fmlaEx12 :: Fmla
fmlaEx12 = FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "e") (FmlaOr (FmlaNot (FmlaOr (FmlaVar "j") (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "f") (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaVar "e"))) (FmlaVar "i"))) (FmlaVar "h")))) (FmlaNot (FmlaOr (FmlaVar "b") (FmlaVar "b")))))) (FmlaOr (FmlaVar "i") (FmlaVar "i"))) (FmlaOr (FmlaVar "e") (FmlaConst True))) (FmlaVar "f")
fmlaEx13 :: Fmla
fmlaEx13 = FmlaNot (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaOr (FmlaConst False) (FmlaVar "a")))) (FmlaOr (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaVar "c")) (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "e")) (FmlaVar "e"))))) (FmlaVar "c"))))
fmlaEx14 :: Fmla
fmlaEx14 = FmlaNot (FmlaOr (FmlaVar "g") (FmlaOr (FmlaVar "g") (FmlaOr (FmlaNot (FmlaOr (FmlaConst True) (FmlaOr (FmlaVar "g") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaConst False) (FmlaOr (FmlaVar "b") (FmlaConst True))))) (FmlaOr (FmlaVar "a") (FmlaNot (FmlaOr (FmlaNot (FmlaOr (FmlaConst False) (FmlaVar "a"))) (FmlaNot (FmlaNot (FmlaConst True))))))) (FmlaConst False)) (FmlaNot (FmlaOr (FmlaVar "g") (FmlaOr (FmlaNot (FmlaConst False)) (FmlaVar "j"))))) (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaOr (FmlaOr (FmlaVar "g") (FmlaOr (FmlaOr (FmlaVar "a") (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "j") (FmlaOr (FmlaConst True) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "i") (FmlaVar "h")) (FmlaOr (FmlaOr (FmlaConst True) (FmlaVar "g")) (FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "j"))))) (FmlaOr (FmlaVar "h") (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "b")) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "j") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "i") (FmlaOr (FmlaVar "g") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "h") (FmlaVar "e")) (FmlaNot (FmlaVar "b"))) (FmlaOr (FmlaNot (FmlaNot (FmlaNot (FmlaVar "b")))) (FmlaOr (FmlaNot (FmlaOr (FmlaConst False) (FmlaVar "b"))) (FmlaOr (FmlaVar "g") (FmlaNot (FmlaVar "j")))))) (FmlaOr (FmlaVar "j") (FmlaNot (FmlaNot (FmlaVar "g"))))) (FmlaNot (FmlaVar "a"))) (FmlaVar "d")))) (FmlaVar "h")) (FmlaVar "b")) (FmlaVar "d"))) (FmlaVar "h"))))))))) (FmlaVar "d")) (FmlaOr (FmlaNot (FmlaVar "h")) (FmlaVar "i"))))) (FmlaOr (FmlaVar "j") (FmlaOr (FmlaOr (FmlaVar "j") (FmlaOr (FmlaVar "j") (FmlaVar "c"))) (FmlaVar "b"))))) (FmlaOr (FmlaVar "j") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "h") (FmlaOr (FmlaOr (FmlaVar "g") (FmlaVar "i")) (FmlaConst False))) (FmlaVar "b")) (FmlaOr (FmlaVar "h") (FmlaVar "e"))) (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "a")) (FmlaVar "d"))) (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "g")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "d") (FmlaNot (FmlaNot (FmlaVar "c")))) (FmlaVar "j")) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "f") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "i") (FmlaNot (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaVar "i") (FmlaOr (FmlaNot (FmlaVar "j")) (FmlaVar "b"))) (FmlaConst True))))) (FmlaVar "i"))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaVar "h") (FmlaVar "j")) (FmlaVar "a")))) (FmlaOr (FmlaOr (FmlaVar "i") (FmlaVar "a")) (FmlaVar "i")))) (FmlaVar "j")) (FmlaOr (FmlaNot (FmlaVar "f")) (FmlaVar "b"))))) (FmlaVar "e"))) (FmlaVar "g")) (FmlaVar "e"))) (FmlaOr (FmlaConst True) (FmlaVar "i"))))) (FmlaOr (FmlaOr (FmlaVar "f") (FmlaNot (FmlaNot (FmlaOr (FmlaVar "c") (FmlaVar "i"))))) (FmlaVar "g"))) (FmlaVar "f")))) (FmlaVar "i"))))))))) (FmlaOr (FmlaOr (FmlaVar "g") (FmlaOr (FmlaOr (FmlaNot (FmlaVar "g")) (FmlaVar "f")) (FmlaNot (FmlaVar "i")))) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "j") (FmlaVar "a")) (FmlaVar "b")))))))
fmlaEx15 :: Fmla
fmlaEx15 = FmlaOr (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "b")) (FmlaOr (FmlaVar "b") (FmlaOr (FmlaConst False) (FmlaOr (FmlaNot (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "a") (FmlaNot (FmlaVar "e")))))) (FmlaNot (FmlaNot (FmlaVar "a")))))))))) (FmlaVar "e")
fmlaEx16 :: Fmla
fmlaEx16 = FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "f") (FmlaNot (FmlaOr (FmlaVar "c") (FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "i")))))) (FmlaVar "j")) (FmlaVar "i"))) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "h") (FmlaOr (FmlaVar "g") (FmlaVar "i"))) (FmlaNot (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaVar "j") (FmlaOr (FmlaVar "f") (FmlaOr (FmlaConst False) (FmlaNot (FmlaVar "j"))))) (FmlaOr (FmlaVar "c") (FmlaOr (FmlaConst False) (FmlaOr (FmlaVar "g") (FmlaOr (FmlaVar "a") (FmlaVar "c"))))))))) (FmlaOr (FmlaNot (FmlaNot (FmlaConst True))) (FmlaNot (FmlaVar "f")))) (FmlaNot (FmlaVar "h"))) (FmlaVar "e")) (FmlaOr (FmlaConst True) (FmlaNot (FmlaVar "a")))))))))) (FmlaOr (FmlaOr (FmlaNot (FmlaVar "e")) (FmlaVar "h")) (FmlaVar "e"))))
fmlaEx17 :: Fmla
fmlaEx17 = FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaConst True)) (FmlaVar "c")) (FmlaVar "d")
fmlaEx18 :: Fmla
fmlaEx18 = FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "g") (FmlaVar "c")) (FmlaVar "g")) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "e")) (FmlaConst True)) (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "c")) (FmlaVar "i")) (FmlaNot (FmlaOr (FmlaVar "f") (FmlaVar "g")))) (FmlaVar "j"))) (FmlaConst True))))
fmlaEx19 :: Fmla
fmlaEx19 = FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "b") (FmlaVar "c"))) (FmlaVar "d")) (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaVar "e") (FmlaConst True)) (FmlaVar "a")))))
fmlaEx20 :: Fmla
fmlaEx20 = FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "g")) (FmlaNot (FmlaVar "g"))
fmlaEx21 :: Fmla
fmlaEx21 = FmlaOr (FmlaVar "b") (FmlaNot (FmlaVar "a"))
fmlaEx22 :: Fmla
fmlaEx22 = FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "f")) (FmlaVar "g"))) (FmlaVar "j")
fmlaEx23 :: Fmla
fmlaEx23 = FmlaOr (FmlaVar "d") (FmlaOr (FmlaOr (FmlaVar "i") (FmlaNot (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaNot (FmlaVar "j")) (FmlaVar "f"))) (FmlaOr (FmlaOr (FmlaNot (FmlaVar "i")) (FmlaNot (FmlaVar "e"))) (FmlaVar "d"))))) (FmlaOr (FmlaVar "f") (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaNot (FmlaVar "i")) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "h") (FmlaVar "c")) (FmlaOr (FmlaVar "c") (FmlaVar "b"))))) (FmlaNot (FmlaVar "f")))) (FmlaOr (FmlaVar "e") (FmlaVar "a"))) (FmlaNot (FmlaOr (FmlaVar "i") (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaVar "g") (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "e") (FmlaConst False))) (FmlaNot (FmlaOr (FmlaVar "j") (FmlaNot (FmlaVar "b"))))) (FmlaVar "a"))) (FmlaVar "f")))) (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "d") (FmlaNot (FmlaNot (FmlaVar "f"))))) (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaConst False)) (FmlaVar "i")) (FmlaOr (FmlaVar "h") (FmlaVar "h"))) (FmlaVar "i")))) (FmlaOr (FmlaOr (FmlaConst False) (FmlaOr (FmlaVar "i") (FmlaVar "d"))) (FmlaNot (FmlaOr (FmlaVar "b") (FmlaVar "i"))))))))))))
fmlaEx24 :: Fmla
fmlaEx24 = FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "e"))
fmlaEx25 :: Fmla
fmlaEx25 = FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "d") (FmlaOr (FmlaOr (FmlaVar "i") (FmlaOr (FmlaVar "d") (FmlaConst True))) (FmlaNot (FmlaVar "g")))) (FmlaVar "e"))) (FmlaVar "h"))) (FmlaVar "d"))) (FmlaVar "d")
fmlaEx26 :: Fmla
fmlaEx26 = FmlaNot (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "b") (FmlaVar "b"))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "a")) (FmlaVar "b")) (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "b")) (FmlaNot (FmlaVar "a")))))
fmlaEx27 :: Fmla
fmlaEx27 = FmlaOr (FmlaVar "h") (FmlaOr (FmlaOr (FmlaNot (FmlaConst False)) (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaConst False) (FmlaConst False)) (FmlaVar "g")) (FmlaOr (FmlaVar "i") (FmlaVar "f"))))) (FmlaOr (FmlaConst True) (FmlaOr (FmlaOr (FmlaVar "d") (FmlaNot (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "i") (FmlaVar "c")) (FmlaNot (FmlaOr (FmlaConst True) (FmlaVar "i")))) (FmlaOr (FmlaNot (FmlaOr (FmlaVar "e") (FmlaVar "b"))) (FmlaVar "h"))) (FmlaVar "j")) (FmlaVar "i"))))) (FmlaOr (FmlaOr (FmlaNot (FmlaVar "j")) (FmlaNot (FmlaOr (FmlaVar "g") (FmlaOr (FmlaOr (FmlaVar "j") (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaVar "g")) (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaNot (FmlaVar "i")))) (FmlaVar "i"))) (FmlaOr (FmlaNot (FmlaOr (FmlaVar "h") (FmlaVar "h"))) (FmlaVar "b")))) (FmlaConst True))))) (FmlaVar "c")))))
fmlaEx28 :: Fmla
fmlaEx28 = FmlaOr (FmlaConst True) (FmlaVar "b")
fmlaEx29 :: Fmla
fmlaEx29 = FmlaNot (FmlaNot (FmlaVar "d"))
fmlaEx30 :: Fmla
fmlaEx30 = FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "a")) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "c")) (FmlaVar "d"))))) (FmlaOr (FmlaNot (FmlaOr (FmlaVar "d") (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "c")) (FmlaVar "e")))) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "e")) (FmlaNot (FmlaConst True))) (FmlaVar "a")) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaVar "e") (FmlaConst True)) (FmlaOr (FmlaConst False) (FmlaVar "a")))) (FmlaOr (FmlaVar "e") (FmlaOr (FmlaConst True) (FmlaVar "b")))))) (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "c")) (FmlaVar "a")) (FmlaOr (FmlaVar "e") (FmlaConst True)))) (FmlaConst True))) (FmlaVar "a")) (FmlaVar "d")) (FmlaOr (FmlaVar "d") (FmlaVar "a")))) (FmlaVar "e"))) (FmlaConst True)))) (FmlaVar "a")
fmlaEx31 :: Fmla
fmlaEx31 = FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "i") (FmlaVar "j")) (FmlaVar "f")) (FmlaOr (FmlaVar "a") (FmlaVar "i"))) (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "d")) (FmlaVar "i"))
fmlaEx32 :: Fmla
fmlaEx32 = FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaVar "e")) (FmlaNot (FmlaNot (FmlaVar "a"))))
fmlaEx33 :: Fmla
fmlaEx33 = FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "c") (FmlaNot (FmlaNot (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaVar "a")))))) (FmlaVar "e")) (FmlaVar "f")))
fmlaEx34 :: Fmla
fmlaEx34 = FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "e") (FmlaVar "a"))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "a")) (FmlaVar "e")) (FmlaVar "c"))))
fmlaEx35 :: Fmla
fmlaEx35 = FmlaOr (FmlaVar "i") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "c")) (FmlaOr (FmlaConst False) (FmlaVar "j"))) (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "g") (FmlaNot (FmlaOr (FmlaVar "i") (FmlaVar "d")))) (FmlaVar "f"))) (FmlaVar "g")))) (FmlaOr (FmlaVar "i") (FmlaOr (FmlaVar "j") (FmlaVar "f"))))) (FmlaNot (FmlaVar "b")))
fmlaEx36 :: Fmla
fmlaEx36 = FmlaOr (FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "b"))) (FmlaConst False)
fmlaEx37 :: Fmla
fmlaEx37 = FmlaOr (FmlaOr (FmlaNot (FmlaVar "e")) (FmlaOr (FmlaNot (FmlaConst True)) (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "e")) (FmlaVar "d")))) (FmlaVar "d")
fmlaEx38 :: Fmla
fmlaEx38 = FmlaNot (FmlaOr (FmlaVar "i") (FmlaOr (FmlaOr (FmlaVar "h") (FmlaVar "g")) (FmlaVar "b")))
fmlaEx39 :: Fmla
fmlaEx39 = FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "d") (FmlaVar "c"))
fmlaEx40 :: Fmla
fmlaEx40 = FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaConst True) (FmlaVar "i")) (FmlaOr (FmlaOr (FmlaVar "j") (FmlaVar "j")) (FmlaOr (FmlaOr (FmlaVar "i") (FmlaOr (FmlaOr (FmlaVar "j") (FmlaVar "d")) (FmlaVar "i"))) (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "i") (FmlaVar "b"))) (FmlaVar "g")) (FmlaVar "f")))))) (FmlaOr (FmlaNot (FmlaVar "g")) (FmlaVar "e")))
fmlaEx41 :: Fmla
fmlaEx41 = FmlaOr (FmlaVar "c") (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaConst False) (FmlaVar "d")) (FmlaOr (FmlaVar "e") (FmlaVar "b")))) (FmlaOr (FmlaVar "c") (FmlaConst True))) (FmlaOr (FmlaOr (FmlaNot (FmlaConst True)) (FmlaOr (FmlaVar "e") (FmlaVar "d"))) (FmlaVar "e")))) (FmlaVar "e"))
fmlaEx42 :: Fmla
fmlaEx42 = FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaVar "a") (FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "e")))) (FmlaOr (FmlaVar "d") (FmlaNot (FmlaVar "c"))))
fmlaEx43 :: Fmla
fmlaEx43 = FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "c")) (FmlaOr (FmlaOr (FmlaVar "g") (FmlaOr (FmlaConst True) (FmlaVar "h"))) (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaVar "h"))) (FmlaVar "d")) (FmlaVar "c")))) (FmlaOr (FmlaOr (FmlaVar "j") (FmlaVar "d")) (FmlaNot (FmlaVar "d")))
fmlaEx44 :: Fmla
fmlaEx44 = FmlaOr (FmlaVar "e") (FmlaOr (FmlaNot (FmlaNot (FmlaVar "e"))) (FmlaVar "a"))
fmlaEx45 :: Fmla
fmlaEx45 = FmlaOr (FmlaNot (FmlaVar "b")) (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "c") (FmlaVar "b"))) (FmlaVar "b"))
fmlaEx46 :: Fmla
fmlaEx46 = FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "i") (FmlaOr (FmlaVar "d") (FmlaOr (FmlaOr (FmlaVar "f") (FmlaOr (FmlaNot (FmlaOr (FmlaVar "j") (FmlaNot (FmlaVar "b")))) (FmlaOr (FmlaNot (FmlaOr (FmlaVar "a") (FmlaVar "g"))) (FmlaNot (FmlaVar "a"))))) (FmlaOr (FmlaVar "f") (FmlaOr (FmlaVar "i") (FmlaVar "j"))))))
fmlaEx47 :: Fmla
fmlaEx47 = FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaConst False) (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaNot (FmlaOr (FmlaConst True) (FmlaVar "d"))) (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "b")) (FmlaNot (FmlaVar "d"))) (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "d")) (FmlaVar "b"))) (FmlaNot (FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "d")))))))) (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "b") (FmlaNot (FmlaVar "c"))) (FmlaVar "d"))) (FmlaVar "e"))) (FmlaVar "e")))) (FmlaOr (FmlaConst False) (FmlaOr (FmlaVar "d") (FmlaVar "c")))) (FmlaOr (FmlaNot (FmlaVar "e")) (FmlaVar "c"))) (FmlaVar "b")
fmlaEx48 :: Fmla
fmlaEx48 = FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaNot (FmlaVar "d"))) (FmlaVar "b")) (FmlaVar "d")
fmlaEx49 :: Fmla
fmlaEx49 = FmlaOr (FmlaOr (FmlaConst False) (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "b")) (FmlaNot (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "d") (FmlaVar "e")))) (FmlaOr (FmlaNot (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaNot (FmlaVar "b")) (FmlaOr (FmlaConst True) (FmlaNot (FmlaNot (FmlaVar "e")))))) (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "e") (FmlaVar "e")))) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "a") (FmlaVar "d"))) (FmlaNot (FmlaVar "b"))) (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "b") (FmlaNot (FmlaConst False)))) (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaVar "a") (FmlaVar "a")))) (FmlaVar "d")) (FmlaNot (FmlaNot (FmlaOr (FmlaNot (FmlaOr (FmlaVar "a") (FmlaVar "c"))) (FmlaOr (FmlaVar "c") (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "e")) (FmlaVar "d"))))))))))) (FmlaVar "c")) (FmlaNot (FmlaOr (FmlaConst False) (FmlaVar "e")))) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "c") (FmlaVar "c"))) (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "d")) (FmlaNot (FmlaVar "e")))))))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "e")) (FmlaOr (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "d") (FmlaOr (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaVar "c")) (FmlaVar "e")))) (FmlaOr (FmlaVar "e") (FmlaVar "a")))) (FmlaNot (FmlaVar "a")))))) (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "a") (FmlaConst False)))))) (FmlaOr (FmlaNot (FmlaConst True)) (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaVar "c"))))))))))) (FmlaConst True)
fmlaEx50 :: Fmla
fmlaEx50 = FmlaOr (FmlaVar "h") (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "g") (FmlaNot (FmlaNot (FmlaNot (FmlaNot (FmlaVar "i")))))) (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "j")) (FmlaOr (FmlaOr (FmlaVar "j") (FmlaVar "i")) (FmlaOr (FmlaVar "j") (FmlaConst False)))))) (FmlaOr (FmlaVar "c") (FmlaNot (FmlaNot (FmlaVar "d"))))) (FmlaOr (FmlaVar "c") (FmlaVar "e")))) (FmlaVar "f"))
fmlaEx51 :: Fmla
fmlaEx51 = FmlaNot (FmlaNot (FmlaOr (FmlaVar "e") (FmlaConst True)))
fmlaEx52 :: Fmla
fmlaEx52 = FmlaOr (FmlaConst False) (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaConst True) (FmlaVar "a"))) (FmlaVar "c"))
fmlaEx53 :: Fmla
fmlaEx53 = FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaVar "e")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "a")) (FmlaOr (FmlaConst True) (FmlaVar "d"))) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "d") (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaVar "a"))) (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaOr (FmlaVar "a") (FmlaVar "b"))) (FmlaConst True))))) (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaConst True) (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaConst True))) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaVar "b")) (FmlaVar "d")) (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "b")) (FmlaVar "b")))) (FmlaVar "e")) (FmlaVar "e")) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "c")) (FmlaNot (FmlaOr (FmlaNot (FmlaNot (FmlaConst False))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "e") (FmlaVar "d"))) (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "e")))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "b")) (FmlaVar "d")) (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "e")) (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "d")) (FmlaOr (FmlaVar "d") (FmlaVar "b"))) (FmlaVar "a")) (FmlaVar "c")) (FmlaVar "b"))))))) (FmlaVar "a"))))) (FmlaVar "c"))) (FmlaVar "e")))) (FmlaVar "b")))) (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "e") (FmlaVar "e"))))) (FmlaOr (FmlaVar "d") (FmlaVar "c")))) (FmlaVar "b")) (FmlaVar "c"))) (FmlaVar "c")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "b") (FmlaVar "e")))) (FmlaNot (FmlaVar "e"))) (FmlaVar "e")) (FmlaVar "b")) (FmlaVar "c"))) (FmlaOr (FmlaConst False) (FmlaNot (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "c")) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "c")) (FmlaOr (FmlaVar "e") (FmlaVar "a"))) (FmlaOr (FmlaNot (FmlaOr (FmlaNot (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "d") (FmlaConst False)))) (FmlaVar "d"))) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "a")) (FmlaNot (FmlaVar "a"))))))) (FmlaVar "c")))))) (FmlaOr (FmlaVar "a") (FmlaOr (FmlaConst False) (FmlaOr (FmlaVar "c") (FmlaNot (FmlaOr (FmlaVar "b") (FmlaConst False)))))))) (FmlaOr (FmlaVar "e") (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaVar "e"))))) (FmlaConst False))) (FmlaVar "a")) (FmlaConst True))) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaVar "e") (FmlaNot (FmlaVar "e"))) (FmlaNot (FmlaVar "a")))) (FmlaVar "e")) (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "c")) (FmlaOr (FmlaVar "b") (FmlaConst False))) (FmlaVar "a"))))) (FmlaVar "a")) (FmlaVar "e")) (FmlaConst True)))) (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "c") (FmlaVar "b"))))) (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "a")) (FmlaVar "d")))))
fmlaEx54 :: Fmla
fmlaEx54 = FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "d") (FmlaNot (FmlaVar "e"))) (FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaVar "g") (FmlaVar "a")) (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaVar "g") (FmlaVar "d")))) (FmlaOr (FmlaConst False) (FmlaVar "d"))))) (FmlaOr (FmlaVar "e") (FmlaVar "h")))) (FmlaConst False)) (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "i") (FmlaNot (FmlaVar "a"))) (FmlaOr (FmlaOr (FmlaVar "c") (FmlaNot (FmlaVar "i"))) (FmlaVar "a"))) (FmlaVar "j")) (FmlaVar "j")) (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "i")) (FmlaOr (FmlaVar "g") (FmlaOr (FmlaVar "j") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "j") (FmlaVar "e")) (FmlaVar "b")) (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "j") (FmlaVar "d")))) (FmlaVar "b")) (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "c") (FmlaVar "f"))) (FmlaVar "c")) (FmlaConst False)))) (FmlaVar "a")))))))) (FmlaNot (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "c") (FmlaNot (FmlaOr (FmlaNot (FmlaNot (FmlaConst False))) (FmlaVar "h")))) (FmlaOr (FmlaVar "i") (FmlaNot (FmlaVar "i"))))) (FmlaOr (FmlaVar "g") (FmlaOr (FmlaVar "i") (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "i") (FmlaVar "a"))) (FmlaVar "g"))))))) (FmlaOr (FmlaNot (FmlaNot (FmlaVar "g"))) (FmlaNot (FmlaOr (FmlaVar "j") (FmlaOr (FmlaVar "f") (FmlaVar "d")))))
fmlaEx55 :: Fmla
fmlaEx55 = FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "e")) (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "a")) (FmlaConst True)))
fmlaEx56 :: Fmla
fmlaEx56 = FmlaOr (FmlaNot (FmlaOr (FmlaVar "g") (FmlaVar "b"))) (FmlaVar "i")
fmlaEx57 :: Fmla
fmlaEx57 = FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "c")) (FmlaOr (FmlaVar "b") (FmlaVar "d")))) (FmlaVar "c"))) (FmlaConst False)
fmlaEx58 :: Fmla
fmlaEx58 = FmlaOr (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaOr (FmlaNot (FmlaNot (FmlaVar "j"))) (FmlaVar "e"))) (FmlaVar "b")
fmlaEx59 :: Fmla
fmlaEx59 = FmlaOr (FmlaNot (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "b") (FmlaVar "c")))) (FmlaVar "b")
fmlaEx60 :: Fmla
fmlaEx60 = FmlaOr (FmlaOr (FmlaConst True) (FmlaConst False)) (FmlaVar "a")
fmlaEx61 :: Fmla
fmlaEx61 = FmlaOr (FmlaConst False) (FmlaVar "b")
fmlaEx62 :: Fmla
fmlaEx62 = FmlaOr (FmlaOr (FmlaConst False) (FmlaVar "a")) (FmlaOr (FmlaVar "j") (FmlaVar "j"))
fmlaEx63 :: Fmla
fmlaEx63 = FmlaNot (FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "c")))
fmlaEx64 :: Fmla
fmlaEx64 = FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "f")) (FmlaVar "a")) (FmlaVar "d")
fmlaEx65 :: Fmla
fmlaEx65 = FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "d") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "c") (FmlaNot (FmlaOr (FmlaNot (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "b") (FmlaConst False)))) (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "e")) (FmlaOr (FmlaVar "b") (FmlaNot (FmlaOr (FmlaOr (FmlaVar "a") (FmlaConst False)) (FmlaVar "c"))))) (FmlaVar "d"))))))) (FmlaVar "d")) (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "e")) (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "c")) (FmlaOr (FmlaVar "c") (FmlaVar "e")))) (FmlaNot (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "b")) (FmlaVar "a")) (FmlaVar "a")) (FmlaVar "b")) (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "e")) (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "c")) (FmlaNot (FmlaVar "c"))))) (FmlaVar "e")))) (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "b") (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaVar "c")) (FmlaVar "b"))))) (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "a")) (FmlaNot (FmlaVar "e"))))) (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "d")) (FmlaVar "d")))) (FmlaOr (FmlaOr (FmlaNot (FmlaVar "b")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaNot (FmlaVar "c"))) (FmlaConst True)) (FmlaVar "c")) (FmlaVar "b"))) (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaNot (FmlaOr (FmlaVar "e") (FmlaVar "b")))))) (FmlaOr (FmlaVar "c") (FmlaVar "d"))))) (FmlaVar "e")) (FmlaVar "c")) (FmlaVar "a"))) (FmlaVar "b"))))) (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaNot (FmlaVar "c"))) (FmlaVar "a")) (FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "a") (FmlaVar "b")))) (FmlaOr (FmlaVar "a") (FmlaVar "e"))))) (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "e")) (FmlaVar "e")) (FmlaOr (FmlaNot (FmlaVar "b")) (FmlaVar "b"))) (FmlaVar "e"))) (FmlaVar "d")) (FmlaNot (FmlaVar "d")))) (FmlaNot (FmlaVar "a"))) (FmlaVar "a"))))) (FmlaVar "b")))) (FmlaVar "a"))) (FmlaVar "a")) (FmlaVar "e"))) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "d")) (FmlaOr (FmlaVar "b") (FmlaNot (FmlaOr (FmlaConst True) (FmlaOr (FmlaVar "a") (FmlaVar "c")))))))) (FmlaConst False)) (FmlaOr (FmlaVar "b") (FmlaVar "e")))))))
fmlaEx66 :: Fmla
fmlaEx66 = FmlaOr (FmlaOr (FmlaOr (FmlaVar "h") (FmlaVar "f")) (FmlaOr (FmlaNot (FmlaOr (FmlaVar "b") (FmlaNot (FmlaNot (FmlaConst True))))) (FmlaOr (FmlaOr (FmlaOr (FmlaConst True) (FmlaOr (FmlaVar "j") (FmlaOr (FmlaVar "j") (FmlaVar "i")))) (FmlaVar "f")) (FmlaVar "c")))) (FmlaVar "d")
fmlaEx67 :: Fmla
fmlaEx67 = FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaNot (FmlaVar "e"))) (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "c")) (FmlaVar "d"))) (FmlaOr (FmlaNot (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "b")) (FmlaVar "b")) (FmlaOr (FmlaVar "c") (FmlaVar "e"))) (FmlaVar "d")) (FmlaVar "b")))) (FmlaOr (FmlaVar "d") (FmlaVar "d")))) (FmlaOr (FmlaVar "a") (FmlaConst False))) (FmlaOr (FmlaConst True) (FmlaVar "a"))
fmlaEx68 :: Fmla
fmlaEx68 = FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaVar "c") (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "a") (FmlaNot (FmlaVar "c"))) (FmlaConst False)) (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "a")) (FmlaVar "b")) (FmlaVar "d")))) (FmlaVar "a")) (FmlaVar "d")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "a")) (FmlaVar "b")))) (FmlaVar "b"))) (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "a") (FmlaVar "e"))))) (FmlaOr (FmlaConst True) (FmlaVar "a"))) (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaConst False) (FmlaVar "d")) (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "a") (FmlaNot (FmlaVar "c"))) (FmlaVar "e")) (FmlaOr (FmlaVar "b") (FmlaVar "e"))) (FmlaVar "a")) (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaConst False) (FmlaVar "e")) (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaNot (FmlaNot (FmlaOr (FmlaVar "e") (FmlaOr (FmlaConst False) (FmlaVar "b")))))) (FmlaOr (FmlaVar "e") (FmlaConst False)))))))) (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "d") (FmlaConst False)) (FmlaVar "a"))) (FmlaVar "c")) (FmlaOr (FmlaVar "d") (FmlaVar "a"))) (FmlaVar "e")))) (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaNot (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaNot (FmlaOr (FmlaVar "e") (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaNot (FmlaOr (FmlaVar "c") (FmlaVar "a"))) (FmlaOr (FmlaNot (FmlaVar "b")) (FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "c")))))) (FmlaNot (FmlaVar "b"))) (FmlaOr (FmlaNot (FmlaNot (FmlaConst True))) (FmlaNot (FmlaVar "d")))))) (FmlaVar "e")) (FmlaOr (FmlaVar "e") (FmlaVar "d"))) (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "d")) (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaConst True) (FmlaVar "a")) (FmlaVar "a"))))))))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "a")) (FmlaOr (FmlaVar "e") (FmlaVar "e"))) (FmlaVar "e"))))) (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaVar "e"))) (FmlaVar "d")) (FmlaVar "a"))) (FmlaNot (FmlaVar "d"))))) (FmlaVar "e"))) (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "d") (FmlaVar "d"))))) (FmlaVar "e"))))))) (FmlaOr (FmlaNot (FmlaVar "e")) (FmlaNot (FmlaNot (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "e") (FmlaVar "b"))) (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "a")) (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "d") (FmlaConst False)))))) (FmlaVar "c")))))) (FmlaConst False))))))))) (FmlaVar "d")) (FmlaVar "e")) (FmlaVar "c"))
fmlaEx69 :: Fmla
fmlaEx69 = FmlaOr (FmlaNot (FmlaOr (FmlaVar "g") (FmlaOr (FmlaVar "h") (FmlaVar "j")))) (FmlaOr (FmlaOr (FmlaVar "f") (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "h") (FmlaOr (FmlaVar "g") (FmlaVar "h"))) (FmlaVar "h"))) (FmlaVar "e")) (FmlaVar "c"))) (FmlaOr (FmlaOr (FmlaConst True) (FmlaVar "e")) (FmlaOr (FmlaVar "g") (FmlaVar "g"))))
fmlaEx70 :: Fmla
fmlaEx70 = FmlaNot (FmlaOr (FmlaVar "b") (FmlaOr (FmlaConst True) (FmlaOr (FmlaOr (FmlaVar "b") (FmlaNot (FmlaVar "d"))) (FmlaVar "c"))))
fmlaEx71 :: Fmla
fmlaEx71 = FmlaOr (FmlaConst True) (FmlaNot (FmlaVar "e"))
fmlaEx72 :: Fmla
fmlaEx72 = FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "d") (FmlaOr (FmlaNot (FmlaOr (FmlaNot (FmlaOr (FmlaVar "e") (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaConst True)) (FmlaOr (FmlaVar "b") (FmlaVar "c"))) (FmlaNot (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "c")) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaNot (FmlaVar "a"))) (FmlaConst False)) (FmlaOr (FmlaVar "e") (FmlaVar "d")))))) (FmlaOr (FmlaNot (FmlaOr (FmlaNot (FmlaConst False)) (FmlaConst False))) (FmlaNot (FmlaOr (FmlaConst True) (FmlaNot (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaVar "e"))) (FmlaVar "c")) (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaNot (FmlaOr (FmlaVar "d") (FmlaOr (FmlaOr (FmlaVar "c") (FmlaNot (FmlaVar "a"))) (FmlaVar "c")))))) (FmlaVar "a")) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "a")) (FmlaVar "b"))))))))))))))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "a") (FmlaNot (FmlaVar "e"))) (FmlaOr (FmlaVar "e") (FmlaVar "c"))) (FmlaVar "b"))))) (FmlaNot (FmlaVar "a")))) (FmlaOr (FmlaVar "d") (FmlaVar "e"))))) (FmlaOr (FmlaOr (FmlaVar "c") (FmlaNot (FmlaVar "d"))) (FmlaVar "d"))) (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "d") (FmlaVar "d"))) (FmlaOr (FmlaVar "e") (FmlaVar "a")))) (FmlaVar "e")))))) (FmlaOr (FmlaVar "e") (FmlaVar "c"))) (FmlaNot (FmlaOr (FmlaVar "b") (FmlaVar "b")))) (FmlaConst False))) (FmlaConst True)) (FmlaVar "a"))) (FmlaNot (FmlaVar "d"))
fmlaEx73 :: Fmla
fmlaEx73 = FmlaNot (FmlaOr (FmlaVar "a") (FmlaNot (FmlaVar "h")))
fmlaEx74 :: Fmla
fmlaEx74 = FmlaNot (FmlaConst True)
fmlaEx75 :: Fmla
fmlaEx75 = FmlaNot (FmlaOr (FmlaNot (FmlaVar "e")) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "j")) (FmlaVar "i")) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "c") (FmlaOr (FmlaNot (FmlaNot (FmlaVar "d"))) (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaConst False) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "j") (FmlaVar "c")) (FmlaNot (FmlaVar "i"))) (FmlaVar "h"))) (FmlaVar "b")) (FmlaOr (FmlaNot (FmlaVar "f")) (FmlaNot (FmlaNot (FmlaVar "f")))))) (FmlaOr (FmlaVar "g") (FmlaNot (FmlaOr (FmlaOr (FmlaVar "g") (FmlaVar "f")) (FmlaOr (FmlaVar "c") (FmlaVar "h")))))) (FmlaVar "e")))))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "d")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "j") (FmlaVar "c")) (FmlaVar "i")) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "h") (FmlaConst False)) (FmlaConst True)) (FmlaVar "c"))) (FmlaNot (FmlaOr (FmlaVar "i") (FmlaVar "j"))))) (FmlaVar "a"))) (FmlaVar "g"))) (FmlaVar "j"))) (FmlaOr (FmlaVar "g") (FmlaVar "i"))) (FmlaOr (FmlaVar "h") (FmlaVar "g"))))
fmlaEx76 :: Fmla
fmlaEx76 = FmlaOr (FmlaVar "b") (FmlaConst True)
fmlaEx77 :: Fmla
fmlaEx77 = FmlaNot (FmlaOr (FmlaConst True) (FmlaOr (FmlaConst True) (FmlaVar "i")))
fmlaEx78 :: Fmla
fmlaEx78 = FmlaNot (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "d") (FmlaConst True)))
fmlaEx79 :: Fmla
fmlaEx79 = FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "f") (FmlaVar "c")) (FmlaConst False)) (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "e"))) (FmlaVar "j")) (FmlaNot (FmlaVar "b"))) (FmlaNot (FmlaVar "f"))))) (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaVar "g")) (FmlaNot (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "g") (FmlaOr (FmlaNot (FmlaVar "j")) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "j") (FmlaConst False)) (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "f") (FmlaNot (FmlaVar "a")))))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "i") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "g") (FmlaOr (FmlaNot (FmlaOr (FmlaVar "c") (FmlaOr (FmlaNot (FmlaOr (FmlaVar "j") (FmlaOr (FmlaNot (FmlaOr (FmlaVar "c") (FmlaConst True))) (FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "g")))))) (FmlaOr (FmlaVar "f") (FmlaNot (FmlaVar "c")))))) (FmlaNot (FmlaVar "h"))))) (FmlaConst True)) (FmlaOr (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaVar "b")) (FmlaOr (FmlaOr (FmlaNot (FmlaVar "f")) (FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "g")))) (FmlaVar "b")))) (FmlaConst True)) (FmlaOr (FmlaVar "d") (FmlaConst True)))) (FmlaVar "a")) (FmlaVar "d")))))) (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "i") (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "h") (FmlaNot (FmlaVar "a")))))) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "h") (FmlaOr (FmlaOr (FmlaVar "h") (FmlaVar "g")) (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaVar "f")))) (FmlaOr (FmlaVar "i") (FmlaNot (FmlaOr (FmlaVar "d") (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "d") (FmlaNot (FmlaOr (FmlaConst True) (FmlaOr (FmlaNot (FmlaOr (FmlaNot (FmlaVar "i")) (FmlaVar "f"))) (FmlaOr (FmlaVar "f") (FmlaOr (FmlaVar "j") (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "g")) (FmlaNot (FmlaVar "d"))))))))) (FmlaOr (FmlaOr (FmlaVar "f") (FmlaVar "d")) (FmlaOr (FmlaVar "g") (FmlaVar "c"))))) (FmlaOr (FmlaVar "f") (FmlaVar "i"))))))) (FmlaVar "d")) (FmlaVar "f"))) (FmlaOr (FmlaOr (FmlaNot (FmlaVar "h")) (FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaOr (FmlaOr (FmlaConst False) (FmlaVar "d")) (FmlaVar "c"))) (FmlaVar "h")) (FmlaVar "b")) (FmlaNot (FmlaVar "h")))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "j") (FmlaOr (FmlaNot (FmlaOr (FmlaVar "i") (FmlaVar "a"))) (FmlaVar "f"))) (FmlaOr (FmlaVar "h") (FmlaVar "j"))) (FmlaVar "g")))) (FmlaVar "c")))) (FmlaOr (FmlaVar "j") (FmlaOr (FmlaVar "g") (FmlaNot (FmlaVar "i")))))))) (FmlaVar "f")))) (FmlaVar "g")
fmlaEx80 :: Fmla
fmlaEx80 = FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "e") (FmlaVar "d"))) (FmlaOr (FmlaVar "b") (FmlaVar "d"))) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaConst True)) (FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "a")))) (FmlaNot (FmlaVar "e"))) (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaVar "d") (FmlaConst True)) (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaConst False))))) (FmlaVar "e"))) (FmlaVar "c")
fmlaEx81 :: Fmla
fmlaEx81 = FmlaNot (FmlaOr (FmlaOr (FmlaVar "d") (FmlaNot (FmlaVar "d"))) (FmlaNot (FmlaVar "i")))
fmlaEx82 :: Fmla
fmlaEx82 = FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "d")) (FmlaVar "d")) (FmlaOr (FmlaNot (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaOr (FmlaVar "d") (FmlaVar "b")))) (FmlaNot (FmlaVar "d")))
fmlaEx83 :: Fmla
fmlaEx83 = FmlaOr (FmlaNot (FmlaVar "a")) (FmlaOr (FmlaVar "j") (FmlaNot (FmlaVar "b")))
fmlaEx84 :: Fmla
fmlaEx84 = FmlaOr (FmlaVar "c") (FmlaOr (FmlaNot (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "b")) (FmlaVar "b"))))) (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "e")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "e")) (FmlaVar "d")) (FmlaVar "a")) (FmlaNot (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaVar "e")) (FmlaNot (FmlaOr (FmlaVar "e") (FmlaVar "c"))))))) (FmlaVar "d"))))
fmlaEx85 :: Fmla
fmlaEx85 = FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "g") (FmlaVar "j")) (FmlaVar "j"))) (FmlaOr (FmlaVar "g") (FmlaConst True))) (FmlaOr (FmlaVar "e") (FmlaVar "a"))
fmlaEx86 :: Fmla
fmlaEx86 = FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaConst False) (FmlaConst False)) (FmlaVar "c")) (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaConst False) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "a") (FmlaVar "b"))) (FmlaOr (FmlaOr (FmlaVar "d") (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "b")) (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "e")) (FmlaVar "e"))) (FmlaVar "b")) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "d") (FmlaNot (FmlaVar "b")))) (FmlaOr (FmlaVar "a") (FmlaVar "a"))) (FmlaVar "e"))) (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "d") (FmlaVar "a")))) (FmlaNot (FmlaVar "d")))) (FmlaVar "c")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaVar "b"))) (FmlaVar "c")) (FmlaConst False)) (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaVar "d"))) (FmlaOr (FmlaOr (FmlaConst True) (FmlaNot (FmlaVar "c"))) (FmlaOr (FmlaVar "a") (FmlaNot (FmlaVar "c"))))) (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaVar "b"))) (FmlaVar "d")) (FmlaVar "b")) (FmlaVar "d"))) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "d")) (FmlaOr (FmlaVar "d") (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "a")) (FmlaNot (FmlaVar "a"))))))) (FmlaNot (FmlaVar "b")))) (FmlaVar "e"))) (FmlaVar "e")) (FmlaOr (FmlaConst True) (FmlaVar "c"))) (FmlaVar "d")) (FmlaNot (FmlaVar "e"))) (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "e")) (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaNot (FmlaConst False)) (FmlaVar "a"))) (FmlaOr (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaOr (FmlaNot (FmlaNot (FmlaVar "b"))) (FmlaOr (FmlaOr (FmlaVar "d") (FmlaNot (FmlaVar "d"))) (FmlaVar "d")))) (FmlaVar "d"))))) (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "b") (FmlaOr (FmlaConst True) (FmlaVar "c")))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "c")) (FmlaVar "d")) (FmlaVar "b"))) (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaNot (FmlaVar "e")))) (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "c") (FmlaOr (FmlaNot (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "a") (FmlaOr (FmlaNot (FmlaVar "e")) (FmlaOr (FmlaVar "b") (FmlaVar "c"))))) (FmlaVar "c")) (FmlaVar "e")))) (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "d") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "b") (FmlaOr (FmlaNot (FmlaOr (FmlaVar "d") (FmlaVar "d"))) (FmlaVar "b")))) (FmlaVar "a")) (FmlaVar "c")) (FmlaVar "b"))) (FmlaOr (FmlaNot (FmlaOr (FmlaVar "e") (FmlaVar "e"))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "a")) (FmlaNot (FmlaOr (FmlaOr (FmlaConst False) (FmlaOr (FmlaVar "a") (FmlaVar "e"))) (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "d") (FmlaNot (FmlaVar "a"))))))) (FmlaVar "b")))) (FmlaVar "a")))))) (FmlaVar "d")))) (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "d")) (FmlaOr (FmlaVar "a") (FmlaNot (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "a")) (FmlaNot (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaVar "e")) (FmlaNot (FmlaConst True)))) (FmlaVar "a")) (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "a")) (FmlaVar "a")))) (FmlaNot (FmlaNot (FmlaNot (FmlaOr (FmlaNot (FmlaVar "e")) (FmlaOr (FmlaOr (FmlaConst False) (FmlaVar "a")) (FmlaNot (FmlaVar "e"))))))))))) (FmlaVar "a")))))) (FmlaNot (FmlaOr (FmlaVar "e") (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaConst False)) (FmlaVar "c")) (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaVar "e")))) (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "d") (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaNot (FmlaVar "e"))) (FmlaNot (FmlaNot (FmlaOr (FmlaConst False) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "a")) (FmlaVar "e"))))))) (FmlaNot (FmlaVar "e")))) (FmlaVar "d")) (FmlaOr (FmlaVar "c") (FmlaNot (FmlaVar "c")))))) (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "d") (FmlaNot (FmlaConst True))))) (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "a")) (FmlaVar "c"))) (FmlaNot (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "c") (FmlaVar "a"))))) (FmlaOr (FmlaConst False) (FmlaVar "c")))) (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "c")) (FmlaVar "b")) (FmlaVar "e")) (FmlaOr (FmlaVar "b") (FmlaVar "d"))))))))))))))))))) (FmlaOr (FmlaVar "e") (FmlaNot (FmlaVar "a"))))) (FmlaOr (FmlaVar "a") (FmlaNot (FmlaVar "c")))))) (FmlaVar "c"))) (FmlaOr (FmlaVar "c") (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "a")) (FmlaVar "d")) (FmlaVar "e"))) (FmlaOr (FmlaOr (FmlaVar "e") (FmlaNot (FmlaOr (FmlaVar "d") (FmlaNot (FmlaVar "a"))))) (FmlaOr (FmlaNot (FmlaVar "b")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "d") (FmlaVar "c")))) (FmlaOr (FmlaVar "d") (FmlaConst False)))) (FmlaVar "c"))) (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "e") (FmlaVar "e")))) (FmlaVar "d"))) (FmlaOr (FmlaNot (FmlaConst True)) (FmlaConst True))) (FmlaVar "a")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaConst True) (FmlaVar "a")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaConst False) (FmlaVar "d")) (FmlaOr (FmlaVar "e") (FmlaVar "c"))) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "d")) (FmlaVar "e")) (FmlaOr (FmlaOr (FmlaConst True) (FmlaOr (FmlaConst True) (FmlaVar "a"))) (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaConst False) (FmlaOr (FmlaConst False) (FmlaNot (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "c") (FmlaNot (FmlaVar "d")))))))) (FmlaVar "c"))))) (FmlaOr (FmlaOr (FmlaConst True) (FmlaOr (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "b")) (FmlaVar "d")))) (FmlaVar "c")) (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "e")) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaVar "e"))) (FmlaConst False)))))))) (FmlaOr (FmlaVar "e") (FmlaNot (FmlaVar "b"))))) (FmlaOr (FmlaVar "a") (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaConst False) (FmlaOr (FmlaVar "d") (FmlaOr (FmlaVar "b") (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "c")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "c") (FmlaVar "b")))) (FmlaOr (FmlaVar "e") (FmlaConst False))) (FmlaVar "e")) (FmlaVar "c")))))))) (FmlaOr (FmlaVar "e") (FmlaNot (FmlaOr (FmlaVar "a") (FmlaVar "a")))))) (FmlaVar "d"))))) (FmlaNot (FmlaConst False))) (FmlaNot (FmlaOr (FmlaOr (FmlaNot (FmlaVar "b")) (FmlaOr (FmlaVar "a") (FmlaOr (FmlaNot (FmlaVar "e")) (FmlaConst True)))) (FmlaOr (FmlaConst True) (FmlaVar "e"))))) (FmlaVar "d")) (FmlaOr (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaNot (FmlaOr (FmlaVar "e") (FmlaVar "c")))) (FmlaVar "d"))) (FmlaVar "b"))) (FmlaNot (FmlaVar "b"))) (FmlaVar "a")) (FmlaNot (FmlaNot (FmlaConst True))))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "b")) (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaConst False))) (FmlaOr (FmlaVar "c") (FmlaVar "e"))) (FmlaOr (FmlaVar "b") (FmlaOr (FmlaConst True) (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "b")) (FmlaOr (FmlaVar "e") (FmlaVar "e")))))))) (FmlaVar "a")) (FmlaVar "d")))) (FmlaNot (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaNot (FmlaOr (FmlaVar "b") (FmlaNot (FmlaVar "d")))) (FmlaVar "c"))) (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaVar "e") (FmlaVar "c")) (FmlaVar "b"))) (FmlaConst False)))))) (FmlaVar "c"))) (FmlaVar "c")) (FmlaNot (FmlaOr (FmlaConst True) (FmlaVar "b")))) (FmlaVar "b"))) (FmlaVar "d")) (FmlaConst False)))))))))) (FmlaVar "a")) (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "a") (FmlaVar "d"))))) (FmlaVar "d"))) (FmlaVar "e")
fmlaEx87 :: Fmla
fmlaEx87 = FmlaOr (FmlaNot (FmlaOr (FmlaConst True) (FmlaVar "j"))) (FmlaNot (FmlaVar "i"))
fmlaEx88 :: Fmla
fmlaEx88 = FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaConst False) (FmlaOr (FmlaOr (FmlaVar "d") (FmlaNot (FmlaVar "e"))) (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "e") (FmlaVar "d"))))) (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaConst False) (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "c")) (FmlaVar "d"))))) (FmlaVar "e"))))) (FmlaOr (FmlaOr (FmlaConst False) (FmlaOr (FmlaVar "e") (FmlaVar "b"))) (FmlaOr (FmlaOr (FmlaNot (FmlaNot (FmlaVar "e"))) (FmlaNot (FmlaVar "b"))) (FmlaVar "a")))) (FmlaOr (FmlaConst True) (FmlaVar "c"))
fmlaEx89 :: Fmla
fmlaEx89 = FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaConst False) (FmlaVar "d")) (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaVar "g") (FmlaVar "f")) (FmlaNot (FmlaVar "g"))))))) (FmlaVar "c")) (FmlaOr (FmlaVar "g") (FmlaVar "d"))) (FmlaNot (FmlaVar "b"))) (FmlaVar "j")) (FmlaVar "h")
fmlaEx90 :: Fmla
fmlaEx90 = FmlaOr (FmlaVar "e") (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaNot (FmlaConst False))) (FmlaVar "c")) (FmlaOr (FmlaNot (FmlaNot (FmlaNot (FmlaVar "e")))) (FmlaOr (FmlaVar "d") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "a") (FmlaVar "e"))) (FmlaOr (FmlaOr (FmlaVar "b") (FmlaVar "a")) (FmlaOr (FmlaVar "c") (FmlaNot (FmlaVar "a"))))) (FmlaVar "d"))))))
fmlaEx91 :: Fmla
fmlaEx91 = FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "a") (FmlaConst False)) (FmlaVar "h")) (FmlaOr (FmlaVar "g") (FmlaVar "c"))) (FmlaVar "c")) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "g") (FmlaVar "i")) (FmlaVar "b")) (FmlaVar "d"))
fmlaEx92 :: Fmla
fmlaEx92 = FmlaOr (FmlaOr (FmlaVar "b") (FmlaOr (FmlaVar "a") (FmlaNot (FmlaOr (FmlaVar "b") (FmlaNot (FmlaVar "e")))))) (FmlaVar "d")
fmlaEx93 :: Fmla
fmlaEx93 = FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "b")) (FmlaOr (FmlaConst True) (FmlaVar "f"))) (FmlaNot (FmlaVar "i"))) (FmlaVar "e")
fmlaEx94 :: Fmla
fmlaEx94 = FmlaNot (FmlaOr (FmlaNot (FmlaConst True)) (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "e") (FmlaVar "d"))) (FmlaVar "b")) (FmlaOr (FmlaConst False) (FmlaVar "e")))) (FmlaOr (FmlaNot (FmlaConst True)) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaNot (FmlaOr (FmlaVar "b") (FmlaNot (FmlaVar "a")))) (FmlaNot (FmlaOr (FmlaVar "a") (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaConst True) (FmlaVar "e")) (FmlaNot (FmlaConst False))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "a") (FmlaNot (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "b") (FmlaVar "b"))))) (FmlaVar "c")) (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "b") (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaConst False)) (FmlaOr (FmlaVar "b") (FmlaVar "a"))) (FmlaVar "c")) (FmlaVar "d"))) (FmlaVar "a")) (FmlaVar "b"))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaConst False)) (FmlaOr (FmlaNot (FmlaConst True)) (FmlaOr (FmlaVar "a") (FmlaOr (FmlaOr (FmlaVar "a") (FmlaNot (FmlaNot (FmlaOr (FmlaOr (FmlaVar "a") (FmlaNot (FmlaVar "b"))) (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "a") (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "c")) (FmlaVar "e")) (FmlaOr (FmlaNot (FmlaVar "e")) (FmlaOr (FmlaVar "c") (FmlaVar "e")))) (FmlaOr (FmlaVar "e") (FmlaOr (FmlaVar "d") (FmlaVar "d"))))))) (FmlaOr (FmlaVar "c") (FmlaVar "e"))))))) (FmlaOr (FmlaVar "c") (FmlaNot (FmlaVar "d"))))))) (FmlaVar "a")) (FmlaVar "c"))) (FmlaVar "d")) (FmlaVar "e"))) (FmlaNot (FmlaVar "d"))))) (FmlaVar "b")) (FmlaVar "c"))))))))) (FmlaOr (FmlaOr (FmlaOr (FmlaVar "d") (FmlaVar "c")) (FmlaConst True)) (FmlaVar "b"))) (FmlaOr (FmlaOr (FmlaVar "e") (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaOr (FmlaVar "c") (FmlaVar "a"))) (FmlaOr (FmlaVar "b") (FmlaVar "e"))) (FmlaOr (FmlaVar "b") (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "d") (FmlaNot (FmlaNot (FmlaVar "e")))) (FmlaVar "d")) (FmlaVar "c")) (FmlaVar "e"))))) (FmlaOr (FmlaVar "d") (FmlaOr (FmlaOr (FmlaVar "a") (FmlaVar "d")) (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "b")) (FmlaVar "e"))))) (FmlaNot (FmlaNot (FmlaVar "d"))))) (FmlaVar "d"))))))
fmlaEx95 :: Fmla
fmlaEx95 = FmlaNot (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "e") (FmlaVar "a"))) (FmlaVar "d"))
fmlaEx96 :: Fmla
fmlaEx96 = FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaVar "f") (FmlaVar "i")) (FmlaOr (FmlaNot (FmlaVar "c")) (FmlaVar "b"))) (FmlaVar "d"))
fmlaEx97 :: Fmla
fmlaEx97 = FmlaOr (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaVar "e")) (FmlaVar "e")
fmlaEx98 :: Fmla
fmlaEx98 = FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaConst True)) (FmlaOr (FmlaConst True) (FmlaNot (FmlaNot (FmlaVar "f"))))) (FmlaOr (FmlaOr (FmlaVar "c") (FmlaOr (FmlaVar "a") (FmlaOr (FmlaVar "a") (FmlaNot (FmlaOr (FmlaVar "b") (FmlaNot (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaOr (FmlaNot (FmlaVar "d")) (FmlaVar "g")) (FmlaOr (FmlaVar "b") (FmlaVar "b"))) (FmlaOr (FmlaVar "g") (FmlaVar "b"))) (FmlaNot (FmlaVar "g"))) (FmlaVar "j")))))))) (FmlaVar "e"))) (FmlaVar "h")
fmlaEx99 :: Fmla
fmlaEx99 = FmlaOr (FmlaOr (FmlaVar "a") (FmlaNot (FmlaVar "b"))) (FmlaNot (FmlaVar "e"))
fmlaEx100 :: Fmla
fmlaEx100 = FmlaOr (FmlaOr (FmlaOr (FmlaVar "c") (FmlaVar "d")) (FmlaOr (FmlaNot (FmlaOr (FmlaNot (FmlaVar "a")) (FmlaOr (FmlaVar "i") (FmlaOr (FmlaNot (FmlaNot (FmlaVar "g"))) (FmlaVar "g"))))) (FmlaVar "c"))) (FmlaVar "f")
fmlaExs :: [(String,Fmla)]
fmlaExs = [("fmlaEx01",fmlaEx01),("fmlaEx02",fmlaEx02),("fmlaEx03",fmlaEx03),("fmlaEx04",fmlaEx04),("fmlaEx05",fmlaEx05),("fmlaEx06",fmlaEx06),("fmlaEx07",fmlaEx07),("fmlaEx08",fmlaEx08),("fmlaEx09",fmlaEx09),("fmlaEx10",fmlaEx10),("fmlaEx11",fmlaEx11),("fmlaEx12",fmlaEx12),("fmlaEx13",fmlaEx13),("fmlaEx14",fmlaEx14),("fmlaEx15",fmlaEx15),("fmlaEx16",fmlaEx16),("fmlaEx17",fmlaEx17),("fmlaEx18",fmlaEx18),("fmlaEx19",fmlaEx19),("fmlaEx20",fmlaEx20),("fmlaEx21",fmlaEx21),("fmlaEx22",fmlaEx22),("fmlaEx23",fmlaEx23),("fmlaEx24",fmlaEx24),("fmlaEx25",fmlaEx25),("fmlaEx26",fmlaEx26),("fmlaEx27",fmlaEx27),("fmlaEx28",fmlaEx28),("fmlaEx29",fmlaEx29),("fmlaEx30",fmlaEx30),("fmlaEx31",fmlaEx31),("fmlaEx32",fmlaEx32),("fmlaEx33",fmlaEx33),("fmlaEx34",fmlaEx34),("fmlaEx35",fmlaEx35),("fmlaEx36",fmlaEx36),("fmlaEx37",fmlaEx37),("fmlaEx38",fmlaEx38),("fmlaEx39",fmlaEx39),("fmlaEx40",fmlaEx40),("fmlaEx41",fmlaEx41),("fmlaEx42",fmlaEx42),("fmlaEx43",fmlaEx43),("fmlaEx44",fmlaEx44),("fmlaEx45",fmlaEx45),("fmlaEx46",fmlaEx46),("fmlaEx47",fmlaEx47),("fmlaEx48",fmlaEx48),("fmlaEx49",fmlaEx49),("fmlaEx50",fmlaEx50),("fmlaEx51",fmlaEx51),("fmlaEx52",fmlaEx52),("fmlaEx53",fmlaEx53),("fmlaEx54",fmlaEx54),("fmlaEx55",fmlaEx55),("fmlaEx56",fmlaEx56),("fmlaEx57",fmlaEx57),("fmlaEx58",fmlaEx58),("fmlaEx59",fmlaEx59),("fmlaEx60",fmlaEx60),("fmlaEx61",fmlaEx61),("fmlaEx62",fmlaEx62),("fmlaEx63",fmlaEx63),("fmlaEx64",fmlaEx64),("fmlaEx65",fmlaEx65),("fmlaEx66",fmlaEx66),("fmlaEx67",fmlaEx67),("fmlaEx68",fmlaEx68),("fmlaEx69",fmlaEx69),("fmlaEx70",fmlaEx70),("fmlaEx71",fmlaEx71),("fmlaEx72",fmlaEx72),("fmlaEx73",fmlaEx73),("fmlaEx74",fmlaEx74),("fmlaEx75",fmlaEx75),("fmlaEx76",fmlaEx76),("fmlaEx77",fmlaEx77),("fmlaEx78",fmlaEx78),("fmlaEx79",fmlaEx79),("fmlaEx80",fmlaEx80),("fmlaEx81",fmlaEx81),("fmlaEx82",fmlaEx82),("fmlaEx83",fmlaEx83),("fmlaEx84",fmlaEx84),("fmlaEx85",fmlaEx85),("fmlaEx86",fmlaEx86),("fmlaEx87",fmlaEx87),("fmlaEx88",fmlaEx88),("fmlaEx89",fmlaEx89),("fmlaEx90",fmlaEx90),("fmlaEx91",fmlaEx91),("fmlaEx92",fmlaEx92),("fmlaEx93",fmlaEx93),("fmlaEx94",fmlaEx94),("fmlaEx95",fmlaEx95),("fmlaEx96",fmlaEx96),("fmlaEx97",fmlaEx97),("fmlaEx98",fmlaEx98),("fmlaEx99",fmlaEx99),("fmlaEx100",fmlaEx100)]

nnfEx01 :: NNF
nnfEx01 = NNFOr []
nnfEx02 :: NNF
nnfEx02 = NNFAnd []
nnfEx03 :: NNF
nnfEx03 = NNFLit (True,"a")
nnfEx04 :: NNF
nnfEx04 = NNFLit (False,"a")
nnfEx05 :: NNF
nnfEx05 = NNFOr [NNFAnd [],NNFOr [],NNFOr [NNFLit (False,"d"),NNFLit (False,"b"),NNFLit (False,"a"),NNFAnd [NNFLit (True,"a"),NNFLit (True,"a"),NNFLit (True,"a")]]]
nnfEx06 :: NNF
nnfEx06 = NNFAnd [NNFLit (False,"f"),NNFLit (False,"c"),NNFLit (False,"a"),NNFAnd [NNFLit (False,"c"),NNFLit (True,"f"),NNFLit (False,"i")]]
nnfEx07 :: NNF
nnfEx07 = NNFAnd [NNFLit (True,"d"),NNFLit (True,"e")]
nnfEx08 :: NNF
nnfEx08 = NNFAnd [NNFOr [NNFLit (False,"i"),NNFLit (False,"c"),NNFLit (True,"d"),NNFAnd [NNFLit (True,"i")]]]
nnfEx09 :: NNF
nnfEx09 = NNFOr [NNFLit (False,"d"),NNFLit (False,"b")]
nnfEx10 :: NNF
nnfEx10 = NNFOr [NNFLit (False,"d")]
nnfEx11 :: NNF
nnfEx11 = NNFOr [NNFLit (True,"a"),NNFLit (True,"a"),NNFOr [NNFOr [NNFLit (False,"e"),NNFLit (True,"c"),NNFLit (False,"c"),NNFLit (True,"c"),NNFLit (True,"b")],NNFLit (True,"c"),NNFLit (False,"e")],NNFLit (False,"b"),NNFLit (False,"a")]
nnfEx12 :: NNF
nnfEx12 = NNFAnd [NNFLit (False,"e"),NNFLit (True,"c"),NNFLit (True,"e"),NNFLit (False,"i")]
nnfEx13 :: NNF
nnfEx13 = NNFOr [NNFLit (False,"j"),NNFOr [],NNFLit (True,"h"),NNFLit (False,"b")]
nnfEx14 :: NNF
nnfEx14 = NNFAnd [NNFLit (False,"e"),NNFAnd [NNFLit (True,"c"),NNFLit (True,"d"),NNFLit (True,"c"),NNFLit (False,"d")]]
nnfEx15 :: NNF
nnfEx15 = NNFAnd [NNFLit (False,"b"),NNFAnd [NNFLit (False,"e"),NNFLit (False,"b")],NNFLit (True,"b"),NNFOr [NNFLit (True,"g"),NNFLit (False,"j"),NNFLit (False,"e"),NNFLit (False,"d"),NNFLit (True,"e")]]
nnfEx16 :: NNF
nnfEx16 = NNFOr [NNFOr [NNFOr [NNFAnd [NNFLit (False,"b")],NNFLit (False,"d")],NNFLit (True,"a")]]
nnfEx17 :: NNF
nnfEx17 = NNFOr [NNFLit (True,"a"),NNFLit (True,"h")]
nnfEx18 :: NNF
nnfEx18 = NNFOr [NNFLit (True,"a"),NNFLit (True,"a"),NNFLit (False,"d"),NNFLit (False,"c"),NNFLit (True,"e")]
nnfEx19 :: NNF
nnfEx19 = NNFOr [NNFLit (True,"h"),NNFLit (False,"h"),NNFLit (True,"h"),NNFLit (False,"d"),NNFLit (False,"g")]
nnfEx20 :: NNF
nnfEx20 = NNFAnd [NNFLit (True,"c")]
nnfEx21 :: NNF
nnfEx21 = NNFAnd [NNFLit (False,"i"),NNFLit (False,"d")]
nnfEx22 :: NNF
nnfEx22 = NNFAnd [NNFOr [NNFOr [],NNFAnd [NNFLit (False,"a"),NNFLit (True,"c"),NNFLit (False,"d"),NNFLit (False,"e"),NNFLit (False,"e")],NNFLit (True,"a"),NNFLit (True,"e"),NNFLit (True,"e")],NNFLit (False,"d")]
nnfEx23 :: NNF
nnfEx23 = NNFAnd [NNFOr [NNFLit (True,"e")],NNFLit (False,"b")]
nnfEx24 :: NNF
nnfEx24 = NNFOr [NNFLit (True,"d"),NNFLit (True,"f"),NNFOr [NNFLit (False,"f"),NNFOr [NNFLit (False,"b"),NNFLit (False,"f"),NNFLit (True,"b")],NNFLit (True,"d"),NNFLit (True,"c")]]
nnfEx25 :: NNF
nnfEx25 = NNFAnd [NNFAnd [NNFLit (False,"e"),NNFLit (False,"b"),NNFLit (True,"a")],NNFLit (True,"a"),NNFLit (False,"c")]
nnfEx26 :: NNF
nnfEx26 = NNFAnd [NNFOr [],NNFLit (False,"d")]
nnfEx27 :: NNF
nnfEx27 = NNFOr [NNFLit (False,"c"),NNFLit (False,"d"),NNFOr [NNFAnd [],NNFLit (False,"g"),NNFLit (True,"a")],NNFLit (True,"e")]
nnfEx28 :: NNF
nnfEx28 = NNFAnd [NNFLit (False,"a")]
nnfEx29 :: NNF
nnfEx29 = NNFOr [NNFLit (True,"d"),NNFLit (False,"h"),NNFLit (False,"c"),NNFLit (False,"c"),NNFLit (False,"h")]
nnfEx30 :: NNF
nnfEx30 = NNFOr [NNFLit (False,"d"),NNFLit (False,"e"),NNFLit (True,"a"),NNFLit (False,"a"),NNFLit (False,"e")]
nnfEx31 :: NNF
nnfEx31 = NNFOr [NNFLit (False,"b"),NNFLit (False,"f"),NNFLit (True,"g"),NNFAnd [NNFLit (False,"d"),NNFLit (False,"c"),NNFLit (False,"h"),NNFOr [NNFLit (False,"d"),NNFLit (True,"e"),NNFLit (True,"g"),NNFLit (True,"c")]]]
nnfEx32 :: NNF
nnfEx32 = NNFOr [NNFLit (False,"a"),NNFLit (False,"d"),NNFLit (False,"a"),NNFLit (False,"e"),NNFLit (True,"a")]
nnfEx33 :: NNF
nnfEx33 = NNFAnd [NNFLit (False,"d"),NNFLit (False,"i"),NNFLit (True,"b"),NNFOr [NNFLit (False,"i"),NNFAnd [NNFLit (False,"d"),NNFAnd [NNFLit (True,"c"),NNFLit (False,"i")],NNFLit (True,"i")],NNFLit (False,"h"),NNFOr [NNFOr [NNFLit (True,"f"),NNFLit (False,"j"),NNFLit (False,"b"),NNFOr [NNFLit (True,"d"),NNFLit (True,"f"),NNFLit (False,"g"),NNFLit (False,"i")],NNFLit (True,"d")],NNFLit (True,"j")]]]
nnfEx34 :: NNF
nnfEx34 = NNFOr [NNFLit (False,"d"),NNFLit (True,"c"),NNFLit (False,"d"),NNFOr [NNFLit (True,"d")]]
nnfEx35 :: NNF
nnfEx35 = NNFOr [NNFLit (True,"a"),NNFLit (False,"j"),NNFLit (True,"c")]
nnfEx36 :: NNF
nnfEx36 = NNFOr [NNFLit (True,"c"),NNFLit (True,"d"),NNFLit (True,"b"),NNFLit (False,"e"),NNFLit (True,"b")]
nnfEx37 :: NNF
nnfEx37 = NNFAnd [NNFLit (False,"f"),NNFOr [NNFAnd []],NNFLit (False,"i")]
nnfEx38 :: NNF
nnfEx38 = NNFAnd [NNFLit (False,"b"),NNFOr [],NNFAnd [NNFAnd [NNFLit (True,"d"),NNFLit (True,"b"),NNFLit (True,"c"),NNFOr [NNFLit (True,"b"),NNFLit (False,"a"),NNFLit (True,"e"),NNFLit (True,"c")],NNFLit (True,"e")],NNFAnd [NNFLit (False,"d"),NNFLit (True,"c")],NNFLit (True,"a"),NNFLit (False,"b")]]
nnfEx39 :: NNF
nnfEx39 = NNFOr [NNFLit (True,"a"),NNFLit (True,"g"),NNFLit (False,"f"),NNFLit (False,"g")]
nnfEx40 :: NNF
nnfEx40 = NNFOr [NNFLit (False,"c"),NNFLit (False,"b"),NNFLit (True,"c"),NNFLit (True,"e"),NNFLit (True,"e")]
nnfEx41 :: NNF
nnfEx41 = NNFAnd [NNFLit (True,"i"),NNFLit (False,"h")]
nnfEx42 :: NNF
nnfEx42 = NNFAnd [NNFLit (True,"g"),NNFOr [],NNFLit (True,"b")]
nnfEx43 :: NNF
nnfEx43 = NNFAnd [NNFLit (True,"c"),NNFLit (False,"a"),NNFLit (True,"c"),NNFLit (True,"e"),NNFOr [NNFLit (False,"a"),NNFLit (True,"a"),NNFLit (False,"b"),NNFLit (False,"b"),NNFOr [NNFLit (True,"b"),NNFLit (False,"b")]]]
nnfEx44 :: NNF
nnfEx44 = NNFOr [NNFLit (False,"e"),NNFLit (False,"a"),NNFLit (True,"e")]
nnfEx45 :: NNF
nnfEx45 = NNFOr [NNFLit (False,"e"),NNFLit (False,"b"),NNFOr [NNFLit (False,"j"),NNFLit (True,"i")]]
nnfEx46 :: NNF
nnfEx46 = NNFAnd [NNFLit (False,"a"),NNFLit (True,"b")]
nnfEx47 :: NNF
nnfEx47 = NNFAnd [NNFLit (True,"f"),NNFLit (False,"g"),NNFOr [],NNFLit (False,"d"),NNFLit (True,"d")]
nnfEx48 :: NNF
nnfEx48 = NNFOr [NNFLit (True,"a")]
nnfEx49 :: NNF
nnfEx49 = NNFOr [NNFAnd [NNFLit (False,"h"),NNFLit (True,"i"),NNFAnd [NNFOr [NNFLit (True,"c"),NNFLit (True,"j"),NNFLit (False,"a"),NNFOr [NNFOr [],NNFLit (True,"i"),NNFLit (True,"e"),NNFLit (False,"c"),NNFLit (True,"e")],NNFLit (True,"c")],NNFAnd [NNFOr [NNFLit (False,"b"),NNFLit (False,"d"),NNFLit (True,"f")],NNFLit (False,"a"),NNFLit (True,"b")]],NNFOr [NNFLit (False,"f"),NNFLit (False,"h")]],NNFLit (False,"j"),NNFLit (True,"j")]
nnfEx50 :: NNF
nnfEx50 = NNFOr [NNFLit (False,"c"),NNFAnd [NNFLit (True,"b"),NNFOr [NNFLit (True,"e"),NNFLit (True,"e"),NNFLit (False,"e"),NNFOr [NNFLit (False,"a"),NNFLit (True,"c"),NNFLit (True,"a"),NNFOr [NNFOr [NNFLit (True,"c"),NNFLit (True,"a"),NNFLit (True,"a"),NNFLit (False,"e"),NNFLit (True,"a")],NNFOr [NNFLit (False,"e"),NNFLit (True,"d"),NNFLit (False,"b")],NNFLit (True,"e"),NNFLit (True,"d"),NNFLit (True,"a")]]],NNFLit (False,"c"),NNFAnd [NNFLit (False,"c"),NNFLit (False,"b")]],NNFLit (True,"b"),NNFOr [NNFLit (False,"a"),NNFLit (True,"a"),NNFOr [NNFLit (False,"e"),NNFLit (False,"d"),NNFLit (False,"c"),NNFLit (False,"e"),NNFLit (True,"c")],NNFLit (True,"b"),NNFLit (True,"d")],NNFLit (True,"e")]
nnfEx51 :: NNF
nnfEx51 = NNFOr [NNFLit (False,"j"),NNFLit (False,"g"),NNFLit (False,"g"),NNFOr [NNFLit (False,"a"),NNFOr [NNFLit (True,"h")],NNFLit (False,"b"),NNFLit (True,"a")]]
nnfEx52 :: NNF
nnfEx52 = NNFOr [NNFAnd [NNFOr [NNFLit (False,"b"),NNFOr []],NNFLit (True,"a"),NNFLit (False,"e")],NNFLit (False,"a")]
nnfEx53 :: NNF
nnfEx53 = NNFAnd [NNFLit (True,"c"),NNFLit (True,"b"),NNFLit (False,"b"),NNFLit (False,"j")]
nnfEx54 :: NNF
nnfEx54 = NNFAnd [NNFLit (True,"c"),NNFLit (True,"a"),NNFLit (True,"c")]
nnfEx55 :: NNF
nnfEx55 = NNFOr [NNFAnd [],NNFLit (True,"c"),NNFLit (True,"c")]
nnfEx56 :: NNF
nnfEx56 = NNFOr [NNFLit (False,"b"),NNFLit (False,"a"),NNFLit (True,"b"),NNFLit (True,"b"),NNFLit (False,"b")]
nnfEx57 :: NNF
nnfEx57 = NNFAnd [NNFLit (False,"d"),NNFLit (True,"a"),NNFLit (False,"d"),NNFLit (True,"d")]
nnfEx58 :: NNF
nnfEx58 = NNFAnd [NNFOr [NNFLit (True,"a"),NNFLit (False,"g"),NNFAnd [NNFAnd [NNFLit (True,"g"),NNFLit (True,"h"),NNFLit (True,"b"),NNFLit (False,"b"),NNFLit (False,"d")],NNFLit (False,"a")],NNFLit (True,"j")],NNFLit (True,"h")]
nnfEx59 :: NNF
nnfEx59 = NNFOr [NNFAnd []]
nnfEx60 :: NNF
nnfEx60 = NNFOr [NNFLit (True,"g"),NNFLit (True,"i"),NNFLit (False,"j")]
nnfEx61 :: NNF
nnfEx61 = NNFOr [NNFOr [NNFOr [NNFLit (True,"d"),NNFLit (False,"a"),NNFLit (True,"c"),NNFLit (True,"c")],NNFLit (True,"b"),NNFLit (True,"c")],NNFLit (False,"b"),NNFLit (False,"d"),NNFOr [NNFLit (True,"d"),NNFOr [NNFLit (True,"d"),NNFLit (False,"b"),NNFLit (True,"b")],NNFAnd [NNFAnd [NNFLit (True,"a"),NNFLit (False,"c"),NNFLit (True,"a"),NNFLit (False,"d")],NNFLit (False,"a"),NNFLit (False,"d"),NNFAnd [NNFLit (False,"c"),NNFLit (True,"d")],NNFLit (False,"d")],NNFLit (True,"a")]]
nnfEx62 :: NNF
nnfEx62 = NNFAnd [NNFOr [NNFLit (True,"j"),NNFLit (True,"d"),NNFLit (False,"a"),NNFLit (False,"a")],NNFLit (False,"h"),NNFLit (True,"a"),NNFLit (True,"j"),NNFLit (True,"b")]
nnfEx63 :: NNF
nnfEx63 = NNFOr [NNFOr [NNFLit (True,"b")],NNFOr [NNFLit (True,"c")],NNFLit (True,"c")]
nnfEx64 :: NNF
nnfEx64 = NNFOr [NNFLit (True,"d"),NNFLit (True,"i"),NNFLit (False,"f"),NNFLit (True,"d"),NNFLit (False,"h")]
nnfEx65 :: NNF
nnfEx65 = NNFOr [NNFLit (False,"f"),NNFLit (True,"g"),NNFLit (True,"a"),NNFLit (False,"c")]
nnfEx66 :: NNF
nnfEx66 = NNFOr [NNFLit (False,"a"),NNFLit (True,"d")]
nnfEx67 :: NNF
nnfEx67 = NNFAnd [NNFLit (False,"b"),NNFOr [NNFOr [NNFLit (True,"d"),NNFLit (True,"j")],NNFLit (False,"c"),NNFLit (False,"h")]]
nnfEx68 :: NNF
nnfEx68 = NNFOr [NNFLit (False,"e"),NNFLit (True,"c"),NNFLit (False,"e"),NNFLit (False,"e"),NNFAnd [NNFLit (True,"a"),NNFLit (False,"e"),NNFLit (True,"b"),NNFLit (True,"d"),NNFLit (True,"b")]]
nnfEx69 :: NNF
nnfEx69 = NNFAnd [NNFLit (True,"j"),NNFOr [NNFLit (False,"i"),NNFLit (True,"f")],NNFLit (False,"c")]
nnfEx70 :: NNF
nnfEx70 = NNFOr [NNFLit (True,"a"),NNFLit (True,"b"),NNFLit (True,"e"),NNFLit (True,"a")]
nnfEx71 :: NNF
nnfEx71 = NNFOr [NNFLit (False,"c"),NNFLit (False,"a"),NNFAnd [],NNFOr [NNFLit (False,"h"),NNFLit (True,"j"),NNFLit (True,"b"),NNFOr [NNFLit (False,"d")],NNFLit (False,"c")]]
nnfEx72 :: NNF
nnfEx72 = NNFOr [NNFLit (True,"a"),NNFLit (False,"e"),NNFLit (False,"c"),NNFLit (True,"a"),NNFLit (True,"e")]
nnfEx73 :: NNF
nnfEx73 = NNFAnd [NNFOr [],NNFLit (True,"g"),NNFLit (False,"a"),NNFLit (False,"f"),NNFLit (True,"d")]
nnfEx74 :: NNF
nnfEx74 = NNFAnd [NNFAnd [NNFLit (True,"a")],NNFLit (False,"c"),NNFLit (False,"a"),NNFLit (True,"a"),NNFLit (False,"c")]
nnfEx75 :: NNF
nnfEx75 = NNFAnd [NNFOr [NNFLit (False,"a"),NNFOr [NNFLit (False,"d"),NNFLit (False,"a"),NNFAnd [NNFLit (True,"b")],NNFLit (False,"a")]],NNFAnd [],NNFLit (False,"c"),NNFLit (True,"b")]
nnfEx76 :: NNF
nnfEx76 = NNFAnd [NNFAnd [NNFLit (False,"e"),NNFLit (False,"a")],NNFLit (False,"f"),NNFLit (False,"d"),NNFLit (False,"g")]
nnfEx77 :: NNF
nnfEx77 = NNFAnd [NNFLit (True,"d"),NNFLit (False,"e"),NNFOr [NNFLit (False,"e"),NNFLit (False,"b"),NNFAnd [NNFLit (False,"d"),NNFLit (True,"a"),NNFLit (True,"e"),NNFLit (False,"c")],NNFLit (True,"b")],NNFOr [NNFLit (True,"e"),NNFLit (False,"e"),NNFOr [NNFLit (True,"a"),NNFLit (False,"b"),NNFLit (True,"b"),NNFLit (False,"c")]],NNFLit (False,"d")]
nnfEx78 :: NNF
nnfEx78 = NNFAnd [NNFLit (False,"e"),NNFLit (False,"b"),NNFAnd [NNFAnd [NNFAnd [NNFLit (True,"i"),NNFLit (False,"e"),NNFLit (True,"d"),NNFAnd [NNFLit (True,"c"),NNFLit (True,"e"),NNFAnd [NNFLit (False,"a")]]],NNFLit (False,"a"),NNFLit (False,"c"),NNFLit (False,"g")],NNFLit (False,"d")]]
nnfEx79 :: NNF
nnfEx79 = NNFOr [NNFLit (False,"e"),NNFLit (True,"b"),NNFLit (True,"c"),NNFLit (False,"b"),NNFLit (False,"b")]
nnfEx80 :: NNF
nnfEx80 = NNFAnd [NNFLit (True,"a"),NNFLit (True,"i"),NNFLit (False,"e"),NNFLit (True,"e")]
nnfEx81 :: NNF
nnfEx81 = NNFAnd [NNFLit (True,"d"),NNFLit (True,"a"),NNFLit (False,"d")]
nnfEx82 :: NNF
nnfEx82 = NNFOr [NNFOr [NNFAnd [NNFLit (True,"b"),NNFLit (True,"e"),NNFAnd [NNFLit (True,"d"),NNFLit (True,"b"),NNFLit (True,"d"),NNFLit (False,"a")]],NNFLit (False,"d"),NNFLit (True,"b")],NNFLit (False,"c"),NNFAnd [NNFLit (True,"e"),NNFAnd [NNFLit (False,"a"),NNFLit (False,"d"),NNFLit (True,"d"),NNFOr []],NNFAnd [NNFLit (False,"d")],NNFLit (True,"c")],NNFLit (True,"c"),NNFLit (True,"e")]
nnfEx83 :: NNF
nnfEx83 = NNFOr [NNFAnd [NNFLit (False,"d"),NNFLit (True,"e")],NNFLit (False,"c")]
nnfEx84 :: NNF
nnfEx84 = NNFOr [NNFOr [NNFLit (True,"e"),NNFLit (False,"d"),NNFLit (True,"d"),NNFLit (True,"a"),NNFLit (True,"c")],NNFLit (True,"b"),NNFOr [NNFLit (True,"d"),NNFLit (True,"c")],NNFLit (False,"c"),NNFLit (True,"b")]
nnfEx85 :: NNF
nnfEx85 = NNFAnd [NNFLit (True,"i"),NNFLit (True,"h"),NNFLit (True,"h"),NNFOr [NNFOr [NNFLit (True,"c"),NNFLit (True,"i"),NNFLit (False,"b"),NNFLit (True,"e"),NNFLit (True,"f")],NNFLit (False,"a"),NNFLit (True,"e"),NNFLit (True,"g"),NNFLit (True,"f")]]
nnfEx86 :: NNF
nnfEx86 = NNFAnd [NNFLit (True,"a"),NNFOr [NNFLit (False,"b")],NNFLit (False,"a")]
nnfEx87 :: NNF
nnfEx87 = NNFOr [NNFLit (True,"a"),NNFLit (True,"j"),NNFLit (True,"e")]
nnfEx88 :: NNF
nnfEx88 = NNFAnd [NNFLit (False,"c"),NNFLit (False,"b"),NNFLit (False,"d"),NNFAnd [],NNFLit (False,"a")]
nnfEx89 :: NNF
nnfEx89 = NNFOr [NNFLit (True,"e"),NNFLit (False,"b")]
nnfEx90 :: NNF
nnfEx90 = NNFOr [NNFLit (True,"a"),NNFLit (True,"a"),NNFOr [NNFOr [NNFLit (False,"a"),NNFAnd [NNFLit (True,"i"),NNFLit (True,"h")],NNFLit (True,"c"),NNFLit (True,"e"),NNFLit (False,"d")],NNFLit (True,"j"),NNFLit (True,"a"),NNFLit (True,"e"),NNFOr [NNFOr [NNFLit (True,"c"),NNFLit (False,"h"),NNFLit (False,"c")],NNFLit (False,"d"),NNFLit (True,"f"),NNFLit (False,"f")]],NNFOr [NNFLit (True,"g")]]
nnfEx91 :: NNF
nnfEx91 = NNFAnd [NNFOr [NNFLit (True,"c"),NNFLit (False,"b"),NNFLit (True,"b"),NNFLit (True,"b"),NNFLit (False,"b")],NNFOr [NNFLit (True,"a"),NNFLit (False,"c"),NNFLit (False,"e"),NNFLit (False,"d")],NNFLit (False,"a"),NNFLit (True,"e")]
nnfEx92 :: NNF
nnfEx92 = NNFAnd [NNFLit (True,"f"),NNFLit (False,"b"),NNFOr [NNFLit (False,"d"),NNFLit (False,"h")]]
nnfEx93 :: NNF
nnfEx93 = NNFAnd [NNFLit (True,"b"),NNFLit (False,"d"),NNFLit (True,"a"),NNFLit (False,"b")]
nnfEx94 :: NNF
nnfEx94 = NNFAnd [NNFAnd [NNFLit (True,"e"),NNFLit (True,"j")],NNFLit (False,"h"),NNFLit (False,"g"),NNFLit (False,"a")]
nnfEx95 :: NNF
nnfEx95 = NNFAnd [NNFLit (False,"c"),NNFLit (False,"c"),NNFOr [NNFLit (False,"e")]]
nnfEx96 :: NNF
nnfEx96 = NNFAnd [NNFLit (False,"g"),NNFLit (False,"b"),NNFLit (True,"i"),NNFAnd [NNFOr [NNFLit (False,"i")]]]
nnfEx97 :: NNF
nnfEx97 = NNFOr [NNFOr [NNFLit (False,"d"),NNFLit (False,"e"),NNFLit (True,"d")],NNFLit (False,"b"),NNFLit (False,"e"),NNFOr [NNFAnd [NNFOr [NNFLit (True,"b")],NNFOr [NNFLit (True,"b"),NNFLit (True,"b"),NNFLit (True,"c")],NNFLit (False,"d")]],NNFLit (True,"a")]
nnfEx98 :: NNF
nnfEx98 = NNFAnd [NNFAnd [NNFAnd [NNFLit (False,"c"),NNFLit (False,"c"),NNFAnd [NNFAnd [NNFOr [NNFLit (False,"f"),NNFLit (False,"b"),NNFLit (False,"j")],NNFLit (False,"c")]],NNFLit (False,"e"),NNFLit (False,"i")],NNFLit (True,"i"),NNFAnd [NNFOr [NNFLit (True,"a"),NNFLit (True,"g")],NNFLit (True,"e"),NNFLit (True,"c"),NNFOr [NNFAnd [NNFOr [NNFLit (False,"e"),NNFOr [],NNFLit (False,"h"),NNFLit (True,"b"),NNFLit (True,"e")],NNFOr [NNFLit (True,"c")],NNFLit (False,"b"),NNFLit (False,"c"),NNFLit (False,"j")],NNFLit (False,"a"),NNFLit (True,"i")],NNFLit (False,"f")]]]
nnfEx99 :: NNF
nnfEx99 = NNFAnd [NNFLit (False,"c"),NNFLit (True,"e"),NNFLit (True,"b"),NNFLit (False,"d"),NNFLit (True,"d")]
nnfEx100 :: NNF
nnfEx100 = NNFOr [NNFOr [],NNFLit (True,"j"),NNFOr [NNFLit (True,"j"),NNFLit (False,"g")],NNFLit (True,"j"),NNFLit (False,"b")]
nnfExs :: [(String,NNF)]
nnfExs = [("nnfEx01",nnfEx01),("nnfEx02",nnfEx02),("nnfEx03",nnfEx03),("nnfEx04",nnfEx04),("nnfEx05",nnfEx05),("nnfEx06",nnfEx06),("nnfEx07",nnfEx07),("nnfEx08",nnfEx08),("nnfEx09",nnfEx09),("nnfEx10",nnfEx10),("nnfEx11",nnfEx11),("nnfEx12",nnfEx12),("nnfEx13",nnfEx13),("nnfEx14",nnfEx14),("nnfEx15",nnfEx15),("nnfEx16",nnfEx16),("nnfEx17",nnfEx17),("nnfEx18",nnfEx18),("nnfEx19",nnfEx19),("nnfEx20",nnfEx20),("nnfEx21",nnfEx21),("nnfEx22",nnfEx22),("nnfEx23",nnfEx23),("nnfEx24",nnfEx24),("nnfEx25",nnfEx25),("nnfEx26",nnfEx26),("nnfEx27",nnfEx27),("nnfEx28",nnfEx28),("nnfEx29",nnfEx29),("nnfEx30",nnfEx30),("nnfEx31",nnfEx31),("nnfEx32",nnfEx32),("nnfEx33",nnfEx33),("nnfEx34",nnfEx34),("nnfEx35",nnfEx35),("nnfEx36",nnfEx36),("nnfEx37",nnfEx37),("nnfEx38",nnfEx38),("nnfEx39",nnfEx39),("nnfEx40",nnfEx40),("nnfEx41",nnfEx41),("nnfEx42",nnfEx42),("nnfEx43",nnfEx43),("nnfEx44",nnfEx44),("nnfEx45",nnfEx45),("nnfEx46",nnfEx46),("nnfEx47",nnfEx47),("nnfEx48",nnfEx48),("nnfEx49",nnfEx49),("nnfEx50",nnfEx50),("nnfEx51",nnfEx51),("nnfEx52",nnfEx52),("nnfEx53",nnfEx53),("nnfEx54",nnfEx54),("nnfEx55",nnfEx55),("nnfEx56",nnfEx56),("nnfEx57",nnfEx57),("nnfEx58",nnfEx58),("nnfEx59",nnfEx59),("nnfEx60",nnfEx60),("nnfEx61",nnfEx61),("nnfEx62",nnfEx62),("nnfEx63",nnfEx63),("nnfEx64",nnfEx64),("nnfEx65",nnfEx65),("nnfEx66",nnfEx66),("nnfEx67",nnfEx67),("nnfEx68",nnfEx68),("nnfEx69",nnfEx69),("nnfEx70",nnfEx70),("nnfEx71",nnfEx71),("nnfEx72",nnfEx72),("nnfEx73",nnfEx73),("nnfEx74",nnfEx74),("nnfEx75",nnfEx75),("nnfEx76",nnfEx76),("nnfEx77",nnfEx77),("nnfEx78",nnfEx78),("nnfEx79",nnfEx79),("nnfEx80",nnfEx80),("nnfEx81",nnfEx81),("nnfEx82",nnfEx82),("nnfEx83",nnfEx83),("nnfEx84",nnfEx84),("nnfEx85",nnfEx85),("nnfEx86",nnfEx86),("nnfEx87",nnfEx87),("nnfEx88",nnfEx88),("nnfEx89",nnfEx89),("nnfEx90",nnfEx90),("nnfEx91",nnfEx91),("nnfEx92",nnfEx92),("nnfEx93",nnfEx93),("nnfEx94",nnfEx94),("nnfEx95",nnfEx95),("nnfEx96",nnfEx96),("nnfEx97",nnfEx97),("nnfEx98",nnfEx98),("nnfEx99",nnfEx99),("nnfEx100",nnfEx100)]

cnfEx01 :: CNF
cnfEx01 = []
cnfEx02 :: CNF
cnfEx02 = [[]]
cnfEx03 :: CNF
cnfEx03 = [[(True,"a")]]
cnfEx04 :: CNF
cnfEx04 = [[(False,"a")]]
cnfEx05 :: CNF
cnfEx05 = [[(False,"e"),(True,"a"),(True,"b"),(True,"a"),(False,"d"),(False,"b")],[],[(True,"b"),(True,"b"),(False,"b"),(False,"d"),(False,"c"),(True,"c"),(True,"d"),(False,"a")],[(False,"a"),(False,"a"),(True,"b"),(True,"a"),(True,"b"),(True,"b"),(True,"e"),(True,"e")]]
cnfEx06 :: CNF
cnfEx06 = [[(False,"e")],[(False,"c"),(False,"d"),(False,"b"),(True,"b"),(False,"i"),(True,"f")]]
cnfEx07 :: CNF
cnfEx07 = [[(False,"e"),(False,"c"),(True,"b"),(True,"d"),(True,"e"),(True,"d"),(False,"b")],[(False,"b"),(False,"b"),(False,"e"),(False,"c"),(True,"e"),(True,"a"),(True,"d")],[(True,"d"),(False,"e"),(True,"b"),(True,"a"),(False,"c"),(False,"b"),(True,"c"),(False,"a")],[(False,"e")],[(True,"c"),(False,"a"),(False,"a")],[(False,"b"),(True,"a"),(False,"c")],[],[(True,"b")]]
cnfEx08 :: CNF
cnfEx08 = [[(False,"e"),(True,"b"),(False,"c"),(True,"c"),(False,"a")],[(False,"a"),(True,"d"),(True,"c"),(False,"d"),(False,"c"),(True,"d")],[(True,"e"),(False,"c"),(True,"e"),(False,"d"),(False,"d")],[],[(False,"b"),(True,"e")],[],[(False,"c"),(True,"e")],[(False,"e"),(True,"c"),(False,"d"),(True,"a"),(True,"d"),(True,"e"),(False,"e"),(True,"c")],[(True,"a"),(False,"c"),(False,"b"),(False,"c"),(True,"a"),(False,"c"),(True,"d"),(False,"c"),(True,"e")]]
cnfEx09 :: CNF
cnfEx09 = [[(False,"g"),(False,"c"),(True,"c"),(False,"f")],[(False,"e"),(False,"h"),(False,"h"),(True,"c"),(False,"h")]]
cnfEx10 :: CNF
cnfEx10 = [[(False,"a"),(True,"c"),(True,"b"),(False,"a"),(False,"e"),(False,"c")],[(False,"c"),(False,"c")],[],[(False,"a"),(True,"c"),(True,"a"),(False,"e"),(False,"d"),(True,"b"),(True,"d"),(True,"a"),(False,"c")],[(True,"a"),(False,"d")],[(False,"a"),(True,"c"),(False,"a"),(True,"a")],[(True,"e"),(True,"b"),(False,"e")],[(True,"a"),(False,"e"),(False,"e"),(False,"b"),(False,"a"),(False,"d"),(True,"c"),(True,"d"),(True,"e")]]
cnfEx11 :: CNF
cnfEx11 = [[(True,"d")],[(True,"a"),(True,"a"),(False,"c"),(False,"j"),(False,"e"),(False,"j")],[(True,"h"),(False,"i"),(True,"j"),(True,"i")]]
cnfEx12 :: CNF
cnfEx12 = [[(False,"d"),(True,"d")],[(True,"b"),(False,"e"),(True,"d")],[(True,"b"),(True,"b"),(True,"e"),(True,"d")]]
cnfEx13 :: CNF
cnfEx13 = [[(False,"e"),(True,"e"),(False,"b")],[(True,"a"),(True,"a"),(False,"b")],[(True,"e"),(False,"c"),(True,"e"),(True,"b"),(True,"d"),(False,"e")],[(False,"d"),(False,"a"),(False,"d")],[(False,"e"),(False,"c"),(True,"a"),(True,"b"),(False,"a")],[(True,"e"),(False,"d"),(True,"d"),(False,"c")],[(False,"b"),(True,"b"),(False,"b"),(False,"a"),(True,"c"),(False,"c"),(True,"a")]]
cnfEx14 :: CNF
cnfEx14 = [[(False,"e"),(False,"f"),(False,"j"),(True,"j"),(False,"j"),(True,"j"),(True,"i")],[(True,"b"),(False,"f")],[],[(True,"d"),(True,"g"),(False,"j"),(True,"j"),(True,"c"),(False,"j"),(False,"h")],[(False,"d"),(True,"i"),(True,"j"),(False,"c"),(True,"j"),(False,"c"),(True,"f"),(True,"d"),(False,"f")],[(True,"h"),(True,"g"),(True,"b"),(True,"e")],[(True,"h"),(True,"c"),(True,"e"),(True,"g"),(False,"c"),(False,"i"),(True,"g"),(False,"j"),(False,"b")]]
cnfEx15 :: CNF
cnfEx15 = [[(False,"e"),(False,"c"),(False,"e"),(False,"a"),(True,"d"),(False,"e"),(True,"a"),(True,"b"),(True,"c")],[(True,"a"),(True,"e"),(False,"a"),(True,"b"),(True,"b")],[(False,"c"),(True,"a"),(True,"c"),(False,"d"),(False,"b")]]
cnfEx16 :: CNF
cnfEx16 = [[(False,"e"),(True,"g"),(True,"a"),(True,"b"),(False,"c"),(True,"b"),(True,"j"),(False,"b")],[(False,"g"),(False,"g"),(True,"h"),(False,"e"),(False,"j"),(False,"b"),(False,"e"),(True,"d"),(False,"e")]]
cnfEx17 :: CNF
cnfEx17 = [[(True,"e"),(True,"d"),(True,"c"),(False,"b"),(True,"b"),(False,"b"),(True,"d"),(True,"e")]]
cnfEx18 :: CNF
cnfEx18 = [[(False,"f"),(True,"h"),(False,"d"),(False,"d"),(True,"f"),(False,"b"),(False,"c")],[(True,"e"),(False,"g"),(True,"e"),(False,"a")],[(True,"b"),(True,"c"),(True,"h"),(True,"e")],[(True,"f"),(True,"g"),(True,"j"),(True,"d"),(True,"d"),(True,"c"),(False,"i"),(False,"j"),(True,"j")],[(False,"j"),(True,"i"),(True,"b")],[(True,"e"),(True,"a"),(False,"a"),(True,"a"),(False,"i"),(True,"g")]]
cnfEx19 :: CNF
cnfEx19 = [[(True,"c"),(False,"b")],[(False,"b"),(False,"a")],[(False,"e"),(False,"a")],[(True,"e")],[(True,"c"),(False,"e"),(False,"e"),(True,"d"),(True,"d"),(True,"a")],[(True,"b"),(False,"e"),(True,"d")],[(True,"c"),(False,"b"),(False,"b")]]
cnfEx20 :: CNF
cnfEx20 = [[(True,"i"),(False,"g"),(False,"i"),(True,"e")],[(False,"a"),(False,"b"),(False,"g"),(True,"b"),(False,"j")]]
cnfEx21 :: CNF
cnfEx21 = [[(True,"a"),(False,"b"),(True,"e"),(False,"e")],[(False,"e"),(False,"c"),(True,"c"),(False,"d"),(True,"d"),(False,"c"),(True,"d"),(True,"d"),(False,"e")]]
cnfEx22 :: CNF
cnfEx22 = [[(False,"e"),(False,"g"),(True,"e")]]
cnfEx23 :: CNF
cnfEx23 = [[(False,"c"),(False,"c"),(False,"a"),(True,"d"),(False,"a"),(False,"d"),(True,"d"),(True,"c"),(False,"b")],[(False,"e")],[(True,"d"),(True,"c"),(False,"e"),(False,"b"),(True,"a"),(False,"b"),(True,"e"),(False,"e"),(False,"d")],[(False,"a"),(False,"c"),(False,"c"),(False,"d"),(False,"b"),(True,"c"),(False,"a"),(True,"b")],[(True,"b"),(False,"a"),(False,"c"),(False,"d"),(False,"c"),(True,"a"),(False,"d"),(False,"a"),(False,"b")]]
cnfEx24 :: CNF
cnfEx24 = [[(False,"a"),(True,"j"),(False,"b"),(False,"i"),(True,"j"),(True,"a")],[(False,"b"),(False,"e"),(False,"f"),(False,"e"),(False,"c")],[(False,"e"),(False,"e"),(True,"e"),(True,"b"),(True,"d"),(True,"h")],[(False,"d"),(True,"a"),(False,"i")],[(True,"j"),(True,"e"),(True,"j"),(True,"c"),(True,"c"),(True,"f"),(True,"d"),(False,"c")],[(False,"h"),(False,"b"),(False,"a"),(False,"a"),(False,"f"),(False,"j"),(False,"h"),(True,"b")],[(False,"j"),(True,"a"),(True,"h"),(False,"g"),(False,"f"),(True,"c"),(True,"c"),(True,"d"),(True,"j")],[(False,"e"),(False,"a"),(True,"h"),(False,"h"),(False,"i"),(False,"b")],[(True,"d"),(False,"f"),(False,"b"),(True,"a"),(False,"d"),(True,"b"),(True,"e"),(True,"b"),(True,"g")]]
cnfEx25 :: CNF
cnfEx25 = [[(True,"e"),(False,"d"),(False,"b"),(True,"c"),(False,"d")],[(True,"d"),(True,"b"),(False,"d")],[(False,"d"),(False,"a"),(False,"c"),(False,"c")],[],[(True,"e"),(True,"e"),(False,"d"),(False,"e"),(False,"a"),(False,"a"),(True,"c")],[(True,"e")],[(True,"c"),(True,"a")],[(False,"d"),(False,"c"),(False,"d"),(True,"b"),(False,"a"),(True,"a"),(True,"c"),(True,"d"),(False,"d")]]
cnfEx26 :: CNF
cnfEx26 = [[(False,"e"),(False,"g"),(False,"d"),(False,"c"),(True,"b")],[(False,"h"),(True,"f"),(True,"d"),(False,"b"),(False,"d"),(False,"i"),(False,"i"),(True,"g"),(True,"j")],[(False,"d"),(True,"a"),(False,"c"),(False,"c"),(True,"d")],[(True,"a"),(True,"f")]]
cnfEx27 :: CNF
cnfEx27 = [[(True,"d"),(False,"c"),(False,"b"),(True,"a"),(True,"c")],[(True,"a"),(False,"a"),(True,"c"),(False,"d")],[(False,"d"),(True,"a"),(True,"d"),(False,"d"),(False,"b"),(False,"a"),(True,"d")],[(True,"c"),(False,"d"),(True,"c"),(False,"b"),(True,"e"),(False,"b"),(True,"e"),(False,"c"),(True,"c")],[(False,"e"),(True,"c"),(False,"a"),(True,"b"),(False,"a"),(False,"d"),(False,"d"),(False,"c"),(True,"c")]]
cnfEx28 :: CNF
cnfEx28 = [[(True,"e"),(True,"e"),(False,"d")],[(False,"a"),(True,"a"),(False,"e"),(True,"d"),(True,"d"),(True,"d")]]
cnfEx29 :: CNF
cnfEx29 = [[],[(True,"f"),(True,"a"),(False,"b"),(False,"c"),(True,"c"),(True,"b"),(True,"g"),(False,"f")],[],[(False,"h"),(False,"e"),(True,"c"),(False,"g"),(False,"i"),(True,"h"),(True,"c"),(False,"j"),(True,"c")],[(False,"g")],[(True,"d"),(False,"f")]]
cnfEx30 :: CNF
cnfEx30 = [[(True,"c"),(False,"d"),(False,"e"),(True,"e")],[(False,"c"),(True,"e"),(False,"b"),(True,"b"),(True,"a")],[(True,"c")],[],[(False,"c")],[(False,"d"),(True,"b"),(False,"c"),(True,"c"),(False,"d"),(False,"e")]]
cnfEx31 :: CNF
cnfEx31 = [[(True,"f"),(False,"f"),(False,"c"),(True,"h"),(False,"a"),(True,"h"),(False,"c")],[(False,"i"),(False,"i"),(True,"a"),(False,"h"),(True,"c"),(True,"j"),(True,"f"),(False,"f")],[(True,"a"),(False,"a"),(False,"e"),(True,"i"),(True,"e"),(True,"d"),(True,"a")],[],[(True,"e"),(False,"j"),(False,"d"),(False,"f"),(True,"h"),(False,"i"),(True,"e")],[(False,"j"),(False,"c")]]
cnfEx32 :: CNF
cnfEx32 = [[(True,"h"),(True,"g"),(True,"c"),(False,"j"),(False,"c")],[(False,"a"),(True,"i"),(True,"i")],[(True,"a"),(True,"e"),(False,"h")],[]]
cnfEx33 :: CNF
cnfEx33 = [[(True,"c"),(False,"b")],[(False,"c"),(True,"c")],[(True,"d"),(False,"a")],[(False,"d"),(False,"c"),(True,"a"),(True,"c"),(False,"c"),(True,"d"),(False,"c"),(True,"a"),(True,"c")],[(True,"c"),(False,"b")],[(False,"c"),(False,"e"),(False,"a"),(False,"e"),(True,"d"),(True,"e"),(False,"b"),(True,"e"),(False,"c")],[(False,"d"),(False,"b")],[(True,"d")],[(False,"c"),(True,"c"),(True,"e"),(False,"c"),(False,"e"),(False,"c")]]
cnfEx34 :: CNF
cnfEx34 = [[(False,"f"),(False,"e"),(True,"g"),(False,"h"),(False,"e"),(False,"g")],[(False,"e"),(False,"a")],[(False,"f"),(True,"a")],[(True,"c"),(False,"i"),(True,"j"),(True,"e"),(False,"b"),(False,"b")],[(True,"j"),(False,"j"),(True,"c"),(False,"j"),(False,"f")],[(False,"i"),(False,"a"),(True,"i"),(True,"a"),(False,"e"),(True,"j"),(True,"f"),(True,"d")],[],[(False,"g"),(True,"j"),(False,"e")],[(False,"j"),(True,"i"),(True,"b"),(True,"e"),(True,"i"),(True,"b"),(False,"d")]]
cnfEx35 :: CNF
cnfEx35 = [[(True,"c"),(True,"a")]]
cnfEx36 :: CNF
cnfEx36 = [[(False,"c"),(False,"a"),(True,"h"),(False,"i"),(True,"b"),(False,"j"),(False,"e"),(True,"g"),(False,"h")],[(False,"a")],[],[(False,"h"),(True,"e"),(False,"b")],[(False,"j"),(False,"d"),(True,"i"),(True,"g"),(True,"j"),(False,"h"),(False,"i"),(True,"a")],[(False,"c"),(True,"g"),(False,"d"),(False,"b"),(False,"g"),(True,"i"),(False,"h")],[(True,"i"),(False,"f"),(True,"d"),(True,"h"),(True,"h"),(False,"b"),(True,"i"),(False,"d"),(False,"i")]]
cnfEx37 :: CNF
cnfEx37 = [[(False,"e")],[(False,"b"),(True,"d"),(True,"b"),(True,"e"),(False,"d"),(False,"e")],[(True,"c"),(True,"b")]]
cnfEx38 :: CNF
cnfEx38 = [[(False,"h")],[(False,"e"),(True,"h"),(True,"i"),(False,"g"),(False,"b"),(True,"c"),(True,"d"),(True,"a")],[(True,"g"),(False,"a"),(True,"i"),(False,"i"),(True,"j"),(False,"h"),(True,"j"),(True,"d"),(False,"b")],[],[(True,"e"),(True,"a"),(False,"b"),(False,"g"),(False,"j")],[(False,"e"),(True,"e")],[(False,"e"),(True,"a"),(True,"j"),(True,"i"),(False,"b"),(False,"c"),(True,"g")],[]]
cnfEx39 :: CNF
cnfEx39 = [[],[(False,"a"),(False,"b"),(True,"c"),(True,"e"),(True,"e"),(True,"d"),(False,"c"),(True,"d")],[(True,"c")],[(False,"a"),(False,"c"),(True,"e"),(False,"d"),(True,"c"),(False,"e"),(True,"e")]]
cnfEx40 :: CNF
cnfEx40 = [[(True,"e"),(False,"e"),(False,"a"),(True,"f"),(False,"h"),(False,"b"),(True,"f"),(False,"e")],[(False,"c"),(True,"b"),(False,"f"),(False,"b"),(True,"g"),(False,"e")],[(False,"g"),(False,"i"),(False,"g"),(False,"h")],[(False,"j")],[(False,"e"),(False,"f"),(True,"e"),(True,"h"),(False,"b"),(True,"j"),(True,"h"),(False,"f"),(False,"c")]]
cnfEx41 :: CNF
cnfEx41 = [[(False,"a"),(True,"e")],[(False,"e"),(True,"c"),(False,"a"),(True,"e"),(True,"a"),(True,"b")],[(False,"b"),(True,"c"),(False,"d"),(False,"c"),(False,"a"),(False,"b"),(True,"b"),(True,"e"),(True,"b")],[(False,"b")],[],[(False,"d"),(False,"a"),(True,"e"),(False,"d"),(True,"c"),(True,"c"),(True,"b"),(False,"d")],[(True,"d")],[(True,"c"),(True,"b"),(False,"e"),(True,"a"),(True,"e"),(False,"b"),(False,"a"),(False,"e"),(True,"c")],[(False,"d"),(True,"e"),(False,"a")]]
cnfEx42 :: CNF
cnfEx42 = [[(True,"h"),(False,"a"),(False,"g"),(True,"i")],[(True,"h")],[(False,"e"),(False,"c"),(False,"g"),(False,"h"),(True,"f"),(True,"e")],[(True,"j"),(False,"f"),(False,"j"),(True,"f"),(True,"e"),(False,"e")]]
cnfEx43 :: CNF
cnfEx43 = [[(True,"d"),(False,"g"),(True,"e"),(True,"j"),(False,"e"),(True,"f"),(True,"b"),(False,"g"),(True,"b")],[(True,"f"),(False,"h"),(True,"g")],[(True,"e"),(False,"f"),(False,"c"),(True,"e")],[(True,"d")],[],[(True,"d"),(False,"b")],[(False,"i"),(False,"c")],[],[(True,"a")]]
cnfEx44 :: CNF
cnfEx44 = [[(False,"b"),(False,"d")],[(False,"d"),(False,"d"),(True,"b"),(False,"c")],[(False,"d"),(False,"d"),(True,"d"),(True,"e"),(False,"a"),(False,"b"),(False,"d")],[],[(False,"d"),(False,"c"),(True,"b"),(True,"a"),(True,"e"),(False,"e")],[(False,"e"),(True,"d"),(False,"d"),(False,"a"),(False,"c"),(True,"d")],[(True,"b"),(False,"e"),(False,"a"),(True,"b"),(False,"c"),(True,"e")]]
cnfEx45 :: CNF
cnfEx45 = [[(True,"h"),(True,"j"),(True,"e"),(False,"c"),(True,"a")],[(False,"g"),(True,"a")],[(False,"a"),(True,"b"),(False,"j"),(True,"j"),(False,"a"),(False,"h"),(True,"j")],[(False,"a"),(True,"f"),(True,"e"),(False,"j"),(False,"j"),(False,"i"),(True,"i"),(False,"h")],[(False,"j"),(True,"g"),(True,"f")],[(True,"a"),(False,"e"),(True,"h"),(False,"g"),(False,"i"),(False,"a"),(False,"a"),(False,"c")],[(False,"e"),(False,"d"),(False,"c"),(False,"i"),(True,"g"),(True,"b"),(True,"j"),(False,"g"),(False,"b")],[(False,"e"),(True,"a")]]
cnfEx46 :: CNF
cnfEx46 = [[(True,"a"),(True,"b"),(False,"d")],[(False,"d"),(True,"e"),(False,"d"),(True,"d"),(False,"a")],[(False,"e"),(True,"a"),(True,"d"),(True,"b")],[(False,"d"),(True,"a"),(True,"d"),(True,"d"),(True,"c"),(True,"c"),(False,"a")]]
cnfEx47 :: CNF
cnfEx47 = [[(True,"i"),(True,"a"),(True,"i"),(True,"f"),(True,"a"),(True,"c"),(False,"g"),(False,"d")],[(True,"b"),(True,"e"),(False,"b")],[(False,"h"),(False,"i"),(False,"j"),(True,"f"),(False,"h"),(True,"g")],[(True,"b")],[(True,"j"),(True,"b"),(True,"j"),(False,"a")],[(False,"h"),(False,"i"),(True,"c"),(False,"h"),(False,"a"),(True,"g"),(False,"b"),(False,"e"),(False,"e")],[(True,"f"),(False,"g"),(False,"i"),(True,"e"),(True,"c"),(True,"e"),(True,"d"),(True,"e")],[(False,"f"),(False,"h"),(True,"h"),(True,"e"),(True,"h"),(True,"h")],[(True,"f"),(False,"d"),(True,"h"),(True,"g"),(False,"f"),(True,"j"),(False,"b")]]
cnfEx48 :: CNF
cnfEx48 = [[(True,"c"),(False,"d"),(True,"b"),(True,"d")],[(False,"b"),(True,"e"),(True,"a"),(True,"d")],[(False,"a"),(True,"c"),(False,"e")],[(True,"a"),(True,"d")],[(True,"c")],[(True,"e"),(False,"b"),(False,"a"),(True,"c"),(True,"c")],[(False,"d"),(False,"e"),(False,"e")],[]]
cnfEx49 :: CNF
cnfEx49 = [[(True,"d"),(False,"d"),(False,"c"),(False,"c")],[(False,"c"),(False,"c"),(True,"j"),(False,"i"),(False,"g"),(False,"b"),(False,"i"),(True,"f")],[(True,"c"),(True,"e"),(True,"g"),(False,"f")],[(False,"c"),(False,"e")],[(True,"b"),(True,"j"),(False,"c"),(False,"a"),(True,"c"),(False,"a"),(False,"j")],[(True,"j"),(True,"e"),(False,"e"),(False,"b"),(False,"i"),(False,"h"),(False,"b"),(True,"f")],[(False,"d"),(True,"i"),(True,"g"),(True,"f"),(False,"h"),(True,"e")]]
cnfEx50 :: CNF
cnfEx50 = [[(True,"d"),(False,"e"),(False,"a"),(False,"a"),(False,"c"),(True,"d"),(False,"b")],[(False,"e"),(False,"d"),(False,"e")],[(False,"b"),(True,"e"),(False,"a"),(False,"c"),(True,"e")],[(False,"e"),(True,"a"),(True,"e"),(True,"c"),(True,"e"),(True,"c")],[(False,"d"),(True,"e"),(True,"a"),(False,"e")],[(True,"a")],[(True,"e"),(False,"b"),(True,"a"),(False,"b"),(True,"d"),(False,"b"),(False,"d")],[(False,"d"),(False,"d"),(False,"c"),(True,"b")],[(False,"a"),(False,"a"),(True,"c"),(True,"b"),(False,"d"),(False,"d"),(True,"b"),(True,"e"),(False,"e")]]
cnfEx51 :: CNF
cnfEx51 = [[(False,"d"),(False,"c"),(False,"a"),(True,"i"),(False,"f"),(False,"a"),(True,"i"),(False,"g")],[(False,"h"),(True,"f"),(False,"f"),(True,"h"),(False,"i"),(True,"g"),(True,"d")],[(False,"j"),(True,"c"),(True,"h"),(True,"b"),(False,"j"),(False,"h")],[(True,"e"),(True,"e"),(False,"h"),(True,"j"),(True,"b"),(True,"b"),(False,"j"),(False,"e"),(False,"d")]]
cnfEx52 :: CNF
cnfEx52 = [[(False,"b"),(False,"b"),(True,"e"),(True,"c"),(True,"a"),(False,"b"),(False,"a"),(False,"a"),(False,"a")],[(False,"e"),(True,"c"),(False,"b"),(False,"b")],[(True,"e"),(False,"e"),(True,"e"),(False,"b"),(False,"d"),(False,"c"),(True,"d"),(True,"a"),(False,"a")]]
cnfEx53 :: CNF
cnfEx53 = [[(True,"f"),(False,"i"),(True,"i"),(True,"f"),(False,"i")],[]]
cnfEx54 :: CNF
cnfEx54 = [[(False,"a"),(False,"d"),(False,"b"),(False,"e"),(False,"e"),(True,"c"),(False,"c"),(True,"a"),(False,"d")]]
cnfEx55 :: CNF
cnfEx55 = [[(False,"a"),(False,"h"),(True,"a"),(False,"b"),(False,"c"),(True,"i"),(True,"c")],[(False,"f"),(True,"e"),(True,"i"),(False,"h"),(False,"a"),(True,"j"),(False,"f"),(False,"g"),(False,"b")],[(False,"b"),(False,"a"),(False,"h")],[(True,"c"),(True,"h"),(True,"d"),(True,"a"),(True,"f")],[(True,"g"),(True,"g"),(False,"a"),(True,"f"),(False,"a"),(True,"b"),(True,"e"),(False,"f"),(False,"e")],[(True,"j"),(True,"e"),(False,"j")],[(False,"e"),(True,"c"),(True,"d"),(True,"c"),(True,"e"),(True,"i"),(False,"a")],[(True,"e")]]
cnfEx56 :: CNF
cnfEx56 = [[(False,"e"),(False,"a"),(True,"e"),(True,"c"),(True,"c"),(False,"c"),(True,"d")],[(True,"b"),(False,"d"),(False,"c"),(True,"a"),(True,"a")],[(True,"a"),(True,"a"),(False,"b"),(False,"a"),(False,"a"),(True,"c"),(False,"d"),(True,"e"),(False,"e")],[(True,"e")],[(False,"d"),(True,"b"),(True,"c"),(False,"a"),(False,"a"),(False,"b")]]
cnfEx57 :: CNF
cnfEx57 = [[(True,"c"),(True,"d"),(False,"f"),(True,"c")],[(True,"f"),(False,"g")],[(True,"b"),(False,"h"),(True,"h"),(True,"c"),(True,"c"),(False,"j"),(False,"c"),(False,"b"),(False,"a")],[(False,"f"),(True,"c"),(True,"a")],[(True,"e"),(True,"i"),(False,"j"),(False,"b"),(True,"f"),(False,"a")]]
cnfEx58 :: CNF
cnfEx58 = [[(False,"c")],[(True,"a"),(True,"d"),(True,"b"),(True,"a"),(False,"a"),(True,"d")],[(True,"c"),(False,"d"),(False,"a"),(False,"c"),(True,"c"),(False,"a")],[(True,"c"),(True,"d"),(True,"a"),(False,"a"),(False,"d"),(True,"c"),(True,"b"),(False,"d")],[],[(True,"c"),(True,"d")],[],[(True,"c"),(False,"b"),(True,"a"),(True,"c")],[(True,"e"),(False,"e"),(True,"c"),(False,"c"),(False,"a")]]
cnfEx59 :: CNF
cnfEx59 = [[(True,"j"),(False,"i"),(False,"h"),(False,"a")],[(False,"c"),(True,"h"),(False,"e"),(True,"b"),(True,"e"),(False,"e"),(False,"h"),(True,"b")],[(False,"i"),(True,"d"),(False,"f"),(False,"d"),(True,"h"),(False,"d")],[(True,"j"),(False,"b"),(True,"b")],[(True,"j"),(False,"a"),(False,"a"),(True,"h")]]
cnfEx60 :: CNF
cnfEx60 = [[(True,"a")],[],[(False,"a"),(False,"d")],[(False,"c"),(False,"c"),(False,"d"),(False,"c"),(True,"d"),(True,"e"),(False,"c")],[(True,"e"),(True,"a")],[],[(True,"c"),(True,"d")]]
cnfEx61 :: CNF
cnfEx61 = [[(True,"g"),(True,"h"),(False,"h"),(True,"e"),(True,"b")],[(False,"a"),(True,"h"),(True,"j"),(False,"d"),(False,"e")],[(True,"f"),(True,"c"),(False,"g"),(False,"c"),(False,"d"),(True,"a"),(True,"c")],[(True,"j"),(True,"f"),(True,"e"),(True,"f"),(True,"h"),(True,"g"),(True,"f"),(True,"i")],[(False,"j"),(False,"d")]]
cnfEx62 :: CNF
cnfEx62 = [[(True,"c"),(False,"e"),(True,"a"),(False,"d"),(True,"d"),(False,"e"),(True,"a")],[(True,"a"),(False,"d"),(False,"a"),(True,"c"),(True,"e"),(False,"e"),(True,"c"),(True,"c"),(True,"d")],[(True,"a"),(False,"d")],[(False,"b"),(True,"d")],[(True,"e")],[(True,"a"),(False,"d"),(True,"c"),(True,"d"),(True,"a"),(True,"c"),(True,"d"),(False,"e"),(True,"e")],[(False,"e"),(False,"d"),(False,"b"),(False,"e"),(True,"a")]]
cnfEx63 :: CNF
cnfEx63 = [[(False,"f"),(False,"d"),(False,"i"),(False,"b"),(False,"j"),(True,"f"),(True,"c"),(False,"j"),(True,"d")],[(True,"d"),(True,"c"),(False,"j")],[(True,"j"),(True,"a"),(False,"f"),(False,"e"),(True,"a"),(True,"e"),(True,"d")],[(True,"g"),(False,"i"),(True,"h")],[(False,"e"),(False,"b"),(False,"g"),(False,"f"),(True,"c"),(True,"c"),(True,"h"),(True,"b")],[(False,"g"),(True,"a"),(True,"e"),(True,"c"),(False,"j"),(False,"a"),(False,"i")],[(True,"j"),(True,"d"),(False,"a"),(True,"b"),(True,"c")],[(True,"c"),(True,"f"),(True,"g"),(False,"f"),(False,"d"),(True,"c")],[(True,"a"),(True,"i"),(False,"j")]]
cnfEx64 :: CNF
cnfEx64 = [[(False,"c"),(True,"a"),(False,"d"),(False,"c")],[(False,"c"),(False,"e"),(True,"d"),(False,"e")],[(False,"e"),(False,"c"),(True,"b")],[(False,"d"),(True,"a"),(True,"d"),(True,"b"),(False,"e"),(True,"a"),(True,"c"),(False,"c"),(False,"b")]]
cnfEx65 :: CNF
cnfEx65 = [[(True,"j"),(False,"b"),(False,"g"),(True,"c")],[(True,"i"),(False,"i"),(False,"g"),(True,"e"),(True,"f"),(True,"i"),(False,"d"),(True,"d")]]
cnfEx66 :: CNF
cnfEx66 = [[(True,"e"),(False,"e"),(False,"e"),(False,"b"),(True,"d"),(False,"c"),(True,"a")]]
cnfEx67 :: CNF
cnfEx67 = [[(True,"c"),(True,"d"),(True,"g"),(True,"h"),(False,"g"),(True,"f")]]
cnfEx68 :: CNF
cnfEx68 = [[(True,"c"),(False,"b"),(True,"a")],[(True,"a"),(False,"a"),(True,"b"),(True,"c"),(True,"c"),(False,"c"),(True,"c"),(True,"d")],[(True,"e"),(False,"c"),(True,"b"),(True,"c"),(False,"d")],[],[(False,"a"),(True,"e"),(False,"c"),(True,"c"),(True,"e")],[(True,"e"),(True,"a"),(False,"b"),(False,"a"),(False,"d"),(False,"d"),(False,"c"),(False,"c")]]
cnfEx69 :: CNF
cnfEx69 = [[(True,"c"),(False,"d"),(False,"h"),(True,"g")],[(True,"g")],[(False,"e"),(False,"b"),(True,"f"),(False,"g"),(True,"g"),(True,"i"),(True,"h"),(False,"j"),(True,"e")],[(False,"a"),(True,"g"),(True,"i"),(False,"g"),(False,"h"),(False,"h"),(False,"c"),(True,"g")],[(True,"a")],[(True,"a"),(False,"d"),(False,"i"),(True,"g"),(True,"g"),(False,"e"),(False,"i"),(False,"e")],[(False,"d"),(False,"d"),(False,"a"),(True,"b"),(False,"d"),(False,"b")]]
cnfEx70 :: CNF
cnfEx70 = [[(True,"c"),(True,"c"),(False,"c"),(False,"a"),(False,"d"),(False,"d"),(False,"a")],[(False,"e"),(True,"c"),(True,"b"),(True,"a"),(True,"b"),(False,"a"),(True,"d"),(True,"e")],[(True,"c"),(True,"b"),(True,"d")],[(False,"a"),(False,"e"),(False,"c"),(True,"e"),(False,"e")]]
cnfEx71 :: CNF
cnfEx71 = [[(False,"b"),(False,"a"),(True,"i")],[(True,"g"),(False,"d")],[(True,"e"),(True,"c"),(False,"f"),(True,"c"),(True,"j"),(True,"f")],[(True,"a"),(True,"j"),(True,"g"),(True,"e")],[(True,"d"),(False,"g"),(True,"e"),(True,"j")],[(True,"e"),(True,"b"),(False,"e"),(False,"c"),(True,"i"),(False,"i"),(False,"e"),(False,"d"),(False,"f")],[(False,"h")]]
cnfEx72 :: CNF
cnfEx72 = [[(False,"b"),(True,"a"),(False,"b"),(False,"d")],[(True,"b")],[(True,"e"),(True,"b"),(True,"e"),(False,"e"),(False,"c"),(False,"b"),(True,"e")],[(True,"b"),(False,"c"),(True,"b")],[(True,"a")],[(False,"d"),(False,"c")],[(False,"a"),(False,"e")],[(True,"e"),(True,"c"),(True,"c"),(False,"d"),(True,"c"),(True,"e"),(False,"a")]]
cnfEx73 :: CNF
cnfEx73 = [[(True,"f"),(False,"c")],[(True,"g"),(True,"h"),(False,"j")],[(True,"b"),(True,"d"),(False,"h"),(False,"g"),(False,"e"),(True,"c"),(True,"j")],[(True,"f"),(False,"e"),(False,"a"),(False,"e")],[(False,"j"),(False,"a"),(False,"a"),(True,"f"),(True,"j"),(False,"i"),(True,"a")]]
cnfEx74 :: CNF
cnfEx74 = [[(False,"c"),(False,"c"),(True,"e"),(True,"b"),(False,"b"),(True,"b"),(True,"a")],[(False,"b"),(False,"d"),(True,"d"),(True,"e"),(False,"a"),(False,"a"),(True,"b"),(False,"b")]]
cnfEx75 :: CNF
cnfEx75 = [[(True,"h"),(True,"a"),(False,"f"),(False,"e"),(True,"g"),(True,"h"),(True,"c"),(True,"j"),(False,"e")],[(False,"j"),(True,"j"),(False,"i"),(False,"j"),(True,"i"),(False,"a"),(True,"h")],[(False,"d"),(True,"d"),(True,"j"),(False,"h"),(True,"c"),(False,"i"),(False,"f"),(True,"j")],[(False,"e"),(True,"b")],[(True,"b")]]
cnfEx76 :: CNF
cnfEx76 = [[(True,"e"),(False,"b")],[(False,"c"),(False,"e"),(True,"a"),(True,"a"),(True,"e"),(False,"b"),(False,"d"),(True,"d"),(False,"d")],[],[(False,"e"),(False,"d"),(True,"c"),(False,"a")],[(False,"b"),(False,"e"),(True,"b"),(False,"c"),(True,"b"),(True,"e"),(False,"b"),(True,"c")]]
cnfEx77 :: CNF
cnfEx77 = [[(False,"c"),(True,"b"),(True,"c"),(False,"d"),(False,"f"),(False,"g"),(True,"j")],[(True,"i"),(False,"h"),(False,"c")],[(True,"d"),(False,"d"),(False,"h"),(True,"f"),(False,"c"),(False,"g"),(True,"i")],[(False,"d"),(True,"d")]]
cnfEx78 :: CNF
cnfEx78 = [[(True,"d"),(False,"d"),(True,"d")],[(False,"e"),(True,"e"),(True,"c"),(False,"b"),(False,"c")],[(True,"b"),(True,"e"),(False,"e"),(False,"b"),(False,"a")],[(False,"d"),(False,"b"),(True,"c"),(True,"d"),(False,"e"),(True,"a"),(False,"a"),(False,"e"),(True,"b")],[(True,"e")],[(True,"a"),(False,"e"),(False,"d")],[(True,"b"),(False,"a"),(False,"a"),(True,"a"),(True,"e"),(True,"a"),(True,"e"),(False,"b"),(False,"b")],[],[(False,"d"),(True,"e"),(False,"e"),(False,"b"),(True,"d"),(False,"e")]]
cnfEx79 :: CNF
cnfEx79 = [[(False,"f"),(False,"i"),(True,"h")],[(True,"h"),(True,"f")]]
cnfEx80 :: CNF
cnfEx80 = [[(True,"c"),(True,"d"),(False,"a"),(False,"a"),(True,"a"),(True,"e"),(False,"a"),(False,"a"),(False,"c")]]
cnfEx81 :: CNF
cnfEx81 = [[(True,"c")],[(True,"f"),(False,"e"),(False,"d"),(True,"c"),(False,"h"),(False,"g"),(False,"f"),(False,"e"),(False,"c")],[(True,"a"),(True,"c"),(True,"g"),(False,"b"),(False,"d"),(True,"d")],[(True,"b"),(True,"h"),(True,"h"),(True,"j"),(False,"h"),(False,"b")],[(False,"a")],[(True,"d"),(False,"g"),(True,"g"),(False,"b"),(True,"f")],[(True,"g"),(False,"c"),(False,"e"),(False,"i"),(True,"h")],[]]
cnfEx82 :: CNF
cnfEx82 = [[(True,"c"),(True,"d")],[(False,"e"),(False,"e"),(True,"b"),(True,"b"),(True,"b")],[(False,"a"),(True,"e"),(False,"e"),(False,"c"),(False,"e"),(True,"e"),(False,"a")],[(True,"d"),(False,"e"),(False,"b"),(True,"d"),(True,"c"),(False,"c"),(False,"d"),(True,"c")],[(True,"d")],[(False,"b"),(False,"c"),(True,"e"),(True,"b"),(True,"e"),(True,"c"),(True,"c"),(True,"a"),(True,"a")],[(True,"d"),(True,"d")],[(False,"b")],[(True,"e"),(True,"b"),(True,"d"),(False,"a"),(True,"a"),(True,"c")]]
cnfEx83 :: CNF
cnfEx83 = [[(False,"c"),(False,"g")],[(True,"a"),(False,"j"),(True,"i"),(False,"h"),(True,"a"),(True,"j"),(True,"g"),(True,"f"),(False,"g")],[(True,"f"),(True,"a"),(True,"f"),(True,"c"),(False,"j"),(True,"e"),(False,"f")],[(False,"i"),(True,"j"),(True,"g"),(False,"h"),(True,"e"),(True,"j"),(False,"i"),(True,"a"),(False,"h")],[(False,"c"),(False,"a"),(True,"b"),(False,"a"),(False,"f"),(True,"b"),(False,"c"),(True,"f"),(False,"e")],[(False,"d")],[(False,"c"),(True,"e"),(True,"b")]]
cnfEx84 :: CNF
cnfEx84 = [[(False,"b"),(False,"d"),(True,"e"),(False,"d"),(True,"e"),(False,"c"),(True,"c")],[(True,"e"),(False,"a"),(True,"c"),(False,"d"),(True,"b"),(True,"e"),(True,"b"),(True,"c"),(True,"e")],[(True,"b"),(True,"e"),(True,"b"),(True,"a"),(False,"c"),(False,"c")],[(False,"e")],[],[(False,"d")],[(False,"b"),(False,"c"),(False,"b"),(False,"e"),(True,"b"),(False,"a"),(False,"a"),(True,"b"),(True,"e")],[(True,"b"),(True,"e"),(True,"b"),(False,"b")]]
cnfEx85 :: CNF
cnfEx85 = [[(False,"j"),(False,"h"),(True,"f"),(True,"j"),(False,"b"),(False,"h"),(True,"d"),(True,"g"),(True,"b")],[(False,"b")],[(False,"d"),(False,"i"),(False,"c")],[(False,"d"),(False,"j"),(True,"j"),(False,"j"),(False,"b")],[(True,"j"),(True,"b"),(False,"g"),(True,"d"),(False,"a"),(False,"f"),(True,"e"),(True,"c")]]
cnfEx86 :: CNF
cnfEx86 = [[(True,"e"),(False,"d")],[],[],[(True,"d"),(True,"d"),(True,"a"),(True,"d"),(True,"b"),(False,"c"),(True,"b"),(True,"b")],[(False,"a"),(False,"c"),(False,"a")],[(True,"b"),(True,"b"),(False,"c"),(False,"c"),(False,"e"),(True,"b")],[(True,"a"),(True,"e"),(False,"b"),(True,"d"),(False,"a"),(False,"e")],[(True,"c")],[(True,"d"),(True,"e"),(False,"d")]]
cnfEx87 :: CNF
cnfEx87 = [[(False,"j"),(False,"i"),(False,"j"),(False,"h"),(False,"f"),(True,"f"),(True,"g"),(False,"h")]]
cnfEx88 :: CNF
cnfEx88 = [[(False,"d"),(True,"b"),(True,"b"),(False,"a"),(False,"d"),(False,"a"),(True,"c"),(False,"c"),(False,"a")]]
cnfEx89 :: CNF
cnfEx89 = [[(False,"g"),(False,"f")],[(True,"e"),(True,"j"),(True,"c")],[(True,"i"),(False,"f"),(True,"b"),(False,"a"),(False,"f"),(True,"d")],[(False,"c")],[(False,"f"),(True,"g"),(False,"c"),(True,"a"),(True,"i"),(True,"a"),(True,"f"),(True,"j")]]
cnfEx90 :: CNF
cnfEx90 = [[(True,"e"),(True,"d"),(False,"e"),(True,"a"),(False,"e"),(True,"d"),(True,"e"),(False,"b")],[],[(True,"c"),(True,"a"),(False,"c"),(False,"d"),(True,"b"),(False,"d"),(True,"e"),(True,"a")],[(True,"a"),(True,"c"),(True,"d")],[(False,"e"),(False,"d"),(False,"a"),(True,"d"),(True,"c"),(False,"a"),(True,"c"),(False,"e"),(True,"a")],[(True,"d"),(True,"a"),(False,"d"),(False,"e"),(True,"c"),(True,"d"),(True,"d"),(False,"b")],[(False,"b"),(True,"e"),(True,"b"),(False,"b"),(False,"a")],[(True,"c"),(True,"e"),(True,"b"),(True,"b")],[(False,"a"),(True,"c"),(True,"c"),(True,"d"),(True,"d"),(True,"b"),(True,"b"),(False,"a"),(True,"c")]]
cnfEx91 :: CNF
cnfEx91 = [[(True,"e"),(False,"e")],[(False,"b"),(True,"d"),(True,"g"),(False,"d"),(False,"f"),(True,"g"),(True,"c"),(True,"f")],[(True,"c"),(False,"d"),(True,"i"),(True,"d"),(False,"f"),(False,"g"),(False,"b"),(True,"d"),(False,"b")],[(False,"b"),(False,"h")],[(True,"f"),(False,"c"),(True,"a"),(False,"e"),(False,"e")],[(True,"e"),(False,"e"),(False,"i"),(True,"a"),(False,"d"),(False,"d")],[(False,"e"),(True,"h"),(False,"f")],[(False,"b"),(False,"j"),(False,"f"),(True,"j"),(True,"g"),(True,"f"),(False,"c")]]
cnfEx92 :: CNF
cnfEx92 = [[(False,"d"),(False,"c"),(False,"b")],[(True,"a"),(False,"e"),(False,"e")],[]]
cnfEx93 :: CNF
cnfEx93 = [[(False,"j"),(True,"h"),(False,"i"),(False,"d"),(False,"d"),(True,"j"),(True,"d"),(True,"c")],[(False,"a"),(True,"i"),(False,"i"),(False,"e"),(False,"e"),(True,"b"),(False,"g"),(False,"h")],[(False,"b"),(True,"b"),(True,"g"),(False,"f"),(False,"i"),(True,"c")],[(False,"f"),(True,"e")],[(False,"f"),(True,"f"),(True,"b"),(True,"j"),(True,"b")],[(False,"d")],[(False,"f"),(True,"d"),(True,"e"),(False,"h"),(True,"j"),(False,"d")],[(False,"e"),(True,"g"),(False,"c"),(False,"g"),(False,"d"),(True,"b"),(True,"c"),(False,"a")]]
cnfEx94 :: CNF
cnfEx94 = [[(False,"e"),(False,"d"),(False,"c"),(True,"a"),(False,"a"),(True,"d")],[(True,"c"),(True,"a"),(True,"e"),(False,"b"),(True,"a"),(True,"e"),(False,"d"),(False,"d")],[(False,"c"),(False,"b"),(False,"a"),(False,"c"),(True,"b"),(True,"b"),(True,"a")],[(False,"b")],[(True,"c"),(True,"e"),(True,"c"),(True,"b"),(False,"a")],[(False,"c"),(True,"b"),(False,"d")],[(False,"b"),(False,"d"),(True,"c")]]
cnfEx95 :: CNF
cnfEx95 = [[(True,"b"),(False,"a"),(False,"h"),(True,"i"),(True,"h"),(False,"h"),(False,"j"),(False,"a")],[(False,"e")],[(False,"j")]]
cnfEx96 :: CNF
cnfEx96 = [[(False,"a"),(False,"c"),(False,"a"),(True,"e"),(False,"b")]]
cnfEx97 :: CNF
cnfEx97 = [[(False,"g"),(False,"j"),(False,"h"),(False,"c"),(False,"g"),(True,"a"),(False,"b")],[(False,"g"),(False,"f"),(True,"c"),(True,"e"),(True,"i")],[(True,"i"),(False,"j"),(True,"a"),(False,"h"),(False,"a"),(False,"b"),(False,"b"),(True,"e")],[(False,"c"),(True,"f"),(False,"i"),(False,"f")],[(False,"f"),(False,"f"),(True,"c"),(True,"a"),(False,"a"),(True,"c"),(True,"h"),(False,"i"),(True,"i")],[(True,"c"),(False,"a"),(True,"a"),(True,"g"),(True,"h"),(False,"j"),(False,"i")]]
cnfEx98 :: CNF
cnfEx98 = [[(True,"e"),(True,"e"),(True,"b"),(False,"d"),(False,"c")],[(True,"b"),(False,"a"),(False,"a"),(True,"a"),(True,"c"),(True,"c"),(True,"e")],[(True,"a"),(False,"c"),(False,"c")],[(True,"b"),(False,"c"),(False,"c"),(True,"b"),(True,"c"),(True,"a"),(True,"e")],[(True,"a"),(False,"a"),(False,"b")]]
cnfEx99 :: CNF
cnfEx99 = [[(True,"c"),(True,"h"),(False,"c"),(False,"e"),(True,"g"),(True,"c"),(True,"j")],[(True,"f"),(True,"a"),(False,"e")],[(True,"d"),(False,"f"),(True,"h")],[(False,"c"),(False,"i"),(True,"f"),(False,"j"),(False,"g"),(False,"a"),(False,"e"),(True,"a")],[(True,"e"),(True,"g"),(False,"h")],[(False,"g"),(False,"a"),(False,"h")],[(False,"e"),(False,"i"),(False,"h"),(True,"h"),(True,"j"),(False,"j"),(True,"g"),(False,"b")],[(False,"h"),(False,"b"),(True,"b"),(False,"b"),(True,"f"),(False,"b"),(True,"f"),(False,"h")],[(True,"i")]]
cnfEx100 :: CNF
cnfEx100 = [[(False,"e"),(True,"d"),(False,"b"),(False,"a"),(False,"a"),(True,"a"),(False,"d"),(True,"a")],[(False,"e"),(False,"d"),(True,"e"),(True,"a"),(False,"b"),(True,"c"),(False,"d")],[(False,"d"),(False,"a"),(False,"c"),(False,"a"),(True,"a"),(False,"a"),(True,"b"),(True,"a")],[(True,"a"),(False,"d"),(False,"d"),(False,"c"),(True,"a"),(True,"c")],[(True,"e"),(True,"d"),(True,"d"),(True,"d"),(False,"b"),(True,"d")],[],[(True,"c"),(False,"d"),(True,"e"),(True,"c"),(True,"d"),(True,"b"),(True,"d"),(False,"e")],[(False,"d"),(False,"d"),(True,"d"),(False,"a"),(True,"e"),(True,"b")]]
cnfExs :: [(String,CNF)]
cnfExs = [("cnfEx01",cnfEx01),("cnfEx02",cnfEx02),("cnfEx03",cnfEx03),("cnfEx04",cnfEx04),("cnfEx05",cnfEx05),("cnfEx06",cnfEx06),("cnfEx07",cnfEx07),("cnfEx08",cnfEx08),("cnfEx09",cnfEx09),("cnfEx10",cnfEx10),("cnfEx11",cnfEx11),("cnfEx12",cnfEx12),("cnfEx13",cnfEx13),("cnfEx14",cnfEx14),("cnfEx15",cnfEx15),("cnfEx16",cnfEx16),("cnfEx17",cnfEx17),("cnfEx18",cnfEx18),("cnfEx19",cnfEx19),("cnfEx20",cnfEx20),("cnfEx21",cnfEx21),("cnfEx22",cnfEx22),("cnfEx23",cnfEx23),("cnfEx24",cnfEx24),("cnfEx25",cnfEx25),("cnfEx26",cnfEx26),("cnfEx27",cnfEx27),("cnfEx28",cnfEx28),("cnfEx29",cnfEx29),("cnfEx30",cnfEx30),("cnfEx31",cnfEx31),("cnfEx32",cnfEx32),("cnfEx33",cnfEx33),("cnfEx34",cnfEx34),("cnfEx35",cnfEx35),("cnfEx36",cnfEx36),("cnfEx37",cnfEx37),("cnfEx38",cnfEx38),("cnfEx39",cnfEx39),("cnfEx40",cnfEx40),("cnfEx41",cnfEx41),("cnfEx42",cnfEx42),("cnfEx43",cnfEx43),("cnfEx44",cnfEx44),("cnfEx45",cnfEx45),("cnfEx46",cnfEx46),("cnfEx47",cnfEx47),("cnfEx48",cnfEx48),("cnfEx49",cnfEx49),("cnfEx50",cnfEx50),("cnfEx51",cnfEx51),("cnfEx52",cnfEx52),("cnfEx53",cnfEx53),("cnfEx54",cnfEx54),("cnfEx55",cnfEx55),("cnfEx56",cnfEx56),("cnfEx57",cnfEx57),("cnfEx58",cnfEx58),("cnfEx59",cnfEx59),("cnfEx60",cnfEx60),("cnfEx61",cnfEx61),("cnfEx62",cnfEx62),("cnfEx63",cnfEx63),("cnfEx64",cnfEx64),("cnfEx65",cnfEx65),("cnfEx66",cnfEx66),("cnfEx67",cnfEx67),("cnfEx68",cnfEx68),("cnfEx69",cnfEx69),("cnfEx70",cnfEx70),("cnfEx71",cnfEx71),("cnfEx72",cnfEx72),("cnfEx73",cnfEx73),("cnfEx74",cnfEx74),("cnfEx75",cnfEx75),("cnfEx76",cnfEx76),("cnfEx77",cnfEx77),("cnfEx78",cnfEx78),("cnfEx79",cnfEx79),("cnfEx80",cnfEx80),("cnfEx81",cnfEx81),("cnfEx82",cnfEx82),("cnfEx83",cnfEx83),("cnfEx84",cnfEx84),("cnfEx85",cnfEx85),("cnfEx86",cnfEx86),("cnfEx87",cnfEx87),("cnfEx88",cnfEx88),("cnfEx89",cnfEx89),("cnfEx90",cnfEx90),("cnfEx91",cnfEx91),("cnfEx92",cnfEx92),("cnfEx93",cnfEx93),("cnfEx94",cnfEx94),("cnfEx95",cnfEx95),("cnfEx96",cnfEx96),("cnfEx97",cnfEx97),("cnfEx98",cnfEx98),("cnfEx99",cnfEx99),("cnfEx100",cnfEx100)]

{-
fmlaExs :: [(String,Fmla)]
fmlaExs = []
nnfExs :: [(String,NNF)]
nnfExs = []
cnfExs :: [(String,CNF)]
cnfExs = []
-}