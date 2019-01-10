{-# OPTIONS -Wall -Wno-unused-imports #-}

module GTicTacToe where

import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map

import Text.Read (readMaybe)
-- readMaybe :: Read a => String -> Maybe a

data Player = X | O
  deriving (Show, Eq)

data Board = Board Int Int Int (Map.Map (Int,Int) (Maybe Player))
  deriving (Show, Eq)
-- Invariant:: forall (Board m n k brd). m > 0 && n > 0 && k > 1 &&
--                                       Map.keys brd == [(i,j) | i <- [0..m-1], j <- [0..n-1]

valid :: Board -> Bool
valid (Board m n k brd) =
  m > 0 && n > 0 && k > 1 &&
  Map.keys brd == [(i,j) | i <- [0..m-1], j <- [0..n-1]]


new :: Int -> Int -> Int -> Board
new = undefined


draw :: Board -> String
draw = undefined


kiars :: Board -> [[((Int, Int), Maybe Player)]]
kiars = undefined


promptForAndValidate :: Read a => String -> (a -> Bool) -> IO a
promptForAndValidate = undefined


play :: Board -> Player -> IO ()
play = undefined


main :: IO ()
main = do putStrLn "GTicTacToe\n"
          m <- promptForAndValidate "m" (>0)
          n <- promptForAndValidate "n" (>0)
          k <- promptForAndValidate "k" (>1)
          play (new m n k) X
