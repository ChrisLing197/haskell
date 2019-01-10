{-# OPTIONS -Wall -Wno-unused-imports #-}

module Cipher where

{-
Useful Prelude types and functions.

lines :: String -> [String]
-- `lines` breaks a string up into a list of strings at newline characters. The resulting strings do not contain newlines.

unlines :: [String] -> String
-- `unlines` is an inverse operation to `lines`. It joins lines, after appending a terminating newline to each.

type FilePath = String
-- File and directory names are values of type `String`.

readFile :: FilePath -> IO String
-- The `readFile` function reads a file and returns the contents of the file as a string

writeFile :: FilePath -> String -> IO ()
-- The computation `writeFile file str` function writes the string `str`, to the file `file`.
-}

import Data.Char (ord,chr,isAsciiUpper,isAsciiLower)
{-
Useful Data.Char functions.

ord :: Char -> Int
-- The `fromEnum` method restricted to the type `Char`.

chr :: Int -> Char
-- The `toEnum` method restricted to the type `Char`.

isAsciiLower :: Char -> Bool
-- Selects ASCII lower-case letters

isAsciiUpper :: Char -> Bool
-- Selects ASCII upper-case letters
-}

import System.Environment (getArgs)
{-
Useful Stytem.Environment functions.

getArgs :: IO [String]
-- Computation `getArgs` returns a list of the program's command line arguments (not including the program name).
Note: When executing a Haskell program as `runhaskell Prog.hs arg1 arg2 arg3`, `getArgs` will return `[arg1, arg2, arg3]`.
-}

import Text.Read (readMaybe)
{-
Useful Text.Read functions.

readMaybe :: Read a => String -> Maybe a
-- Parse a string using the `Read` instance. Succeeds if there is exactly one valid result.
-}

import Test.HUnit


fruitsPlainText :: String
fruitsPlainText = "1. apple!\n2. banana?\n3. cherry$\n4. date#\n"
fruitsCipher0Text :: String
fruitsCipher0Text = "4. date#\n3. cherry$\n2. banana?\n1. apple!\n"
fruitsCipher1Text :: String
fruitsCipher1Text = "4. hexi#\n3. fkhuub$\n2. dcpcpc?\n1. bqqmf!\n"
fruitsCipher7Text :: String
fruitsCipher7Text = "4. fcvg#\n3. xczmmt$\n2. pobobo?\n1. hwwsl!\n"
fruitsCipher19Text :: String
fruitsCipher19Text = "4. byrc#\n3. hmjwwd$\n2. nmzmzm?\n1. tiiex!\n"


encrypt :: Int -> String -> String
encrypt = undefined

encryptTests :: Test
encryptTests =
  TestList [encrypt 0 fruitsPlainText ~?= fruitsCipher0Text,
            encrypt 1 fruitsPlainText ~?= fruitsCipher1Text,
            encrypt 7 fruitsPlainText ~?= fruitsCipher7Text,
            encrypt 19 fruitsPlainText ~?= fruitsCipher19Text]


decrypt :: Int -> String -> String
decrypt = undefined

decryptTests :: Test
decryptTests =
  TestList [decrypt 0 fruitsCipher0Text ~?= fruitsPlainText,
            decrypt 1 fruitsCipher1Text ~?= fruitsPlainText,
            decrypt 7 fruitsCipher7Text ~?= fruitsPlainText,
            decrypt 19 fruitsCipher19Text ~?= fruitsPlainText]


main :: IO ()
main = putStrLn usage
  where usage = "Usage: Cipher {enc|dec} key inFile outFile"

{-

$ runhaskell Cipher.hs enc 7 gettysburg.txt gettysburg.enc7

$ runhaskell Cipher.hs dec 7 gettysburg.enc7 gettysburg.txt

$ runhaskell Cipher.hs
Usage: Cipher {enc|dec} key inFile outFile

$ runhaskell Cipher.hs dec seven gettysburg.enc7 gettysburg.txt
Usage: Cipher {enc|dec} key inFile outFile

$ runhaskell Cipher.hs dec 99 gettysburg.enc7 gettysburg.txt
Usage: Cipher {enc|dec} key inFile outFile

-}
