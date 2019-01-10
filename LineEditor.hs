{-# OPTIONS -Wall -Wno-unused-imports #-}

module LineEditor where

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

import System.Environment (getArgs)
{-
(Possibly) Useful Stytem.Environment functions.

getArgs :: IO [String]
-- Computation `getArgs` returns a list of the program's command line arguments (not including the program name).
Note: When executing a Haskell program as `runhaskell Prog.hs arg1 arg2 arg3`, `getArgs` will return `[arg1, arg2, arg3]`.
-}

import System.IO (hSetBuffering, stdout)

import Text.Read (readMaybe)
{-
Useful Text.Read functions.

readMaybe :: Read a => String -> Maybe a
-- Parse a string using the `Read` instance. Succeeds if there is exactly one valid result.
-}


main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStrLn "Line Editor\n"


{-

README:

-}
