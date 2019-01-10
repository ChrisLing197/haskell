{-

Name: Christain Ling
Time spent on assignment: 15 hours
Collaborators/Acknowledgements: getting the pdf explained really basically helped me understand what I think we were required to do.

-}

{-# OPTIONS -Wall -Wno-unused-imports -Wno-unused-do-bind #-}

module SXMLReformat where

{-
Useful Prelude types and functions.

getContents :: IO String
-}

import Data.Char

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


import Parser
import PrettyPrint


newtype SXML = SXML Elt deriving (Eq, Read, Show)

data Elt = Elt String [Att] [Item] deriving (Eq, Read, Show)
-- Invariant:: forall (Elt n atts items) . validName n

data Att = Att String String deriving (Eq, Read, Show)
-- Invariant:: forall (Att n v) . validName n && validAttValue v

data Item = IElt Elt | IText String deriving (Eq, Read, Show)
-- Invariant:: forall (IText s) . validText s

validName :: String -> Bool
validName []     = False
validName (c:cs) = nameStartChar c && all nameChar cs
    where nameStartChar = (== ':') ||| (== '_') ||| isAlpha
          nameChar = nameStartChar ||| (== '-') ||| (== '.') ||| isDigit

validAttValue :: String -> Bool
validAttValue = all ((/= '<') &&& (/= '>') &&& (/= '"') &&& (not . isSpace ||| (== ' ')))

validText :: String -> Bool
validText = (not . null) &&& all ((/= '<') &&& (/= '>') &&& (not . isSpace))

infixr 2 |||
(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f ||| g = \ x -> f x || g x
infixr 3 &&&
(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f &&& g = \ x -> f x && g x



sxmlP :: Parser SXML
sxmlP = do _ <- manyL misc
           e <- elt
           _ <- manyL misc
           return (SXML e)

  where misc = (skipMany1L comments) +++ (skipMany1L space)
        comments = do _       <- string "<!--"
                      content <- manyA get
                      _       <- string "-->"
                      if not $ any (Data.List.isPrefixOf "--") (Data.List.tails content)
                        then return content
                        else pfail

        elt = emptyTagElt <++ startEndTagElt

        att = do nam <- name
                 _   <- string "=">>(manyL space>>string "\"")
                 cha <- manyL (satisfy (\c -> c /= '<' &&c/='>'&&c/='\"' || (isSpace c&&c==' ')))
                 _   <- char '\"'
                 return (Att nam cha)
        name = do hea   <- letter +++ satisfy (\c -> c=='_' || c==':')
                  follo <-manyL (alphanum +++ satisfy (\c -> c=='_' || c=='-' || c=='.' || c==':'))
                  return (hea:follo)
        emptyTagElt = do _    <- char '<'
                         nam  <- name
                         atts <- manyL (manyL space>>att)
                         _    <- skipSpaces >> string "/>"
                         return (Elt nam atts [])
        startEndTagElt = do (na,at) <- startTag
                            items   <- manyL (manyL misc >> ((IElt <$> elt) +++ ((IText <$> (many1L (satisfy (\c -> c/='<' && c/='>'&& not (isSpace c))))))))
                            _       <- manyL misc
                            et      <- endTag
                            if et == na
                              then return (Elt na at items)
                              else pfail
        startTag = do _ <- char '<'
                      n <- name
                      s <- manyL (many1L space >> att)
                      _ <- skipSpaces>>char '>'
                      return (n,s)
        endTag = do _ <- string "</"
                    n <- name
                    _ <- skipSpaces>>char '>'
                    return n



sxmlD :: SXML -> Doc
sxmlD (SXML elt) = nest 0 (eltD elt)

  where eltD (Elt name atts [])    = text "<" <+> (text name) <+> text " " <+> (attD atts) <+> text "/>" <+> lbreak
        eltD (Elt name atts items) = text "<" <+> (text name) <+> text " "<+>attD atts <+> text ">"<+>lbreak <+> nest 4 (itemD items) <+> lbreak <+> text "</"<+>(text name)<+> text ">"
        attD atts= pack (map (\(Att name avalue) -> (text name) <+> (text "=\"" <+> text avalue<+>text "\"")) atts)
        itemD []     = empty
        itemD (ielt) = pack (map (\ (i)->
                                    case i of
                                      IElt ie -> nest 4 (lbreak<+>eltD ie <+>lbreak)
                                      IText ie -> nest 4 (text ie) ) ielt)


{-

Describe your `sxmlD`.  That is, give a description of the pretty-printed SXML
documents produced by `sxmlD`.  Comment on any design decisions that you made in
developing `sxmlD`.  Comment on any "ugly" aspects that remain in pretty-printed
SXML documents produced by `sxmlD`.

-----

sxmlD pretty prints based on the premise of building tags and if there is another elt found, nest it 4 spaces and surround that nest with line breaks for spacing. 
The ugliest design aspect is that tabbing of IText has an extra indent.

-}



main :: IO ()
main = do args     <- getArgs
          contents <- getContents
          let sxml = parse sxmlP contents
          let intParsed = readMaybe (head args)
          if length sxml == 0
            then print "** PARSE ERROR **"
            else if length args == 0
                   then pprint 80 (sxmlD ((\ (x,_) -> x) (head (sxml))))
                   else case intParsed of
                          Just i  -> if i < 1
                                       then pprint 80 (sxmlD ((\ (x,_) -> x) (head sxml)))
                                       else pprint i (sxmlD ((\ (x,_) -> x) (head (sxml))))
                          Nothing -> pprint 80 (sxmlD ((\ (x,_) -> x) (head sxml)))


{-

$ runhaskell SXMLReformat < gettysburg.sxml

$ runhaskell sXMLReformat 40 < gettysburg.sxml

-}