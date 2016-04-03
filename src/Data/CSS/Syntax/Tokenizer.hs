{-# LANGUAGE OverloadedStrings #-}

module Data.CSS.Syntax.Tokenizer
    ( Token(..)
    , NumericValue(..)
    , HashFlag(..)
    , Unit

    , tokenize
    , serialize
    ) where


import           Control.Applicative
import           Control.Monad

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Attoparsec.Text as AP
import           Data.Monoid
import           Data.Char
import           Numeric

import           Data.CSS.Syntax.Tokenizer.Types

import           Prelude



-- Tokenization
-------------------------------------------------------------------------------


-- | Parse a 'Text' into a list of 'Token's.
--
-- https://drafts.csswg.org/css-syntax/#tokenization

tokenize :: String -> Either String [Token]
tokenize = Right . parseToken . preprocessInputStream

parseToken :: String -> [Token]
parseToken [] = []
parseToken (x:xs)
    | isWhitespace x = Whitespace : parseToken (dropWhile isWhitespace xs)
    | x == '"' = consumeStringToken '"' xs
    | otherwise = Delim x : parseToken xs


consumeStringToken :: Char -> String -> [Token]
consumeStringToken endingCodePoint = go ""
  where
    go acc [] = [String endingCodePoint acc]
    go acc input@(x:xs)
        | x == endingCodePoint = (String endingCodePoint acc) : parseToken xs
        | x == '\n' = (BadString endingCodePoint acc) : parseToken input
        | x == '\\' = case xs of
            [] -> go acc xs
            ('\n' : rest) -> go acc rest
        | otherwise = go (acc ++ [x]) xs


-- | Before sending the input stream to the tokenizer, implementations must
-- make the following code point substitutions: (see spec)
--
-- https://drafts.csswg.org/css-syntax/#input-preprocessing

preprocessInputStream :: String -> String
preprocessInputStream = f
  where
    f []                    = []

    f ('\x000D':'\x000A':r) = '\x000A' : f r

    f ('\x000D':r)          = '\x000A' : f r
    f ('\x000C':r)          = '\x000A' : f r

    f ('\x0000':r)          = '\xFFFD' : f r

    f (x:r)                 = x : f r



-- Serialization
-------------------------------------------------------------------------------


-- | Serialize a list of 'Token's back into 'Text'. Round-tripping is not
-- guaranteed to be identity. The tokenization step drops some information
-- from the source.
--
-- https://drafts.csswg.org/css-syntax/#serialization

serialize :: [Token] -> String
serialize = mconcat . map renderToken


renderToken :: Token -> String
renderToken (Whitespace)         = " "

-- renderToken (CDO)                = "<!--"
-- renderToken (CDC)                = "-->"
--
-- renderToken (Comma)              = ","
-- renderToken (Colon)              = ":"
-- renderToken (Semicolon)          = ";"
--
-- renderToken (LeftParen)          = "("
-- renderToken (RightParen)         = ")"
-- renderToken (LeftSquareBracket)  = "["
-- renderToken (RightSquareBracket) = "]"
-- renderToken (LeftCurlyBracket)   = "{"
-- renderToken (RightCurlyBracket)  = "}"
--
-- renderToken (SuffixMatch)        = "$="
-- renderToken (SubstringMatch)     = "*="
-- renderToken (PrefixMatch)        = "^="
-- renderToken (DashMatch)          = "|="
-- renderToken (IncludeMatch)       = "~="
--
-- renderToken (Column)             = "||"
--
-- renderToken (String d x)         = [d] <> renderString x <> [d]
-- renderToken (BadString d x)      = [d] <> renderString x <> [d]
--
-- renderToken (Number x _)         = T.unpack x
-- renderToken (Percentage x _)     = x <> "%"
-- renderToken (Dimension x _ u)    = x <> u
--
-- renderToken (Url x)              = "url(" <> x <> ")"
-- renderToken (BadUrl x)           = "url(" <> x <> ")"
--
-- renderToken (Ident x)            = x
--
-- renderToken (AtKeyword x)        = "@" <> x
--
-- renderToken (Function x)         = x <> "("
--
-- renderToken (Hash _ x)           = "#" <> x
--
-- renderToken (Delim x)            = T.singleton x



isWhitespace :: Char -> Bool
isWhitespace '\x0009' = True
isWhitespace '\x000A' = True
isWhitespace '\x0020' = True
isWhitespace _        = False
{-# INLINE isWhitespace #-}


isHexChar :: Char -> Bool
isHexChar ch
    | ch >= '0' && ch <= '9' = True
    | ch >= 'A' && ch <= 'F' = True
    | ch >= 'a' && ch <= 'f' = True
    | otherwise              = False


unhex :: (Functor m, Monad m) => String -> m Int
unhex = fmap toInt . go []
  where

    go :: Monad m => [Int] -> String -> m [Int]
    go acc []    = return acc
    go acc (a:r) = do
        x <- c a
        go (x:acc) r

    toInt = sum . map (\(e, x) -> 16 ^ e * x) . zip [(0::Int)..]

    c :: Monad m => Char -> m Int
    c '0' = return 0
    c '1' = return 1
    c '2' = return 2
    c '3' = return 3
    c '4' = return 4
    c '5' = return 5
    c '6' = return 6
    c '7' = return 7
    c '8' = return 8
    c '9' = return 9
    c 'A' = return 10
    c 'B' = return 11
    c 'C' = return 12
    c 'D' = return 13
    c 'E' = return 14
    c 'F' = return 15
    c 'a' = return 10
    c 'b' = return 11
    c 'c' = return 12
    c 'd' = return 13
    c 'e' = return 14
    c 'f' = return 15
    c _   = fail "Invalid hex digit!"
