{-# LANGUAGE OverloadedStrings #-}

module Data.CSS.Syntax.Tokenizer.Types
    ( Token(..)
    , NumericValue(..)
    , HashFlag(..)
    , Unit
    ) where


import           Data.Text (Text)
import           Data.Char
import           Data.Scientific

import           Prelude


data Token
    = Whitespace

    | CDO -- CommentDelimiterOpen
    | CDC -- CommentDelimiterClose

    | Comma
    | Colon
    | Semicolon

    | LeftParen
    | RightParen
    | LeftSquareBracket
    | RightSquareBracket
    | LeftCurlyBracket
    | RightCurlyBracket

    | SuffixMatch
    | SubstringMatch
    | PrefixMatch
    | DashMatch
    | IncludeMatch

    | Column

    | String !Char !String
    | BadString !Char !String

    | Number !String !NumericValue
    | Percentage !String !NumericValue
    | Dimension !String !NumericValue !Unit

    | Url !String
    | BadUrl !String

    | Ident !String

    | AtKeyword !Text

    | Function !Text

    | Hash !HashFlag !String

    | Delim !Char

    deriving (Show, Eq)


data NumericValue
    = NVInteger !Scientific
    | NVNumber !Scientific
    deriving (Show, Eq)

data HashFlag = HId | HUnrestricted
    deriving (Show, Eq)

type Unit = Text
