{-# LANGUAGE OverloadedStrings #-}

module Data.CSS.Syntax.Tokens
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
import           Data.Scientific
import           Numeric

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

    | String !Char !Text
    | BadString !Char !Text

    | Number !Text !NumericValue
    | Percentage !Text !NumericValue
    | Dimension !Text !NumericValue !Unit

    | Url !Text
    | BadUrl !Text

    | Ident !Text

    | AtKeyword !Text

    | Function !Text

    | Hash !HashFlag !Text

    | Delim !Char

    deriving (Show, Eq)


data NumericValue
    = NVInteger !Scientific
    | NVNumber !Scientific
    deriving (Show, Eq)

data HashFlag = HId | HUnrestricted
    deriving (Show, Eq)

type Unit = Text



-- Tokenization
-------------------------------------------------------------------------------


-- | Parse a 'Text' into a list of 'Token's.
--
-- https://drafts.csswg.org/css-syntax/#tokenization

tokenize :: Text -> Either String [Token]
tokenize = parseOnly (many' parseToken) . preprocessInputStream



-- | Before sending the input stream to the tokenizer, implementations must
-- make the following code point substitutions: (see spec)
--
-- https://drafts.csswg.org/css-syntax/#input-preprocessing

preprocessInputStream :: Text -> Text
preprocessInputStream = T.pack . f . T.unpack
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

serialize :: [Token] -> Text
serialize = mconcat . map renderToken


renderToken :: Token -> Text
renderToken (Whitespace)         = " "

renderToken (CDO)                = "<!--"
renderToken (CDC)                = "-->"

renderToken (Comma)              = ","
renderToken (Colon)              = ":"
renderToken (Semicolon)          = ";"

renderToken (LeftParen)          = "("
renderToken (RightParen)         = ")"
renderToken (LeftSquareBracket)  = "["
renderToken (RightSquareBracket) = "]"
renderToken (LeftCurlyBracket)   = "{"
renderToken (RightCurlyBracket)  = "}"

renderToken (SuffixMatch)        = "$="
renderToken (SubstringMatch)     = "*="
renderToken (PrefixMatch)        = "^="
renderToken (DashMatch)          = "|="
renderToken (IncludeMatch)       = "~="

renderToken (Column)             = "||"

renderToken (String d x)         = T.singleton d <> renderString x <> T.singleton d
renderToken (BadString d x)      = T.singleton d <> renderString x <> T.singleton d

renderToken (Number x _)         = x
renderToken (Percentage x _)     = x <> "%"
renderToken (Dimension x _ u)    = x <> u

renderToken (Url x)              = "url(" <> x <> ")"
renderToken (BadUrl x)           = "url(" <> x <> ")"

renderToken (Ident x)            = x

renderToken (AtKeyword x)        = "@" <> x

renderToken (Function x)         = x <> "("

renderToken (Hash _ x)           = "#" <> x

renderToken (Delim x)            = T.singleton x



renderString :: Text -> Text
renderString = T.pack . concatMap f . T.unpack
  where
    nonPrintableCodePoint c
        | c >= '\x0000' && c <= '\x0008' = True -- NULL through BACKSPACE
        | c == '\x000B'                  = True -- LINE TABULATION
        | c >= '\x000E' && c <= '\x001F' = True -- SHIFT OUT through INFORMATION SEPARATOR ONE
        | c == '\x007F'                  = True -- DELETE
        | otherwise                      = False

    nonASCIICodePoint c = c >= '\x0080' -- control

    f c = if nonPrintableCodePoint c || nonASCIICodePoint c
        then "\\" <> showHex (ord c) ""
        else [c]


parseComment :: Parser ()
parseComment = do
    void $ AP.string "/*"
    void $ AP.manyTill' AP.anyChar (void (AP.string "*/") <|> AP.endOfInput)

parseWhitespace :: Parser Token
parseWhitespace = do
    AP.skip isWhitespace
    AP.skipWhile isWhitespace
    return Whitespace
{-# INLINE parseWhitespace #-}


parseChar :: Token -> Char -> Parser Token
parseChar t c = do
    _ <- AP.char c
    return t
{-# INLINE parseChar #-}

parseStr :: Token -> Text -> Parser Token
parseStr t str = AP.string str *> return t
{-# INLINE parseStr #-}


escapedCodePoint :: Parser Char
escapedCodePoint = do
    mbChar <- AP.peekChar
    case mbChar of
        Nothing -> return $ '\xFFFD'
        Just ch -> do
         if isHexChar ch
          then do
            (t, _) <- AP.runScanner [] f
            case unhex (T.unpack t) of
                Nothing -> fail $ "escapedCodePoint: unable to parse hex " ++ (T.unpack t)
                Just cp -> do
                    AP.peekChar >>= \c -> case c of
                        Just nc -> if isWhitespace nc then void AP.anyChar else return ()
                        _ -> return ()
                    return $ if cp == 0 || cp > 0x10FFFF
                      then chr 0xFFFD
                      else chr cp
          else do
            if ch == '\n'
                then fail "A newline"
                else AP.anyChar >> return ch

  where
    f :: String -> Char -> Maybe String
    f acc c =
        if length acc < 6 && isHexChar c
            then Just $ c:acc
            else Nothing


nextInputCodePoint :: Parser Char
nextInputCodePoint = escapedCodePoint' <|> AP.anyChar
{-# INLINE nextInputCodePoint #-}


whenNext :: Char -> a -> Parser a
whenNext c a = do
    mbChar <- AP.peekChar
    if mbChar == Just c
        then return a
        else fail "whenNext"

-- 4.3.4. Consume a string token
parseString :: Char -> Parser Token
parseString endingCodePoint = do
    _ <- AP.char endingCodePoint
    go mempty

  where
    go acc = choice
        [ (AP.endOfInput <|> void (AP.char endingCodePoint)) *> return (String endingCodePoint acc)
        , AP.string "\\\n" *> go acc
        , whenNext '\n' (BadString endingCodePoint acc)
        , nextInputCodePoint >>= \ch -> go (acc <> T.singleton ch)
        ]


parseHash :: Parser Token
parseHash = do
    _ <- AP.char '#'
    name <- parseName
    return $ Hash HId name

isNameStartCodePoint :: Char -> Bool
isNameStartCodePoint c = isLetter c || c >= '\x0080' || c == '_'
{-# INLINE isNameStartCodePoint #-}

isNameCodePoint :: Char -> Bool
isNameCodePoint c = isNameStartCodePoint c || isDigit c || c == '-'
{-# INLINE isNameCodePoint #-}

parseNumeric :: Parser Token
parseNumeric = do
    (repr, nv) <- parseNumericValue
    dimNum repr nv <|> pctNum repr nv <|> return (Number repr nv)
  where
    dimNum repr nv = do
        unit <- parseName
        return $ Dimension repr nv unit
    pctNum repr nv = do
        _ <- AP.char '%'
        return $ Percentage repr nv

nameCodePoint :: Parser Char
nameCodePoint = AP.satisfy isNameCodePoint
{-# INLINE nameCodePoint #-}

escapedCodePoint' :: Parser Char
escapedCodePoint' = do
    _ <- AP.char '\\'
    escapedCodePoint

parseName :: Parser Text
parseName = do
    chars <- AP.many1' $
        nameCodePoint <|> escapedCodePoint'

    case chars of
        '-':xs -> case xs of
            _:_ -> return $ T.pack chars
            _ -> fail "parseName: Not a valid name start"
        _ -> return $ T.pack chars


parseSign :: Parser (Text, Integer)
parseSign = do
    mbChar <- AP.peekChar
    case mbChar of
        Just '+' -> AP.anyChar >> return ("+", 1)
        Just '-' -> AP.anyChar >> return ("-", (-1))
        _        -> return ("", 1)

isDecimal :: Char -> Bool
isDecimal c = c >= '0' && c <= '9'
{-# INLINE isDecimal #-}

parseDigits :: Parser (Text, Integer)
parseDigits = do
    text <- AP.takeWhile1 isDecimal
    pure (text, T.foldl' step 0 text)

  where
    step a c = a * 10 + fromIntegral (ord c - 48)


parseNumericValue :: Parser (Text, NumericValue)
parseNumericValue = do
    -- Sign (optional)
    (sS, s) <- parseSign

    -- Digits before the decimal dot. They are optional (".1em").
    (iS, i) <- parseDigits <|> pure ("",0)

    -- Decimal dot and digits after it. If the decimal dot is there then it
    -- MUST be followed by one or more digits. This is not allowed: "1.".
    (fS, f, fB) <- option ("", 0, False) $ do
        void $ AP.char '.'
        (digits, n) <- parseDigits
        pure ("." <> digits, fromIntegral n, True)

    -- Exponent (with optional sign).
    (tS, t, eS, e, eB) <- option ("", 1, "", 0, False) $ do
        e <- AP.char 'E' <|> AP.char 'e'
        (tS, t) <- parseSign
        (eS, eN) <- parseDigits

        return (T.singleton e <> tS, t, eS, eN, True)

    let repr = sS<>iS<>fS<>tS<>eS
    if T.null repr || repr == "-" || repr == "+" || T.head repr == 'e' || T.head repr == 'E'
        then fail "parseNumericValue: no parse"
        else do
            let v = fromIntegral s * (fromIntegral i + f*10^^(-(T.length fS - 1))) * 10^^(t*e)
            return $ if fB || eB
                then (repr, NVNumber v)
                else (repr, NVInteger v)


parseUrl :: Parser Token
parseUrl = do
    _ <- AP.takeWhile isWhitespace
    go mempty

  where
    endOfUrl acc = (AP.endOfInput <|> void (AP.char ')')) *> return (Url acc)

    go acc = choice
        [ endOfUrl acc
        , (AP.char '"' <|> AP.char '\'' <|> AP.char '(') >>= \ch -> badUrl (acc <> T.singleton ch)
        , AP.string "\\\n" *> badUrl (acc <> "\\\n")
        , AP.takeWhile1 isWhitespace >>= \c -> (endOfUrl acc <|> badUrl (acc <> c))
        , nextInputCodePoint >>= \ch -> go (acc <> T.singleton ch)
        ]

    badUrl acc = choice
        [ (AP.endOfInput <|> void (AP.char ')')) *> return (BadUrl acc)
        , nextInputCodePoint >>= \ch -> badUrl (acc <> T.singleton ch)
        ]


parseIdentLike :: Parser Token
parseIdentLike = do
    name <- parseName
    choice
        [ do
            -- Special handling of url() functions (they are not really
            -- functions, they have their own Token type).
            guard $ T.isPrefixOf "url" (T.map toLower name)

            void $ AP.char '('
            void $ AP.takeWhile isWhitespace

            whenNext '"' (Function name) <|> whenNext '\'' (Function name) <|> parseUrl

        , AP.char '(' *> return (Function name)
        , return (Ident name)
        ]


parseEscapedIdentLike :: Parser Token
parseEscapedIdentLike = do
    mbChar <- AP.peekChar
    case mbChar of
        Just '\\' -> parseIdentLike <|> (AP.anyChar >> return (Delim '\\'))
        _         -> fail "parseEscapedIdentLike: Does not start with an escape code"

parseAtKeyword :: Parser Token
parseAtKeyword = do
    _ <- AP.char '@'
    name <- parseName
    return $ AtKeyword name


parseToken :: Parser Token
parseToken = AP.many' parseComment *> choice
    [ parseWhitespace

    , AP.string "<!--" *> return CDO
    , AP.string "-->" *> return CDC

    , parseChar Comma ','
    , parseChar Colon ':'
    , parseChar Semicolon ';'
    , parseChar LeftParen '('
    , parseChar RightParen ')'
    , parseChar LeftSquareBracket '['
    , parseChar RightSquareBracket ']'
    , parseChar LeftCurlyBracket '{'
    , parseChar RightCurlyBracket '}'

    , parseStr SuffixMatch "$="
    , parseStr SubstringMatch "*="
    , parseStr PrefixMatch "^="
    , parseStr DashMatch "|="
    , parseStr IncludeMatch "~="

    , parseStr Column "||"

    , parseNumeric

    , parseEscapedIdentLike
    , parseIdentLike
    , parseHash

    , parseString '"'
    , parseString '\''

    , parseAtKeyword

    , AP.anyChar >>= return . Delim
    ] <?> "token"



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
