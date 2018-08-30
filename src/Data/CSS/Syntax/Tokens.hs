{-# LANGUAGE OverloadedStrings, RankNTypes, PatternSynonyms, ViewPatterns,
             BangPatterns #-}

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

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import           Data.Monoid
import           Data.Char
import           Data.Scientific
import           Numeric

import           Prelude

import           Data.Text.Internal (Text(..))
import qualified Data.Text.Array as A
import           Control.Monad.ST (ST)
import           GHC.Base (unsafeChr)
import           Data.Word (Word16)
import           Data.Char (ord)
import           Data.Bits


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
tokenize = Right . parseTokens . preprocessInputStream



-- | Before sending the input stream to the tokenizer, implementations must
-- make the following code point substitutions: (see spec)
--
-- https://drafts.csswg.org/css-syntax/#input-preprocessing

preprocessInputStream :: Text -> Text
preprocessInputStream t0@(Text _ _ len) = withNewA len $ \ dst -> do
    let go t d = case t of
            '\x0D' :. '\x0A' :. t' ->
                put '\x0A' t'
            '\x0D' :. t' ->
                put '\x0A' t'
            '\x0C' :. t' ->
                put '\x0A' t'
            '\x00' :. t' ->
                put '\xFFFD' t'
            c :. t' ->
                put c t'
            _ ->
                return d
            where put x t' = do
                      write dst d x
                      go t' (d + 1)
    go t0 0


-- Low level utilities
-------------------------------------------------------------------------------

pattern (:.) :: Char -> Text -> Text
pattern x :. xs <- (uncons -> Just (x, xs))

infixr 5 :.

-- | uncons first Word16 from Text without trying to decode UTF-16 sequence
uncons :: Text -> Maybe (Char, Text)
uncons (Text src offs len)
    | len <= 0 = Nothing
    | otherwise =
      Just (w2c (A.unsafeIndex src offs), Text src (offs+1) (len-1))
{-# INLINE uncons #-}

write :: A.MArray s -> Int -> Char -> ST s ()
write dst d x = A.unsafeWrite dst d (c2w x)
{-# INLINE write #-}

type Writer' s = (A.MArray s -> Int -> ST s Int, Text)
type Writer s = A.MArray s -> Int -> ST s (Int, Text)

-- | no-op for convenient pattern matching
w2c :: Word16 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

c2w :: Char -> Word16
c2w = fromIntegral . ord
{-# INLINE c2w #-}

withNewA :: Int -> (forall s . A.MArray s -> ST s Int) -> Text
withNewA len act = Text a 0 l
    where (a, l) = A.run2 $ do
              dst <- A.new len
              dLen <- act dst
              return (dst, dLen)


-- Serialization
-------------------------------------------------------------------------------


-- | Serialize a list of 'Token's back into 'Text'. Round-tripping is not
-- guaranteed to be identity. The tokenization step drops some information
-- from the source.
--
-- https://drafts.csswg.org/css-syntax/#serialization

-- TODO: serialization must insert /**/ between adjacent idents
-- (look at the table in the linked section)

serialize :: [Token] -> Text
serialize = TL.toStrict . TLB.toLazyText . foldr (<>) "" . map renderToken


renderToken :: Token -> TLB.Builder
renderToken token = case token of
    Whitespace         -> " "

    CDO                -> "<!--"
    CDC                -> "-->"

    Comma              -> ","
    Colon              -> ":"
    Semicolon          -> ";"

    LeftParen          -> "("
    RightParen         -> ")"
    LeftSquareBracket  -> "["
    RightSquareBracket -> "]"
    LeftCurlyBracket   -> "{"
    RightCurlyBracket  -> "}"

    SuffixMatch        -> "$="
    SubstringMatch     -> "*="
    PrefixMatch        -> "^="
    DashMatch          -> "|="
    IncludeMatch       -> "~="

    Column             -> "||"

    String d x         ->
        TLB.singleton d <> t (renderString x) <> TLB.singleton d
    BadString d x      ->
        TLB.singleton d <> t (renderString x) <> TLB.singleton d

    Number x _         -> t x
    Percentage x _     -> t x <> "%"
    Dimension x _ u    -> t x <> t u

    Url x              -> "url(" <> t x <> ")"
    BadUrl x           -> "url(" <> t x <> ")"

    Ident x            -> t x

    AtKeyword x        -> "@" <> t x

    Function x         -> t x <> "("

    Hash _ x           -> "#" <> t x

    Delim x            -> TLB.singleton x
    where t = TLB.fromText


renderString :: Text -> Text
renderString t0@(Text _ _ l)
    | T.any needEscape t0 = withNewA (l*7) $ go t0 0
    | otherwise = t0
  where
    go t d dst = case T.uncons t of
        Nothing -> return d
        Just (c, t')
            | needEscape c -> do
                write dst d '\\'
                d' <- foldM (\ o x -> write dst o x >> return (d+1))
                    (d+1) (showHex (ord c) [])
                go t' d' dst
            | otherwise -> do
                write dst d c
                go t' (d+1) dst

    needEscape c = nonPrintableCodePoint c || nonASCIICodePoint c
    nonPrintableCodePoint c
        | c >= '\x0000' && c <= '\x0008' = True -- NULL through BACKSPACE
        | c == '\x000B'                  = True -- LINE TABULATION
        | c >= '\x000E' && c <= '\x001F' = True -- SHIFT OUT through INFORMATION SEPARATOR ONE
        | c == '\x007F'                  = True -- DELETE
        | otherwise                      = False

    nonASCIICodePoint c = c >= '\x0080' -- control


escapedCodePoint :: Text -> Maybe (Writer' s)
escapedCodePoint t = case t of
    (hex -> Just d) :. ts -> go 5 d ts
    '\n' :. _ -> Nothing
    c :. ts -> Just (\ dst d -> write dst d c >> return (d+1), ts)
    _ -> Just (\ dst d -> write dst d '\xFFFD' >> return (d+1), t)
    where go :: Int -> Int -> Text -> Maybe (Writer' s)
          go 0 acc ts = ret acc ts
          go n acc ts = case ts of
              (hex -> Just d) :. ts' -> go (n-1) (acc*16 + d) ts'
              c :. ts' | isWhitespace c -> ret acc ts'
              _ -> ret acc ts
          ret (safe -> c) ts
              | c < 0x10000 = Just
                  (\ dst d -> write dst d (unsafeChr c) >> return (d+1), ts)
              | otherwise = Just
                  (\ dst d -> write dst d lo >> write dst (d+1) hi >> return (d+2)
                  ,ts)
              where m = c - 0x10000
                    lo = unsafeChr $ (m `shiftR` 10) + 0xD800
                    hi = unsafeChr $ (m .&. 0x3FF) + 0xDC00

safe :: Int -> Int
safe x
    | x == 0 || x > 0x10FFFF   = 0xFFFD
    | x .&. 0x1ff800 /= 0xd800 = x
    | otherwise                = 0xFFFD -- UTF16 surrogate code point

hex :: Char -> Maybe Int
hex c
    | c >= '0' && c <= '9' = Just (ord c - ord '0')
    | c >= 'a' && c <= 'f' = Just (ord c - ord 'a' + 10)
    | c >= 'A' && c <= 'F' = Just (ord c - ord 'A' + 10)
    | otherwise            = Nothing

{-# INLINE safe #-}
{-# INLINE hex #-}

escapedCodePoint' :: Text -> Maybe (Writer' s)
escapedCodePoint' ('\\' :. ts) = escapedCodePoint ts
escapedCodePoint' _ = Nothing

isNameStartCodePoint :: Char -> Bool
isNameStartCodePoint c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
    c >= '\x0080' || c == '_'

isNameCodePoint :: Char -> Bool
isNameCodePoint c = isNameStartCodePoint c || (c >= '0' && c <= '9') || c == '-'

nameCodePoint :: Text -> Maybe (Writer' s)
nameCodePoint (c :. ts) | isNameCodePoint c =
    Just (\ dst d -> write dst d c >> return (d+1), ts)
nameCodePoint _ = Nothing

codePoint :: Text -> Maybe (Writer' s)
codePoint ts = nameCodePoint ts <|> escapedCodePoint' ts

parseName :: Text -> Maybe (Writer s)
parseName t = case t of
    '-' :. ts -> getName' <$> codePoint ts
    ts -> getName <$> codePoint ts
    where getName' n dst d = do
              write dst d '-'
              getName n dst (d + 1)
          getName (w, ts') dst d = do
              d' <- w dst d
              loop ts' dst d'
          loop ts dst d = case codePoint ts of
              Just (w, ts') -> do
                  d' <- w dst d
                  loop ts' dst d'
              Nothing -> return (d, ts)

{-# INLINE parseName #-}
{-# INLINE nameCodePoint #-}
{-# INLINE escapedCodePoint #-}
{-# INLINE escapedCodePoint' #-}
{-# INLINE codePoint #-}

parseNumericValue :: Text -> Maybe (Text, NumericValue, Text)
parseNumericValue t0@(Text a offs1 _) = case withSign start t0 of
    Just (nv, ts@(Text _ offs2 _)) ->
        Just (Text a offs1 (offs2 - offs1), nv, ts)
    Nothing -> Nothing
    where start sign t = case t of
              '.' :. (digit -> Just d) :. ts -> dot sign d (-1) ts
              (digit -> Just d) :. ts        -> digits sign d ts
              _ -> Nothing
          digits sign !c t = case t of
              '.' :. (digit -> Just d) :. ts -> dot sign (c*10 + d) (-1) ts
              (digit -> Just d) :. ts        -> digits sign (c*10 + d) ts
              _ -> Just $ expn NVInteger (sign c) 0 t
          dot sign !c !e t = case t of
              (digit -> Just d) :. ts        -> dot sign (c*10 + d) (e-1) ts
              _ -> Just $ expn NVNumber (sign c) e t
          expn f c e0 t = case t of
              x :. ts
                  | x == 'e' || x == 'E'
                  , Just r <- withSign (expStart c e0 0) ts -> r
              _ -> (f $ scientific c e0, t)
          expStart c e0 e sign t = case t of
              (digit -> Just d) :. ts -> expDigits c e0 (e*10 + d) sign ts
              _ -> Nothing
          expDigits c e0 !e sign t = case t of
              (digit -> Just d) :. ts -> expDigits c e0 (e*10 + d) sign ts
              _ -> Just (NVNumber $ scientific c (sign e + e0), t)
          digit :: Enum a => Char -> Maybe a
          digit c
              | c >= '0' && c <= '9' = Just (toEnum $ ord c - ord '0')
              | otherwise = Nothing
          withSign :: Num a => ((a -> a) -> Text -> Maybe (b, Text))
                   -> Text -> Maybe (b, Text)
          withSign f t = case t of
              '+' :. ts -> f id ts
              '-' :. ts -> f negate ts
              _ -> f id t


skipComment :: Text -> Text
skipComment t = case t of
    '*' :. '/' :. ts -> ts
    _ :. ts -> skipComment ts
    ts -> ts

skipWhitespace :: Text -> Text
skipWhitespace t = case t of
    c :. ts
        | isWhitespace c -> skipWhitespace ts
        | otherwise -> t
    ts -> ts

parseTokens :: Text -> [Token]
parseTokens t0@(Text _ _ len) = snd $ A.run2 $ do
    dst <- A.new len
    dsta <- A.unsafeFreeze dst
    let go' !t acc d tgo = go (t : acc) d tgo
        go !acc d tgo = case tgo of
            c :. ts | isWhitespace c ->
                 go' Whitespace acc d (skipWhitespace ts)
            '/' :. '*' :. ts -> go acc d (skipComment ts)

            '<' :. '!' :. '-' :. '-' :. ts -> token CDO ts
            '-' :. '-' :. '>' :. ts ->        token CDC ts

            ',' :. ts -> token Comma ts
            ':' :. ts -> token Colon ts
            ';' :. ts -> token Semicolon ts
            '(' :. ts -> token LeftParen ts
            ')' :. ts -> token RightParen ts
            '[' :. ts -> token LeftSquareBracket ts
            ']' :. ts -> token RightSquareBracket ts
            '{' :. ts -> token LeftCurlyBracket ts
            '}' :. ts -> token RightCurlyBracket ts

            '$' :. '=' :. ts -> token SuffixMatch ts
            '*' :. '=' :. ts -> token SubstringMatch ts
            '^' :. '=' :. ts -> token PrefixMatch ts
            '|' :. '=' :. ts -> token DashMatch ts
            '~' :. '=' :. ts -> token IncludeMatch ts

            '|' :. '|' :. ts -> token Column ts

            (parseNumericValue -> Just (repr, nv, ts))
                | '%' :. ts' <- ts ->
                    go' (Percentage repr nv) acc d ts'
                | Just u <- parseName ts -> do
                    (unit, d', ts') <- mkText dst d u
                    go' (Dimension repr nv unit) acc d' ts'
                | otherwise ->
                    go' (Number repr nv) acc d ts

            -- ident like
            (parseName -> Just n) -> do
                (name, d', ts) <- mkText dst d n
                if isUrl name then
                    -- Special handling of url() functions (they are not really
                    -- functions, they have their own Token type).
                    case ts of
                        '(' :. (skipWhitespace -> ts') ->
                            case ts' of
                                '"'  :. _ -> go' (Function name) acc d' ts'
                                '\'' :. _ -> go' (Function name) acc d' ts'
                                _ -> parseUrl acc d' ts'
                        _ -> go' (Ident name) acc d' ts
                else
                    case ts of
                        '(' :. ts' -> go' (Function name) acc d' ts'
                        _ -> go' (Ident name) acc d' ts

            '"' :. ts -> parseString '"' acc d ts
            '\'' :. ts -> parseString '\'' acc d ts

            '@' :. (parseName -> Just n) -> do
                (name, d', ts) <- mkText dst d n
                go' (AtKeyword name) acc d' ts

            '#' :. (parseName -> Just n) -> do
                (name, d', ts) <- mkText dst d n
                go' (Hash HId name) acc d' ts

            c :. ts ->
                token (Delim c) ts
            _ -> return (dst, reverse acc)

            where token !t ts = go (t : acc) d ts

        isUrl t@(Text _ _ 3)
            | u :. r :. l :. _ <- t =
                (u == 'u' || u == 'U') &&
                (r == 'r' || r == 'R') &&
                (l == 'l' || l == 'L')
        isUrl _ = False

        parseString endingCodePoint acc d0 = string d0
            where string d t = case t of
                      c :. ts | c == endingCodePoint -> ret String d ts
                      '\\' :. '\n' :. ts -> string d ts
                      '\n' :. _ -> ret BadString d t
                      -- TODO: following code is repeated 3 times,
                      -- generalize and profile
                      (escapedCodePoint' -> Just (p, ts)) -> do
                          d' <- p dst d
                          string d' ts
                      c :. ts -> do
                          write dst d c
                          string (d+1) ts
                      _ -> ret String d t
                  ret f d ts =
                      go' (f endingCodePoint (Text dsta d0 (d-d0))) acc d ts
        parseUrl acc d0 tUrl = url d0 (skipWhitespace tUrl)
            where ret f d ts = go' (f (Text dsta d0 (d-d0))) acc d ts
                  url d t = case t of
                      ')' :. ts -> ret Url d ts
                      c :. ts
                          | c == '"' || c == '\'' || c == '(' -> do
                              write dst d c
                              badUrl (d+1) ts
                          | isWhitespace c -> do
                              write dst d c
                              whitespace d (d+1) ts
                      '\\' :. '\n' :. ts -> do
                          write dst d '\\'
                          write dst (d+1) '\n'
                          badUrl (d+2) ts
                      (escapedCodePoint' -> Just (p, ts)) -> do
                          d' <- p dst d
                          url d' ts
                      c :. ts -> do
                          write dst d c
                          url (d+1) ts
                      _ ->
                          ret Url d t
                  whitespace dw d t = case t of
                      c :. ts -> do
                          write dst d c
                          if isWhitespace c then
                              whitespace dw (d+1) ts
                          else if c == ')' then
                              ret Url dw ts
                          else
                              badUrl (d+1) ts
                      _ ->
                          ret Url dw t
                  badUrl d t = case t of
                      ')' :. ts -> ret BadUrl d ts
                      (escapedCodePoint' -> Just (p, ts)) -> do
                          d' <- p dst d
                          badUrl d' ts
                      c :. ts -> do
                          write dst d c
                          badUrl (d+1) ts
                      _ -> ret BadUrl d t
        mkText :: A.MArray s -> Int -> Writer s -> ST s (Text, Int, Text)
        mkText dest d w = do
            (d', ts) <- w dest d
            return (Text dsta d (d' - d), d', ts)

    go [] 0 t0


isWhitespace :: Char -> Bool
isWhitespace '\x0009' = True
isWhitespace '\x000A' = True
isWhitespace '\x0020' = True
isWhitespace _        = False
