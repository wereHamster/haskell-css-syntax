{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Data.Monoid
import           Data.CSS.Syntax.Tokens
import           Test.Hspec
import           Prelude



main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do

    describe "Data.CSS.Syntax.Tokens" $ do
        it "Single Character" $ do
            tokenize "(" `shouldBe` Right [LeftParen]
            tokenize ")" `shouldBe` Right [RightParen]
            tokenize "[" `shouldBe` Right [LeftSquareBracket]
            tokenize "]" `shouldBe` Right [RightSquareBracket]
            tokenize ",," `shouldBe` Right [Comma, Comma]

        it "Multiple Character" $ do
            tokenize "~=" `shouldBe` Right [IncludeMatch]
            tokenize "||" `shouldBe` Right [Column]
            tokenize "|||" `shouldBe` Right [Column, Delim '|']
            tokenize "<!--" `shouldBe` Right [CDO]
            tokenize "<!---" `shouldBe` Right [CDO, Delim '-']
            tokenize "<!---->" `shouldBe` Right [CDO, CDC]
            tokenize "<!-- -->" `shouldBe` Right [CDO, Whitespace, CDC]

        it "Delimiter" $ do
            tokenize "^" `shouldBe` Right [Delim '^']
            tokenize "|" `shouldBe` Right [Delim '|']
            tokenize "\x7f" `shouldBe` Right [Delim '\x7f']
            tokenize "\1" `shouldBe` Right [Delim '\x1']
            tokenize "~-" `shouldBe` Right [Delim '~', Delim '-']
            tokenize "*^" `shouldBe` Right [Delim '*', Delim '^']

        it "Whitespace" $ do
            tokenize "     " `shouldBe` Right [Whitespace]
            tokenize "\n\rS" `shouldBe` Right [Whitespace, Ident "S"]
            tokenize "    *" `shouldBe` Right [Whitespace, Delim '*']
            tokenize "\n\r\f2" `shouldBe` Right [Whitespace, Number "2" (NVInteger 2)]

        it "Escapes" $ do
            tokenize "hel\\6Co" `shouldBe` Right [Ident "hello"]
            tokenize "\\26 B" `shouldBe` Right [Ident "&B"]
            tokenize "'hel\\6c o'" `shouldBe` Right [String '\'' "hello"]
            tokenize "'spac\\65\r\ns'" `shouldBe` Right [String '\'' "spaces"]
            tokenize "spac\\65\r\ns" `shouldBe` Right [Ident "spaces"]
            tokenize "spac\\65\n\rs" `shouldBe` Right [Ident "space", Whitespace, Ident "s"]
            tokenize "sp\\61\tc\\65\fs" `shouldBe` Right [Ident "spaces"]
            tokenize "hel\\6c  o" `shouldBe` Right [Ident "hell", Whitespace, Ident "o"]
            tokenize "test\\\n" `shouldBe` Right [Ident "test", Delim '\\', Whitespace]
            tokenize "test\\D799" `shouldBe` Right [Ident "test\xD799"]
            tokenize "\\E000" `shouldBe` Right [Ident "\xe000"]
            tokenize "te\\s\\t" `shouldBe` Right [Ident "test"]
            tokenize "spaces\\ in\\\tident" `shouldBe` Right [Ident "spaces in\tident"]
            tokenize "\\.\\,\\:\\!" `shouldBe` Right [Ident ".,:!"]
            tokenize "\\\r" `shouldBe` Right [Delim '\\', Whitespace]
            tokenize "\\\f" `shouldBe` Right [Delim '\\', Whitespace]
            tokenize "\\\r\n" `shouldBe` Right [Delim '\\', Whitespace]
            -- let replacement = "\xFFFD"
            tokenize "null\\\0" `shouldBe` Right [Ident "null\xfffd"]
            tokenize "null\\\0\0" `shouldBe` Right [Ident $ "null" <> "\xfffd" <> "\xfffd"]
            tokenize "null\\0" `shouldBe` Right [Ident $ "null" <> "\xfffd"]
            tokenize "null\\0000" `shouldBe` Right [Ident $ "null" <> "\xfffd"]
            tokenize "large\\110000" `shouldBe` Right [Ident $ "large" <> "\xfffd"]
            tokenize "large\\23456a" `shouldBe` Right [Ident $ "large" <> "\xfffd"]
            tokenize "surrogate\\D800" `shouldBe` Right [Ident $ "surrogate" <> "\xfffd"]
            tokenize "surrogate\\0DABC" `shouldBe` Right [Ident $ "surrogate" <> "\xfffd"]
            tokenize "\\00DFFFsurrogate" `shouldBe` Right [Ident $ "\xfffd" <> "surrogate"]
            tokenize "\\10fFfF" `shouldBe` Right [Ident "\x10ffff"]
            tokenize "\\10fFfF0" `shouldBe` Right [Ident $ "\x10ffff" <> "0"]
            tokenize "\\10000000" `shouldBe` Right [Ident $ "\x100000" <> "00"]
            tokenize "eof\\" `shouldBe` Right [Ident "eof\xfffd"]

        it "Ident" $ do
            tokenize "simple-ident" `shouldBe` Right [Ident "simple-ident"]
            tokenize "testing123" `shouldBe` Right [Ident "testing123"]
            tokenize "hello!" `shouldBe` Right [Ident "hello", Delim '!']
            tokenize "world\5" `shouldBe` Right [Ident "world", Delim '\5']
            tokenize "_under score" `shouldBe` Right [Ident "_under", Whitespace, Ident "score"]
            tokenize "-_underscore" `shouldBe` Right [Ident "-_underscore"]
            tokenize "-text" `shouldBe` Right [Ident "-text"]
            tokenize "-\\6d" `shouldBe` Right [Ident "-m"]
            tokenize "--abc" `shouldBe` Right [Ident "--abc"]
            tokenize "--" `shouldBe` Right [Ident "--"]
            tokenize "--11" `shouldBe` Right [Ident "--11"]
            tokenize "---" `shouldBe` Right [Ident "---"]
            tokenize "\x2003" `shouldBe` Right [Ident "\x2003"] -- em-space
            tokenize "\xA0" `shouldBe` Right [Ident "\xA0"]  -- non-breaking space
            tokenize "\x1234" `shouldBe` Right [Ident "\x1234"]
            tokenize "\x12345" `shouldBe` Right [Ident "\x12345"]
            tokenize "\0" `shouldBe` Right [Ident "\xfffd"]
            tokenize "ab\0c" `shouldBe` Right [Ident $ "ab\xfffd" <> "c"]

        it "Function" $ do
            tokenize "scale(2)" `shouldBe` Right [Function "scale", Number "2" (NVInteger 2), RightParen]
            tokenize "foo-bar\\ baz(" `shouldBe` Right [Function "foo-bar baz"]
            tokenize "fun\\(ction(" `shouldBe` Right [Function "fun(ction"]
            tokenize "-foo(" `shouldBe` Right [Function "-foo"]
            tokenize "url(\"foo.gif\"" `shouldBe` Right [Function "url", String  '"' "foo.gif"]
            tokenize "foo(  \'bar.gif\'" `shouldBe` Right [Function "foo", Whitespace, String '\'' "bar.gif"]
            -- // To simplify implementation we drop the whitespace in function(url),whitespace,string()
            tokenize "url(  \'bar.gif\'" `shouldBe` Right [Function "url", String '\'' "bar.gif"]

        it "AtKeyword" $ do
            tokenize "@at-keyword" `shouldBe` Right [AtKeyword "at-keyword"]
            tokenize "@hello!" `shouldBe` Right [AtKeyword "hello", Delim '!']
            tokenize "@-text" `shouldBe` Right [AtKeyword "-text"]
            tokenize "@--abc" `shouldBe` Right [AtKeyword "--abc"]
            tokenize "@--" `shouldBe` Right [AtKeyword "--"]
            tokenize "@--11" `shouldBe` Right [AtKeyword "--11"]
            tokenize "@---" `shouldBe` Right [AtKeyword "---"]
            tokenize "@\\ " `shouldBe` Right [AtKeyword " "]
            tokenize "@-\\ " `shouldBe` Right [AtKeyword "- "]
            tokenize "@@" `shouldBe` Right [ Delim '@', Delim '@']
            -- tokenize "@2" `shouldBe` Right [  Delim '@', Number "2" (NVInteger 2)]
            -- tokenize "@-1" `shouldBe` Right [  Delim '@', Number "-1" (NVInteger (-1))]


        it "Url" $ do
            tokenize "url(foo.gif)" `shouldBe` Right [Url "foo.gif"]
            tokenize "urL(https://example.com/cats.png)" `shouldBe` Right [Url "https://example.com/cats.png"]
            tokenize "uRl(what-a.crazy^URL~this\\ is!)" `shouldBe` Right [Url "what-a.crazy^URL~this is!"]
            tokenize "uRL(123#test)" `shouldBe` Right [Url "123#test"]
            tokenize "Url(escapes\\ \\\"\\'\\)\\()" `shouldBe` Right [Url "escapes \"')("]
            tokenize "UrL(   whitespace   )" `shouldBe` Right [Url "whitespace"]
            tokenize "URl( whitespace-eof " `shouldBe` Right [Url "whitespace-eof"]
            tokenize "URL(eof" `shouldBe` Right [Url "eof"]
            tokenize "url(not/*a*/comment)" `shouldBe` Right [Url "not/*a*/comment"]
            tokenize "urL()" `shouldBe` Right [Url ""]
            tokenize "uRl(white space)," `shouldBe` Right [BadUrl "white space", Comma]
            tokenize "Url(b(ad)," `shouldBe` Right [BadUrl "b(ad", Comma]
            tokenize "uRl(ba'd):" `shouldBe` Right [BadUrl "ba'd", Colon]
            tokenize "urL(b\"ad):" `shouldBe` Right [BadUrl "b\"ad", Colon]
            tokenize "uRl(b\"ad):" `shouldBe` Right [BadUrl "b\"ad", Colon]
            tokenize "Url(b\\\rad):" `shouldBe` Right [BadUrl "b\\\nad", Colon]
            tokenize "url(b\\\nad):" `shouldBe` Right [BadUrl "b\\\nad", Colon]
            tokenize "url(/*'bad')*/" `shouldBe` Right [BadUrl "/*'bad'", Delim '*', Delim '/']
            tokenize "url(ba'd\\\\))" `shouldBe` Right [BadUrl "ba'd\\", RightParen]

        it "String" $ do
            tokenize "'text'" `shouldBe` Right [String '\'' "text"]
            tokenize "\"text\"" `shouldBe` Right [String '"' "text"]
            tokenize "'testing, 123!'" `shouldBe` Right [String '\'' "testing, 123!"]
            tokenize "'es\\'ca\\\"pe'" `shouldBe` Right [String '\'' "es'ca\"pe"]
            tokenize "'\"quotes\"'" `shouldBe` Right [String '\'' "\"quotes\""]
            tokenize "\"'quotes'\"" `shouldBe` Right [String '"' "'quotes'"]
            tokenize "\"mismatch'" `shouldBe` Right [String '"' "mismatch'"]
            tokenize "'text\5\t\xb'" `shouldBe` Right [String '\'' "text\5\t\xb"]
            tokenize "\"end on eof" `shouldBe` Right [String '"' "end on eof"]
            tokenize "'esca\\\nped'" `shouldBe` Right [String '\'' "escaped"]
            tokenize "\"esc\\\faped\"" `shouldBe` Right [String '"' "escaped"]
            tokenize "'new\\\rline'" `shouldBe` Right [String '\'' "newline"]
            tokenize "\"new\\\r\nline\"" `shouldBe` Right [String '"' "newline"]
            tokenize "'bad\nstring" `shouldBe` Right [BadString '\'' "bad", Whitespace, Ident "string"]
            tokenize "'bad\rstring" `shouldBe` Right [BadString '\'' "bad", Whitespace, Ident "string"]
            tokenize "'bad\r\nstring" `shouldBe` Right [BadString '\'' "bad", Whitespace, Ident "string"]
            tokenize "'bad\fstring" `shouldBe` Right [BadString '\'' "bad", Whitespace, Ident "string"]
            tokenize "'\0'" `shouldBe` Right [String '\'' "\xFFFD"]
            tokenize "'hel\0lo'" `shouldBe` Right [String '\'' "hel\xfffdlo"]
            tokenize "'h\\65l\0lo'" `shouldBe` Right [String '\'' "hel\xfffdlo"]

        it "Hash" $ do
            tokenize "#id-selector" `shouldBe` Right [Hash HId "id-selector"]
            tokenize "#FF7700" `shouldBe` Right [Hash HId "FF7700"]
            -- tokenize "#3377FF" `shouldBe` Right [Hash HUnrestricted "3377FF"]
            tokenize "#\\ " `shouldBe` Right [Hash HId " "]
            tokenize "# " `shouldBe` Right [Delim '#', Whitespace]
            tokenize "#\\\n" `shouldBe` Right [Delim '#', Delim '\\', Whitespace]
            tokenize "#\\\r\n" `shouldBe` Right [Delim '#', Delim '\\', Whitespace]
            tokenize "#!" `shouldBe` Right [Delim '#', Delim '!']

        it "Number" $ do
            tokenize "10" `shouldBe` Right [Number "10" (NVInteger 10)]
            tokenize "12.0" `shouldBe` Right [Number "12.0" (NVNumber 12)]
            tokenize "+45.6" `shouldBe` Right [Number "+45.6" (NVNumber 45.6)]
            tokenize "-7" `shouldBe` Right [Number "-7" (NVInteger (-7))]
            tokenize "010" `shouldBe` Right [Number "010" (NVInteger 10)]
            tokenize "10e0" `shouldBe` Right [Number "10e0" (NVNumber 10)]
            tokenize "12e3" `shouldBe` Right [Number "12e3" (NVNumber 12000)]
            tokenize "3e+1" `shouldBe` Right [Number "3e+1" (NVNumber 30)]
            tokenize "12E-1" `shouldBe` Right [Number "12E-1" (NVNumber 1.2)]
            tokenize ".7" `shouldBe` Right [Number ".7" (NVNumber 0.7)]
            tokenize "-.3" `shouldBe` Right [Number "-.3" (NVNumber (-0.3))]
            tokenize "+637.54e-2" `shouldBe` Right [Number "+637.54e-2" (NVNumber 6.3754)]
            tokenize "-12.34E+2" `shouldBe` Right [Number "-12.34E+2" (NVNumber (-1234))]
            tokenize "+ 5" `shouldBe` Right [Delim '+', Whitespace, Number "5" (NVInteger 5)]
            tokenize "-+12" `shouldBe` Right [Delim '-', Number "+12" (NVInteger 12)]
            tokenize "+-21" `shouldBe` Right [Delim '+', Number "-21" (NVInteger (-21))]
            tokenize "++22" `shouldBe` Right [Delim '+', Number "+22" (NVInteger 22)]
            tokenize "13." `shouldBe` Right [Number "13" (NVInteger 13), Delim ('.')]
            tokenize "1.e2" `shouldBe` Right [Number "1" (NVInteger 1), Delim '.', Ident "e2"]
            tokenize "2e3.5" `shouldBe` Right [Number "2e3" (NVNumber 2e3), Number ".5" (NVNumber 0.5)]
            tokenize "2e3." `shouldBe` Right [Number "2e3" (NVNumber 2e3), Delim '.']
            tokenize "1000000000000000000000000" `shouldBe` Right [Number "1000000000000000000000000" (NVInteger 1e24)]

        it "Dimension" $ do
            tokenize "10px" `shouldBe` Right [Dimension "10" (NVInteger 10) "px"]
            tokenize "12.0em" `shouldBe` Right [Dimension "12.0" (NVNumber 12) "em"]
            tokenize "-12.0em" `shouldBe` Right [Dimension "-12.0" (NVNumber (-12)) "em"]
            tokenize "+45.6__qem" `shouldBe` Right [Dimension "+45.6" (NVNumber 45.6) "__qem"]
            tokenize "5e" `shouldBe` Right [Dimension "5" (NVInteger 5) "e"]
            tokenize "5px-2px" `shouldBe` Right [Dimension "5" (NVInteger 5) "px-2px"]
            tokenize "5e-" `shouldBe` Right [Dimension "5" (NVInteger 5) "e-"]
            tokenize "5\\ " `shouldBe` Right [Dimension "5" (NVInteger 5) " "]
            tokenize "40\\70\\78" `shouldBe` Right [Dimension "40" (NVInteger 40) "px"]
            tokenize "4e3e2" `shouldBe` Right [Dimension "4e3" (NVNumber 4e3) "e2"]
            tokenize "0x10px" `shouldBe` Right [Dimension "0" (NVInteger 0) "x10px"]
            tokenize "4unit " `shouldBe` Right [Dimension "4" (NVInteger 4) "unit", Whitespace]
            tokenize "5e+" `shouldBe` Right [Dimension "5" (NVInteger 5) "e", Delim '+']
            tokenize "2e.5" `shouldBe` Right [Dimension "2" (NVInteger 2) "e", Number ".5" (NVNumber 0.5)]
            tokenize "2e+.5" `shouldBe` Right [Dimension "2" (NVInteger 2) "e", Number "+.5" (NVNumber 0.5)]

        it "Percentage" $ do
            tokenize "10%" `shouldBe` Right [Percentage "10" $ NVInteger 10]
            tokenize "+12.0%" `shouldBe` Right [Percentage "+12.0" $ NVNumber 12.0]
            tokenize "-48.99%" `shouldBe` Right [Percentage "-48.99" $ NVNumber (-48.99)]
            tokenize "6e-1%" `shouldBe` Right [Percentage "6e-1" $ NVNumber 6e-1]
            tokenize "5%%" `shouldBe` Right [Percentage "5" (NVInteger 5), Delim '%']


-- 402	TEST(CSSTokenizerTest, UnicodeRangeToken)
-- 403	{
-- 404	    TEST_TOKENS("u+012345-123456", unicodeRange(0x012345, 0x123456));
-- 405	    TEST_TOKENS("U+1234-2345", unicodeRange(0x1234, 0x2345));
-- 406	    TEST_TOKENS("u+222-111", unicodeRange(0x222, 0x111));
-- 407	    TEST_TOKENS("U+CafE-d00D", unicodeRange(0xcafe, 0xd00d));
-- 408	    TEST_TOKENS("U+2??", unicodeRange(0x200, 0x2ff));
-- 409	    TEST_TOKENS("U+ab12??", unicodeRange(0xab1200, 0xab12ff));
-- 410	    TEST_TOKENS("u+??????", unicodeRange(0x000000, 0xffffff));
-- 411	    TEST_TOKENS("u+??", unicodeRange(0x00, 0xff));
-- 412
-- 413	    TEST_TOKENS("u+222+111", unicodeRange(0x222, 0x222), number(IntegerValueType, 111, PlusSign));
-- 414	    TEST_TOKENS("u+12345678", unicodeRange(0x123456, 0x123456), number(IntegerValueType, 78, NoSign));
-- 415	    TEST_TOKENS("u+123-12345678", unicodeRange(0x123, 0x123456), number(IntegerValueType, 78, NoSign));
-- 416	    TEST_TOKENS("u+cake", unicodeRange(0xca, 0xca), ident("ke"));
-- 417	    TEST_TOKENS("u+1234-gggg", unicodeRange(0x1234, 0x1234), ident("-gggg"));
-- 418	    TEST_TOKENS("U+ab12???", unicodeRange(0xab1200, 0xab12ff), delim('?'));
-- 419	    TEST_TOKENS("u+a1?-123", unicodeRange(0xa10, 0xa1f), number(IntegerValueType, -123, MinusSign));
-- 420	    TEST_TOKENS("u+1??4", unicodeRange(0x100, 0x1ff), number(IntegerValueType, 4, NoSign));
-- 421	    TEST_TOKENS("u+z", ident("u"), delim('+'), ident("z"));
-- 422	    TEST_TOKENS("u+", ident("u"), delim('+'));
-- 423	    TEST_TOKENS("u+-543", ident("u"), delim('+'), number(IntegerValueType, -543, MinusSign));

        it "Comment" $ do
            tokenize "/*comment*/a" `shouldBe` Right [Ident "a"]
            tokenize "/**\\2f**//" `shouldBe` Right [Delim '/']
            tokenize "/**y*a*y**/ " `shouldBe` Right [Whitespace]
            tokenize ",/* \n :) \n */)" `shouldBe` Right [Comma, RightParen]
            tokenize ":/*/*/" `shouldBe` Right [Colon]
            tokenize "/**/*" `shouldBe` Right [Delim '*']
            tokenize ";/******" `shouldBe` Right [Semicolon]
