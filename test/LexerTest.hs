{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Lexer

import Control.Applicative

import Test.Tasty
import Test.Tasty.HUnit
import Data.Either
import Data.Maybe
import Data.List

inputDir, outputDir :: String
inputDir = "tests-portable/scanner/input/"
outputDir = "tests-portable/scanner/output/"

main :: IO ()
main = do
  sysTestTree <- traverse getSysTests $
    [ "char" <> show (n :: Integer) | n <- [1..15]] <>
    [ "hexlit" <> show (n :: Integer) | n <- [1, 2, 4]] <>
    [ "number" <> show (n :: Integer) | n <- [1..5]] <>
    [ "string" <> show (n :: Integer) | n <- [1..3]] <>
    [ "literal" <> show (n :: Integer) | n <- [1..6]] <>
      [ "literals" ] <>
    [ "op" <> show (n :: Integer) | n <- [1..8]] <>
    [ "id" <> show (n :: Integer) | n <- [1..5]] <>
    [ "tokens" <> show (n :: Integer) | n <- [1..6]] <>
    [ "ws" <> show (n :: Integer) | n <- [1, 2]] <>
    [ "variants" ]
  let sysTests = testGroup "System Tests" sysTestTree
  defaultMain $ testGroup "Lexer Tests" [ customTests , sysTests ]

formatToken :: RawToken -> Maybe String
formatToken RawToken {position = Position {line, column}, token}
  = case token of
      LineComment _  -> Nothing
      BlockComment _ -> Nothing
      CharLit '"'    -> Just $ unwords [show line, "CHARLITERAL", "'\\\"'"]
      CharLit c      -> Just $ unwords [show line, "CHARLITERAL", show c]
      HexLit h       -> Just $ unwords [show line, "INTLITERAL 0x" <> h]
      DecLit d       -> Just $ unwords [show line, "INTLITERAL", d]
      BoolLit b      -> Just $ unwords [show line, "BOOLEANLITERAL"
                                       , if b then "true" else "false"]
      StrLit s       -> Just $ unwords [show line, "STRINGLITERAL", show' s]
      Identifier n   -> Just $ unwords [show line, "IDENTIFIER", n]
      Bool           -> Just $ unwords [show line, "bool"]
      Int            -> Just $ unwords [show line, "int"]
      Void           -> Just $ unwords [show line, "void"]
      Import         -> Just $ unwords [show line, "import"]
      Break          -> Just $ unwords [show line, "break"]
      Continue       -> Just $ unwords [show line, "continue"]
      If             -> Just $ unwords [show line, "if"]
      Else           -> Just $ unwords [show line, "else"]
      For            -> Just $ unwords [show line, "for"]
      While          -> Just $ unwords [show line, "while"]
      Return         -> Just $ unwords [show line, "return"]
      Len            -> Just $ unwords [show line, "len"]
      And            -> Just $ unwords [show line, "&&"]
      Or             -> Just $ unwords [show line, "||"]
      Equals         -> Just $ unwords [show line, "=="]
      NEquals        -> Just $ unwords [show line, "!="]
      LessEq         -> Just $ unwords [show line, "<="]
      GreaterEq      -> Just $ unwords [show line, ">="]
      PlusGets       -> Just $ unwords [show line, "+="]
      MinusGets      -> Just $ unwords [show line, "-="]
      PlusPlus       -> Just $ unwords [show line, "++"]
      MinusMinus     -> Just $ unwords [show line, "--"]
      Not            -> Just $ unwords [show line, "!"]
      Modulo         -> Just $ unwords [show line, "%"]
      Less           -> Just $ unwords [show line, "<"]
      Greater        -> Just $ unwords [show line, ">"]
      Comma          -> Just $ unwords [show line, ","]
      LParens        -> Just $ unwords [show line, "("]
      RParens        -> Just $ unwords [show line, ")"]
      LBrace         -> Just $ unwords [show line, "{"]
      RBrace         -> Just $ unwords [show line, "}"]
      LBracket       -> Just $ unwords [show line, "["]
      RBracket       -> Just $ unwords [show line, "]"]
      Semicolon      -> Just $ unwords [show line, ";"]
      Plus           -> Just $ unwords [show line, "+"]
      Minus          -> Just $ unwords [show line, "-"]
      Times          -> Just $ unwords [show line, "*"]
      Divide         -> Just $ unwords [show line, "/"]
      Gets           -> Just $ unwords [show line, "="]
      where show' :: String -> String
            show' = escSingleQuotes . show
              where escSingleQuotes "" = ""
                    escSingleQuotes ('\'':cs) = '\\':'\'':escSingleQuotes cs
                    escSingleQuotes (c:cs) = c:escSingleQuotes cs

getSysTests :: String -> IO TestTree
getSysTests test = do
  fin  <- readFile $ inputDir <> test <> ".dcf"
  fout <- readFile $ outputDir <> test <> ".out"
  let (errs, tokens) = partitionEithers $ rawTokens (fin, Position 1 0)
  pure $ testCase test $
    if null errs
       then unlines (mapMaybe formatToken tokens) @?= fout
       else assertBool "no error expected" $
         any (test `isPrefixOf`) $ lines fout

customTests :: TestTree
customTests = testGroup "Custom tests"
  [ instanceTests
  , primitiveTests
  , literalTests
  , identifierKeywordsTests
  , symbolTests
  , commentTests
  , whitespaceTests
  ]

instanceTests :: TestTree
instanceTests = testGroup "Instances"
  [ testCase "Applicative" $
    let lexer1 = Lexer $ \s -> Right (s, True) -- do nothing
        lexer2 = Lexer $ \(c:rest, Position ln col) ->
                          Right ((rest, Position ln (col+1)), c) -- one Char
     in runLexer ((,) <$> lexer1 <*> lexer2) ("abc", Position 3 10) @?=
       Right (("bc", Position 3 11), (True, 'a'))
  , testCase "Alternative first" $
    let lexer1 = Lexer $ \s -> Right (s, 'X') -- const X
        lexer2 = Lexer $ \(_, pos) -> Left (UnexpectedEOF pos)
     in runLexer (lexer1 <|> lexer2) ("abc", Position 3 10) @?=
       Right (("abc", Position 3 10), 'X')
  , testCase "Alternative second" $
    let lexer1 = Lexer $ \(_, pos) -> Left (UnexpectedEOF pos)
        lexer2 = Lexer $ \s -> Right (s, 'X') -- const X
     in runLexer (lexer1 <|> lexer2) ("abc", Position 3 10) @?=
       Right (("abc", Position 3 10), 'X')
  , testCase "Alternative both" $
    let lexer1 = Lexer $ \s -> Right (s, 'X')
        lexer2 = Lexer $ \s -> Right (s, 'Y')
     in runLexer (lexer1 <|> lexer2) ("abc", Position 3 10) @?=
       Right (("abc", Position 3 10), 'X')
  , testCase "Alternative both (longer)" $
    let lexer1 = Lexer $ \s -> Right (s, 'X')
        lexer2 = Lexer $ \(_, p) -> Right (("", p), 'Y')
     in runLexer (lexer1 <|> lexer2) ("abc", Position 3 10) @?=
       Right (("abc", Position 3 10), 'X')
  , testCase "Alternative neither" $
    let lexer1 = Lexer @() $ const $ Left (UnexpectedEOF $ Position 0 0)
        lexer2 = Lexer     $ const $ Left (UnexpectedEOF $ Position 0 1)
     in runLexer (lexer1 <|> lexer2) ("abc", Position 3 10) @?=
       Left (UnexpectedEOF $ Position 0 0)
  , testCase "Monad accept" $
    let lexer1 = Lexer $ \(c:rest, Position ln col) ->
                          Right ((rest, Position ln (col+1)), c) -- one Char
        charL c = Lexer $ \case
          (c':rest, Position ln col) | c == c' ->
            Right ((rest, Position ln (col+1)), [c, c])
          s -> Left $ unexpected s
     in runLexer (lexer1 >>= charL) ("aac", Position 3 10) @?=
       Right (("c", Position 3 12), "aa")
  , testCase "Monad reject" $
    let lexer1 = Lexer $ \(c:rest, Position ln col) ->
                          Right ((rest, Position ln (col+1)), c) -- one Char
        charL c = Lexer $ \case
          (c':rest, Position ln col) | c == c' ->
            Right ((rest, Position ln (col+1)), [c, c])
          s -> Left $ unexpected s
     in runLexer (lexer1 >>= charL) ("abc", Position 3 10) @?=
       Left (Unexpected 'b' (Position 3 11))
  ]

primitiveTests :: TestTree
primitiveTests = testGroup "Primitives"
  [ testGroup "satisfies"
    [ testCase "accept" $
      let isVowel = (`elem` "aeiou")
       in runLexer (satisfies isVowel) ("arest", Position 1 1) @?=
         Right (("rest", Position 1 2), 'a')

    , testCase "reject" $
      let isVowel = (`elem` "aeiou")
       in runLexer (satisfies isVowel) ("xrest", Position 1 1) @?=
         Left (Unexpected 'x' (Position 1 1))
    ]
  , testGroup "char"
    [ testCase "accept" $
      runLexer (char 'c') ("cde", Position 3 10) @?=
        Right (("de", Position 3 11), 'c')

    , testCase "with \\n" $
      runLexer (char '\n') ("\nde", Position 3 10) @?=
        Right (("de", Position 4 0), '\n')

    , testCase "reject" $
      runLexer (char 'c') ("abc", Position 3 10) @?=
        Left (Unexpected 'a' (Position 3 10))
    ]
  , testGroup "string"
    [ testCase "accept" $
      runLexer (string "abc") ("abcde", Position 3 10) @?=
        Right (("de", Position 3 13), "abc")

    , testCase "reject start" $
      runLexer (string "abc") ("x", Position 3 10) @?=
        Left (Unexpected 'x' (Position 3 10))

    , testCase "reject EOF" $
      runLexer (string "abc") ("ab", Position 3 10) @?=
        Left (UnexpectedEOF (Position 3 12))

    , testCase "reject later" $
      runLexer (string "abc") ("abxc", Position 3 10) @?=
        Left (Unexpected 'x' (Position 3 12))

    , testCase "accept \\n" $
      runLexer (string "ab\nc") ("ab\ncd", Position 3 10) @?=
        Right (("d", Position 4 1), "ab\nc")

    , testCase "reject \\n" $
      runLexer (string "ab\nc") ("ab\nxx", Position 3 10) @?=
        Left (Unexpected 'x' (Position 4 0))
    ]
  , testGroup "oneOf"
    [ testCase "empty" $
      runLexer @Char (oneOf []) ("abc", Position 3 10) @?=
        Left (Unexpected 'a' (Position 3 10))

    , testCase "accept" $
      runLexer (oneOf [char 'a', char 'b']) ("abc", Position 3 10) @?=
        Right (("bc", Position 3 11), 'a')

    , testCase "accept multiple" $
      runLexer (oneOf [string "ab", string "abc"]) ("abcd", Position 3 10) @?=
        Right (("cd", Position 3 12), "ab")

    , testCase "reject" $
      runLexer (oneOf [char 'a', char 'b']) ("cab", Position 3 10) @?=
        Left (Unexpected 'c' (Position 3 10))
    ]
  , testGroup "notFollowedBy"
    [ testCase "accept" $
      runLexer (char 'a' <* notFollowedBy (char 'c'))
               ("abc", Position 3 10) @?=
        Right (("bc", Position 3 11), 'a')
    , testCase "accept, more after" $
      runLexer ((:) <$> (char 'a' <* notFollowedBy (char 'c')) <*> string "bc")
               ("abcd", Position 3 10) @?=
        Right (("d", Position 3 13), "abc")
    , testCase "reject" $
      runLexer (string "ab" <* notFollowedBy (char 'c'))
               ("abcd", Position 3 10) @?=
        Left (Unexpected 'c' (Position 3 12))
    , testCase "reject newline" $
      runLexer (string "ab" <* notFollowedBy (char '\n'))
               ("ab\nd", Position 3 10) @?=
        Left (Unexpected '\n' (Position 3 12))
    ]
  ]

literalTests :: TestTree
literalTests = testGroup "Literals"
  [ testGroup "DecLit"
    [ testCase "accept" $
      runLexer decLit ("123l", Position 2 3) @?=
        Right (("l", Position 2 6), DecLit "123")
    , testCase "accept then space" $
      runLexer decLit ("123 ", Position 2 3) @?=
        Right ((" ", Position 2 6), DecLit "123")
    , testCase "accept then symbol" $
      runLexer decLit ("123+", Position 2 3) @?=
        Right (("+", Position 2 6), DecLit "123")
    , testCase "no negative" $
      runLexer decLit ("-1", Position 2 3) @?=
        Left (Unexpected '-' (Position 2 3))
    , testCase "reject" $
      runLexer decLit ("x0l", Position 2 3) @?=
        Left (Unexpected 'x' (Position 2 3))
    ]
  , testGroup "HexLit"
    [ testCase "accept" $
      runLexer hexLit ("0x1b2ar", Position 2 3) @?=
        Right (("r", Position 2 9), HexLit "1b2a")
    , testCase "accept capital" $
      runLexer hexLit ("0x1B2A ", Position 2 3) @?=
        Right ((" ", Position 2 9), HexLit "1B2A")
    , testCase "accept then symbol" $
      runLexer hexLit ("0x1b2a+", Position 2 3) @?=
        Right (("+", Position 2 9), HexLit "1b2a")
    , testCase "reject empty" $
      runLexer hexLit ("0xl", Position 2 3) @?=
        Left (Unexpected 'l' (Position 2 5))
    , testCase "reject capital x" $
      runLexer hexLit ("0X1", Position 2 3) @?=
        Left (Unexpected 'X' (Position 2 4))
    ]
  , testGroup "BoolLit"
    [ testCase "accept true" $
      runLexer boolLit ("true", Position 2 3) @?=
        Right (("", Position 2 7), BoolLit True)
    , testCase "accept false" $
      runLexer boolLit ("false", Position 2 3) @?=
        Right (("", Position 2 8), BoolLit False)
    , testCase "accept then space" $
      runLexer boolLit ("true ", Position 2 3) @?=
        Right ((" ", Position 2 7), BoolLit True)
    , testCase "accept then symbol" $
      runLexer boolLit ("true+", Position 2 3) @?=
        Right (("+", Position 2 7), BoolLit True)
    , testCase "reject capital" $
      runLexer boolLit ("True", Position 2 3) @?=
        Left (Unexpected 'T' (Position 2 3))
    , testCase "reject prefix" $
      runLexer boolLit ("truey", Position 2 3) @?=
        Left (Unexpected 'y' (Position 2 7))
    ]
  , testGroup "CharLit"
    [ testCase "accept letter" $
      runLexer charLit ("'a'rest", Position 0 1) @?=
        Right (("rest", Position 0 4), CharLit 'a')
    , testCase "accept space" $
      runLexer charLit ("' 'rest", Position 0 1) @?=
        Right (("rest", Position 0 4), CharLit ' ')
    , testCase "accept symbol" $
      runLexer charLit ("'+'rest", Position 0 1) @?=
        Right (("rest", Position 0 4), CharLit '+')
    , testCase "reject empty" $
      runLexer charLit ("''rest", Position 0 1) @?=
        Left (Unexpected '\'' (Position 0 2))
    , testCase "reject nonprintable" $
      runLexer charLit ("'\SOH'", Position 0 1) @?=
        Left (Unexpected '\SOH' (Position 0 2))
    , testCase "accept escaped newline" $
      runLexer charLit ("'\\n'rest", Position 0 1) @?=
        Right (("rest", Position 0 5), CharLit '\n')
    , testCase "reject non-escaped newline" $
      runLexer charLit ("'\n'rest", Position 0 1) @?=
        Left (Unexpected '\n' (Position 0 2))
    , testCase "accept escaped tab" $
      runLexer charLit ("'\\t'rest", Position 0 1) @?=
        Right (("rest", Position 0 5), CharLit '\t')
    , testCase "reject non-escaped tab" $
      runLexer charLit ("'\t'rest", Position 0 1) @?=
        Left (Unexpected '\t' (Position 0 2))
    , testCase "accept escaped '" $
      runLexer charLit ("'\\''rest", Position 0 1) @?=
        Right (("rest", Position 0 5), CharLit '\'')
    , testCase "reject non-escaped '" $
      runLexer charLit ("'''rest", Position 0 1) @?=
        Left (Unexpected '\'' (Position 0 2))
    , testCase "accept escaped \"" $
      runLexer charLit ("'\\\"'rest", Position 0 1) @?=
        Right (("rest", Position 0 5), CharLit '\"')
    , testCase "reject non-escaped \"" $
      runLexer charLit ("'\"'rest", Position 0 1) @?=
        Left (Unexpected '\"' (Position 0 2))
    , testCase "reject non-escaped \"" $
      runLexer charLit ("'\"'rest", Position 0 1) @?=
        Left (Unexpected '\"' (Position 0 2))
    , testCase "illegal escaped p" $
      runLexer charLit ("'\\p'rest", Position 0 0) @?=
        Left (Fatal "illegal escaped character" (Position 0 2))
    ]
  , testGroup "StrLit"
    [ testCase "accept empty" $
      runLexer strLit ("\"\"rest", Position 0 1) @?=
        Right (("rest", Position 0 3), StrLit "")
    , testCase "accept single" $
      runLexer strLit ("\"a\"rest", Position 0 1) @?=
        Right (("rest", Position 0 4), StrLit "a")
    , testCase "accept words" $
      runLexer strLit ("\"hello world\"rest", Position 0 1) @?=
        Right (("rest", Position 0 14), StrLit "hello world")
    , testCase "accept symbols" $
      runLexer strLit ("\"!@#$%^&*()\"rest", Position 3 4) @?=
        Right (("rest", Position 3 16), StrLit "!@#$%^&*()")
    , testCase "reject nonprintable" $
      runLexer strLit ("\"a\SOH\"", Position 0 1) @?=
        Left (Unexpected '\SOH' (Position 0 3))
    , testCase "accept escaped '" $
      runLexer strLit ("\"\\'\"rest", Position 0 1) @?=
        Right (("rest", Position 0 5), StrLit "\'")
    , testCase "reject non-escaped '" $
      runLexer strLit ("\"'\"rest", Position 0 1) @?=
        Left (Unexpected '\'' (Position 0 2))
    , testCase "accept escaped \"" $
      runLexer strLit ("\"\\\"\"rest", Position 0 1) @?=
        Right (("rest", Position 0 5), StrLit "\"")
    ]
  ]

identifierKeywordsTests :: TestTree
identifierKeywordsTests = testGroup "Identifiers/Keywords"
  [ testGroup "Keywords"
    [ testCase "bool" $
      runLexer keyword ("bool#", Position 0 0) @?=
        Right (("#", Position 0 4), Bool)
    , testCase "int" $
      runLexer keyword ("int", Position 0 0) @?=
        Right (("", Position 0 3), Int)
    , testCase "continue" $
      runLexer keyword ("continue", Position 0 0) @?=
        Right (("", Position 0 8), Continue)
    , testCase "len" $
      runLexer keyword ("len", Position 0 0) @?=
        Right (("", Position 0 3), Len)
    , testCase "reject non-keyword" $
      runLexer keyword ("in", Position 0 0) @?=
        Left (Unexpected 'i' (Position 0 0))
    , testCase "reject bad case" $
      runLexer keyword ("Bool", Position 0 0) @?=
        Left (Unexpected 'B' (Position 0 0))
    , testCase "reject trailing chars" $
      runLexer keyword ("integer", Position 0 0) @?=
        Left (Unexpected 'e' (Position 0 3))
    ]
  , testGroup "Identifiers"
    [ testCase "alphabetical" $
      runLexer identifier ("abc#", Position 0 0) @?=
        Right (("#", Position 0 3), Identifier "abc")
    , testCase "capital" $
      runLexer identifier ("Abc#", Position 0 0) @?=
        Right (("#", Position 0 3), Identifier "Abc")
    , testCase "alphanumeric" $
      runLexer identifier ("ab0c1#", Position 0 0) @?=
        Right (("#", Position 0 5), Identifier "ab0c1")
    , testCase "has underscore" $
      runLexer identifier ("ab0_c1#", Position 0 0) @?=
        Right (("#", Position 0 6), Identifier "ab0_c1")
    , testCase "reject number" $
      runLexer identifier ("1ab0c", Position 0 0) @?=
        Left (Unexpected '1' (Position 0 0))
    , testCase "reject symbol" $
      runLexer identifier ("#a", Position 0 0) @?=
        Left (Unexpected '#' (Position 0 0))
    ]
  , testGroup "Both"
    [ testCase "keyword" $
      runLexer (keyword <|> identifier) ("for()", Position 1 2) @?=
        Right (("()", Position 1 5), For)
    , testCase "identifier" $
      runLexer (keyword <|> identifier) ("var1a ", Position 1 2) @?=
        Right ((" ", Position 1 7), Identifier "var1a")
    , testCase "keyword prefix" $
      runLexer (keyword <|> identifier) ("form$", Position 1 2) @?=
        Right (("$", Position 1 6), Identifier "form")
    , testCase "keyword with underscore" $
      runLexer (keyword <|> identifier) ("for_", Position 1 2) @?=
        Right (("", Position 1 6), Identifier "for_")
    , testCase "starts with digit" $
      runLexer (keyword <|> identifier) ("1ab'", Position 1 2) @?=
        Left (Unexpected '1' (Position 1 2))
    ]
  ]

symbolTests :: TestTree
symbolTests = testGroup "Symbols"
  [ testCase "left brace" $
    runLexer symbol ("{}", Position 0 0) @?=
      Right (("}", Position 0 1), LBrace)
  , testCase "and" $
    runLexer symbol ("&& ", Position 0 0) @?=
      Right ((" ", Position 0 2), And)
  , testCase "gets" $
    runLexer symbol ("= ", Position 0 0) @?=
      Right ((" ", Position 0 1), Gets)
  , testCase "not" $
    runLexer symbol ("!", Position 0 0) @?=
      Right (("", Position 0 1), Not)
  , testCase "equals" $
    runLexer symbol ("== ", Position 0 0) @?=
      Right ((" ", Position 0 2), Equals)
  , testCase "not equals" $
    runLexer symbol ("!= ", Position 0 0) @?=
      Right ((" ", Position 0 2), NEquals)
  , testCase "less" $
    runLexer symbol ("< ", Position 0 0) @?=
      Right ((" ", Position 0 1), Less)
  , testCase "less equals" $
    runLexer symbol ("<= ", Position 0 0) @?=
      Right ((" ", Position 0 2), LessEq)
  , testCase "plus" $
    runLexer symbol ("+", Position 0 0) @?=
      Right (("", Position 0 1), Plus)
  , testCase "increment" $
    runLexer symbol ("++", Position 0 0) @?=
      Right (("", Position 0 2), PlusPlus)
  , testCase "plus gets" $
    runLexer symbol ("+=", Position 0 0) @?=
      Right (("", Position 0 2), PlusGets)
  ]

commentTests :: TestTree
commentTests = testGroup "Comments"
  [ testCase "line comment" $
    runLexer comment ("// one 13 3 \nnext line", Position 0 0) @?=
      Right (("next line", Position 1 0), LineComment " one 13 3 ")
  , testCase "line comment EOF" $
    runLexer comment ("//cmnt", Position 0 0) @?=
      Right (("", Position 0 6), LineComment "cmnt")
  , testCase "multiline comment" $
    runLexer comment ("/* one 13 3 \n two 4 */after", Position 0 0) @?=
      Right (("after", Position 1 9), BlockComment " one 13 3 \n two 4 ")
  , testCase "multiline empty" $
    runLexer comment ("/**/after", Position 0 0) @?=
      Right (("after", Position 0 4), BlockComment "")
  , testCase "multiline with delimiter chars" $
    runLexer comment ("/* ** \n t//wo 4 */after", Position 0 0) @?=
      Right (("after", Position 1 11), BlockComment " ** \n t//wo 4 ")
  , testCase "nested multiline" $
    runLexer comment ("/* ** /*\n t/wo */4 */after", Position 0 0) @?=
      Right (("after", Position 1 12), BlockComment " ** /*\n t/wo */4 ")
  , testCase "multiple nested multiline" $
    runLexer comment ("/* /*a\n /*\n*/ /**/*/4 */after", Position 0 0) @?=
      Right (("after", Position 2 13), BlockComment " /*a\n /*\n*/ /**/*/4 ")
  , testCase "nongreedy multiline" $
    runLexer comment ("/*a\nb*/*/after", Position 0 0) @?=
      Right (("*/after", Position 1 3), BlockComment "a\nb")
  , testCase "almost nested multiline" $
    runLexer comment ("/* ** \n/* t/wo 4 */after", Position 0 0) @?=
      Left (Fatal "unclosed block comment" $ Position 0 2)
  ]

whitespaceTests :: TestTree
whitespaceTests = testGroup "Whitespace"
  [ testCase "spaces" $
    runLexer whitespace ("  ", Position 0 0) @?=
      Right (("", Position 0 2), ())
  , testCase "tabs" $
    runLexer whitespace ("\t\t", Position 0 0) @?=
      Right (("", Position 0 2), ())
  , testCase "newlines" $
    runLexer whitespace ("\n\n", Position 0 0) @?=
      Right (("", Position 2 0), ())
  , testCase "combination" $
    runLexer whitespace ("\t \n\ta", Position 0 0) @?=
      Right (("a", Position 1 1), ())
  ]
