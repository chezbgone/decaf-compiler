{-# LANGUAGE LambdaCase #-}

module Lexer where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor
import Data.List

data ScannedToken = ScannedToken { line :: Int
                                 , column :: Int
                                 , token :: Token
                                 } deriving (Eq)

data Token = DecLit String
           | HexLit String
           | BoolLit Bool
           | CharLit Char
           | StrLit String
           | Identifier String
           | LineComment String
           | BlockComment String
           | Not
           | And
           | Or
           | Equals
           | NEquals
           | Plus
           | Minus
           | Times
           | Divide
           | Modulo
           | Less
           | Greater
           | LessEq
           | GreaterEq
           | Comma
           | LParens
           | RParens
           | LBrace
           | RBrace
           | LBracket
           | RBracket
           | Semicolon
           | Gets
           | PlusGets
           | MinusGets
           | PlusPlus
           | MinusMinus
           | Bool
           | Int
           | Void
           | Import
           | Break
           | Continue
           | If
           | Else
           | For
           | While
           | Return
           | Len
           deriving (Eq, Show)

-----------------
-- Lexer types --
-----------------

data LexerError = Unexpected Char
                | UnexpectedChar
                | UnexpectedEOF
                deriving (Show)

newtype Lexer a = Lexer {
  runLexer :: String -> Either LexerError (String, a)
} deriving Functor

-- instance Functor Lexer where
--   fmap f (Lexer l) = Lexer (fmap (fmap f) . l)

instance Applicative Lexer where
  pure a = Lexer (\s -> Right (s, a))
  Lexer lf <*> Lexer la = Lexer $
    \input -> do
      (rest, f) <- lf input
      (s, a) <- la rest
      pure (s, f a)

instance Alternative Lexer where
  empty = Lexer (Left . unexpected)
  Lexer la <|> Lexer lb = Lexer $
    \input -> case (la input, lb input) of
                (a, Left _) -> a
                (Left _, b) -> b
                (a@(Right (restA, _)), b@(Right (restB, _))) ->
                  if length restA <= length restB then a else b

instance Monad Lexer where
  Lexer la >>= f = Lexer $
    \input -> do
      (rest, a) <- la input
      runLexer (f a) rest

instance MonadPlus Lexer

---------------------------
-- Primitive combinators --
---------------------------

unexpected :: String -> LexerError
unexpected "" = UnexpectedEOF
unexpected (c:_) = Unexpected c

satisfies :: (Char -> Bool) -> Lexer Char
satisfies p = Lexer $ \case
    c : cs | p c -> Right (cs, c)
    rest         -> Left $ unexpected rest

char :: Char -> Lexer Char
char c = satisfies (c ==)

string :: String -> Lexer String
string = traverse char

oneOf :: Alternative f => forall a. [f a] -> f a
oneOf = foldl1' (<|>)

notFollowedBy :: Lexer a -> Lexer ()
notFollowedBy l =
  optional l >>= -- detect string
    maybe (pure ())       -- if not detected then success
          (const $ Lexer $ const $ Left UnexpectedChar) -- else fail

------------
-- Tokens --
------------

decLit :: Lexer Token
decLit = fmap DecLit $ some $ satisfies isDigit

hexLit :: Lexer Token
hexLit = fmap HexLit $ string "0x" *> some (satisfies isHexDigit)

boolLit :: Lexer Token
boolLit = string "true" $> BoolLit True <|>
            string "false" $> BoolLit False

charLit :: Lexer Token
charLit = fmap CharLit $ char '\'' *> satisfies (/= '\'') <* char '\''

strLit :: Lexer Token
strLit = fmap StrLit $ char '\"' *> many (satisfies (/= '\"')) <* char '\"'

literal :: Lexer Token
literal = decLit <|> hexLit <|> boolLit <|> charLit <|> strLit

identCharHead :: Char -> Bool
identCharHead c = c == '_' || isAlpha c

identCharRest :: Char -> Bool
identCharRest c = c == '_' || isAlphaNum c

identifier :: Lexer Token
identifier = fmap Identifier $ (:) <$> satisfies identCharHead
                                   <*> many (satisfies identCharRest)

keyword :: Lexer Token
keyword = oneOf [ string "bool" $> Bool
                , string "int" $> Int
                , string "void" $> Void
                , string "import" $> Import
                , string "break" $> Break
                , string "continue" $> Continue
                , string "if" $> If
                , string "else" $> Else
                , string "for" $> For
                , string "while" $> While
                , string "return" $> Return
                , string "len" $> Len
                ] <* notFollowedBy (satisfies identCharRest)

symb :: Lexer Token
symb = oneOf [ string "!"  $> Not
             , string "&&" $> And
             , string "||" $> Or
             , string "==" $> Equals
             , string "!=" $> NEquals
             , string "+"  $> Plus
             , string "-"  $> Minus
             , string "*"  $> Times
             , string "/"  $> Divide
             , string "%"  $> Modulo
             , string "<"  $> Less
             , string ">"  $> Greater
             , string "<=" $> LessEq
             , string ">=" $> GreaterEq
             , string ","  $> Comma
             , string "("  $> LParens
             , string ")"  $> RParens
             , string "{"  $> LBrace
             , string "}"  $> RBrace
             , string "["  $> LBracket
             , string "]"  $> RBracket
             , string ";"  $> Semicolon
             , string "="  $> Gets
             , string "+=" $> PlusGets
             , string "-=" $> MinusGets
             , string "++" $> PlusPlus
             , string "--" $> MinusMinus
             ]


lineComment :: Lexer Token
lineComment = fmap LineComment $
  string "//" *> many (satisfies (/='\n')) <* optional (char '\n')

-- content of a block comment
blockComment' :: Lexer String
blockComment' = string begin *> inComment
  where begin = "/*"
        end = "*/"
        inComment = string "*/" $> "" <|>
          ((++) <$> blockComment' <*> inComment) <|>
          ((++) <$> some (satisfies (`notElem` (begin ++ end))) <*> inComment) <|>
          ((:) <$> oneOf (char <$> (begin ++ end)) <*> inComment)

blockComment :: Lexer Token
blockComment = BlockComment <$> blockComment'


tokenL :: Lexer Token
tokenL = literal <|> keyword <|> identifier <|> symb
