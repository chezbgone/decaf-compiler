{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Lexer where

import Control.Monad
import Control.Applicative

data ScannedToken = ScannedToken { line :: Int
                                 , column :: Int
                                 , token :: Token
                                 } deriving (Eq)

data Token = DecLiteral String
           | HexLiteral String
           | BoolLiteral Bool
           | CharLiteral Char
           | StringLiteral String
           | Keyword String
           | Identifier String
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
           | LBrack
           | RBrack
           | Import
           | Int
           | Bool
           | Void
           | Semicolon
           | Len
           | Gets
           | PlusGets
           | MinusGets
           | PlusPlus
           | MinusMinus
           | If
           | Else
           | For
           | While
           | Return
           | Break
           | Continue
           deriving (Eq)

instance Show Token where
  show (Keyword k) = k
  show (Identifier s) = s
  show LBrace = "{"
  show RBrace = "}"
  show _ = undefined


-- =========== --
-- Lexer types --
-- =========== --

data LexerError = Unexpected Char
                | UnexpectedEOF

newtype Lexer a = Lexer {
  runLexer :: String -> Either LexerError (String, a)
}

instance Functor Lexer where
  fmap f (Lexer l) = Lexer (fmap (fmap f) . l)

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

-- ===================== --
-- Primitive combinators --
-- ===================== --

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
