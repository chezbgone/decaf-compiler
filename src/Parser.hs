{-# LANGUAGE LambdaCase #-}
module Parser where

import Lexer (Position, Token, RawToken)
import qualified Lexer
import Control.Applicative
import Control.Monad

---------------
-- AST types --
---------------

type Var = String

data AssignOp = Gets
data ArithOp = Plus | Minus | Times | Divide | Modulo
data RelOp = Less | Greater | LessEq | GreaterEq
data EqOp = Equals | NotEquals
data CondOp = And | Or

data BinOp = AOp ArithOp
           | ROp RelOp
           | EOp EqOp
           | COp CondOp

data Location = Var Var
              | At Var Expr -- id[expr]

data Expr = Location Location
          | Call -- TODO
          | Literal -- TODO
          | Len Var
          | BinOp BinOp Expr Expr
          | Negate Expr
          | Not Expr
          | Parens Expr


data ASTNodeLabel = Program
                  | Imports
                  | Fields
                  | Methods
                  | Import
                  | Field
  deriving (Eq, Show)

data AST = ASTToken RawToken
         | ASTFieldDecl Token -- type
                        [(String, Maybe Int)]
         | ASTNode ASTNodeLabel [AST]
         deriving (Eq, Show)

------------------------
-- Parser combinators --
------------------------

data ParserError = Unexpected RawToken
                 | UnexpectedEOF -- Position?
                 | Fatal String Position
                 deriving (Eq, Show)

newtype Parser a = Parser {
  runParser :: [RawToken] -> Either ParserError ([RawToken], a)
} deriving Functor

-- These are very similar to the Lexer instances
instance Applicative Parser where
  pure a = Parser (\s -> Right (s, a))
  liftA2 f (Parser la) (Parser lb) = Parser $
    \input -> do
      (rest, a) <- la input
      (rest', b) <- lb rest
      pure (rest', f a b)

instance Alternative Parser where
  empty = Parser (Left . unexpected)
  Parser la <|> Parser lb = Parser $
    \input -> case (la input, lb input) of
                (e@(Left (Fatal _ _)), _) -> e
                (_, e@(Left (Fatal _ _))) -> e
                (a, Left _)               -> a
                (Left _, b)               -> b
                (a, _)                    -> a

instance Monad Parser where
  Parser la >>= f = Parser $
    \input -> do
      (rest, a) <- la input
      runParser (f a) rest

instance MonadPlus Parser

-------------
-- Parsers --
-------------

unexpected :: [RawToken] -> ParserError
unexpected [] = UnexpectedEOF
unexpected (t:_) = Unexpected t


satisfies :: (RawToken -> Bool) -> Parser RawToken
satisfies p = Parser $
  \case
    (t:rest) | p t -> Right (rest, t)
    ts             -> Left $ unexpected ts

anyToken :: Parser RawToken
anyToken = satisfies (const True)

token :: Token -> Parser RawToken
token t = satisfies (\rt -> t == Lexer.token rt)


-----

identifier :: Parser RawToken
identifier = satisfies $ \case
    (Lexer.RawToken _ (Lexer.Identifier _)) -> True
    _                                       -> False

pType :: Parser RawToken
pType = satisfies $ \case
    (Lexer.RawToken _ Lexer.Bool) -> True
    (Lexer.RawToken _ Lexer.Int)  -> True
    _                             -> False

-- import is a Haskell keyword
pImport :: Parser AST
pImport = (\imp -> ASTNode Import [ASTToken imp]) <$>
  (token Lexer.Import *> identifier <* token Lexer.Semicolon)

imports :: Parser AST
imports = ASTNode Imports <$> many pImport

field :: Parser AST
field = do
  ty <- pType
  var <- identifier
  pure $ ASTFieldDecl (Lexer.token ty) []


fields :: Parser AST
fields = ASTNode Fields <$> many field

methods :: Parser AST
methods = ASTNode Methods <$> undefined

ast :: Parser AST
ast = ASTNode Program <$> sequence [imports, fields, methods]

parse :: [RawToken] -> Either ParserError AST
parse = undefined
