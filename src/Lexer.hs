{-# LANGUAGE LambdaCase #-}

module Lexer where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor
import Data.Foldable (asum)
import Data.List

-----------------
-- Lexer types --
-----------------

data Position = Position { line :: Int
                         , column :: Int
                         } deriving (Eq, Show)

data RawToken = RawToken { position :: Position
                         , token :: Token
                         } deriving (Eq, Show)

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

type UnprocessedString = (String, Position)

data LexerError = Unexpected Char Position
                | UnexpectedEOF Position
                | Fatal String Position
                deriving (Eq, Show)

newtype Lexer a = Lexer {
  runLexer :: UnprocessedString -> Either LexerError (UnprocessedString, a)
} deriving Functor

instance Applicative Lexer where
  pure a = Lexer (\s -> Right (s, a))
  liftA2 f (Lexer la) (Lexer lb) = Lexer $
    \input -> do
      (rest, a) <- la input
      (rest', b) <- lb rest
      pure (rest', f a b)
  Lexer lf <*> Lexer la = Lexer $
    \input -> do
      (rest, f) <- lf input
      (rest', a) <- la rest
      pure (rest', f a)

instance Alternative Lexer where
  empty = Lexer (Left . unexpected)
  Lexer la <|> Lexer lb = Lexer $
    \input -> case (la input, lb input) of
                (e@(Left (Fatal _ _)), _) -> e
                (_, e@(Left (Fatal _ _))) -> e
                (a, Left _)               -> a
                (Left _, b)               -> b
                (a, _)                    -> a

instance Monad Lexer where
  Lexer la >>= f = Lexer $
    \input -> do
      (rest, a) <- la input
      runLexer (f a) rest

instance MonadPlus Lexer

---------------------------
-- Primitive combinators --
---------------------------

fatal :: forall a. String -> Lexer a -> Lexer a
fatal msg (Lexer l) =
  Lexer $ \s -> case l s of
    e@(Left (Fatal _ _))     -> e
    Left (Unexpected _ pos)  -> Left (Fatal msg pos)
    Left (UnexpectedEOF pos) -> Left (Fatal msg pos)
    res                      -> res

unexpected :: UnprocessedString -> LexerError
unexpected ("", p) = UnexpectedEOF p
unexpected (c:_, p) = Unexpected c p

skip :: Char -> Position -> Position
skip '\n' (Position ln _  ) = Position (ln+1) 0
skip _    (Position ln col) = Position ln (col+1)

satisfies :: (Char -> Bool) -> Lexer Char
satisfies p = Lexer $
  \case (c:rest, pos) | p c -> Right ((rest, skip c pos), c)
        s                   -> Left $ unexpected s

anyChar :: Lexer Char
anyChar = satisfies (const True)

char :: Char -> Lexer Char
char c = satisfies (c ==)

string :: String -> Lexer String
string = traverse char

oneOf :: Alternative f => forall a. [f a] -> f a
oneOf = asum

notFollowedBy :: forall a. Lexer a -> Lexer ()
notFollowedBy la = Lexer $ \s -> runLexer (newLexer s) s
  where newLexer str = optional la >>= -- detect a
          maybe (pure ())       -- if not detected then success
            (const $ Lexer $ const $ Left $ unexpected str)

------------
-- Tokens --
------------

decLit :: Lexer Token
decLit = fmap DecLit $ some $ satisfies isDigit

hexLit :: Lexer Token
hexLit = fmap HexLit $ string "0x" *> some (satisfies isHexDigit)

boolLit :: Lexer Token
boolLit = (string "true" $> BoolLit True <|>
           string "false" $> BoolLit False) <*
             notFollowedBy (satisfies identCharRest)

inChar :: Lexer Char
inChar = oneOf [char (chr i) | i <- [32..126] \\ fmap (ord . fst) escapedChars]
  <|> char '\\' *> fatal errMsg (oneOf [string e $> c | (c, e) <- escapedChars])
    where escapedChars = [ ('\n', "n")
                         , ('\t', "t")
                         , ('\\', "\\")
                         , ('"', "\"")
                         , ('\'', "\'")
                         ]
          errMsg = "illegal escaped character"

charLit :: Lexer Token
charLit = fmap CharLit $ char '\'' *> inChar <* fatal errMsg (char '\'')
  where errMsg = "unclosed character literal"

strLit :: Lexer Token
strLit = fmap StrLit $ char '\"' *> many inChar <* char '\"'

literal :: Lexer Token
literal = hexLit <|> decLit <|> boolLit <|> charLit <|> strLit

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

symbol :: Lexer Token
symbol = oneOf [ string "&&" $> And
               , string "||" $> Or
               , string "==" $> Equals
               , string "!=" $> NEquals
               , string "<=" $> LessEq
               , string ">=" $> GreaterEq
               , string "+=" $> PlusGets
               , string "-=" $> MinusGets
               , string "++" $> PlusPlus
               , string "--" $> MinusMinus
               , string "!"  $> Not
               , string "%"  $> Modulo
               , string "<"  $> Less
               , string ">"  $> Greater
               , string ","  $> Comma
               , string "("  $> LParens
               , string ")"  $> RParens
               , string "{"  $> LBrace
               , string "}"  $> RBrace
               , string "["  $> LBracket
               , string "]"  $> RBracket
               , string ";"  $> Semicolon
               , string "+"  $> Plus
               , string "-"  $> Minus
               , string "*"  $> Times
               , string "/"  $> Divide
               , string "="  $> Gets
               ]

lineComment :: Lexer Token
lineComment = fmap LineComment $
  string "//" *> many (satisfies (/='\n')) <* optional (char '\n')

blockComment :: Lexer Token
blockComment = BlockComment <$> (string begin *> fatal errMsg inComment)
  where
    (begin, end) = ("/*", "*/")
    delimChars = nub (begin <> end)
    inBlockComment = (\c -> begin ++ c ++ end) <$> (string begin *> inComment)
    inComment = string end $> "" <|>
      (++) <$> inBlockComment <*> inComment <|>
      (++) <$> some (satisfies (`notElem` delimChars)) <*> inComment <|>
      (:) <$ notFollowedBy (string begin) <*> oneOf (char <$> delimChars) <*> inComment
    errMsg = "unclosed block comment"

comment :: Lexer Token
comment = lineComment <|> blockComment

whitespace :: Lexer ()
whitespace = void $ some whitespaceChar
  where whitespaceChar = char ' ' <|> char '\n' <|> char '\t'

oneToken :: Lexer Token
oneToken = comment <|> literal <|> keyword <|> identifier <|> symbol

rawTokens :: UnprocessedString -> [Either LexerError RawToken]
rawTokens ("", _) = []
rawTokens u@(x:xs, pos) =
  let trimStart str = either (const str) fst $ runLexer whitespace str
      trimmed = trimStart u
      (_, startPos) = trimmed
   in if fst trimmed == ""
         then []
         else case runLexer oneToken trimmed of
           Left err          -> Left err : rawTokens (xs, skip x pos)
           Right (rest, tok) -> Right (RawToken startPos tok) : rawTokens rest
