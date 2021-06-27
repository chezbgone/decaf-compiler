module Lexer where

data ScannedToken = ScannedToken { line :: Int
                                 , column :: Int
                                 , token :: Token
                                 } deriving (Eq)

data Token = Keyword String
           | Identifier String
           | LCurly
           | RCurly
           deriving (Eq)

instance Show Token where
  show (Keyword k) = k
  show (Identifier s) = s
  show LCurly = "{"
  show RCurly = "}"

scan :: String -> Either String ScannedToken
scan = undefined
