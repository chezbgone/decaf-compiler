import Lexer

import System.Directory

main :: IO ()
main = do
  dir <- getCurrentDirectory
  putStrLn "=~=~=~="
  putStrLn dir
  putStrLn "=~=~=~="
