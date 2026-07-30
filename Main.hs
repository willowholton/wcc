module Main (main) where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Lexer (lexer)

main :: IO ()
main = do
  args <- getArgs
  run args


run :: [String] -> IO ()
run args = case args of
  [('-':_)] -> do
    putStrLn ("Error: missing file name")
    exitFailure

  [file] -> do
    exists <- doesFileExist file
    if exists
      then exitSuccess
      else do
        putStrLn ("Error: " ++ file ++ " does not exist")
        exitFailure

  [flag, file] -> do
    exists <- doesFileExist file
    if exists
      then
        if validFlag flag
          then do
            contents <- readFile file
            case lexer contents of
              Left err -> do
                putStrLn ("Lex error: " ++ err)
                exitFailure
              Right tokens -> do
                print tokens
                exitSuccess
          else do
            putStrLn ("Error: " ++ flag ++ " is invalid")
            exitFailure
      else do
        putStrLn ("Error: " ++ file ++ " does not exist")
        exitFailure
  _ -> do
    putStrLn ("Error: no file specified")
    exitFailure

validFlag :: String -> Bool
validFlag flag = case flag of
  "--lex"     -> True
  "--parse"   -> True
  "--codegen" -> True
  "-S"        -> True
  _           -> False