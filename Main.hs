module Main (main) where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Lexer
import Parser


main :: IO ()
main = do
  args <- getArgs
  run args


run :: [String] -> IO ()
run [flag, file] = do
  exists <- doesFileExist file
  if not exists
    then do
      putStrLn("Error: " ++ file ++ " does not exist")
      exitFailure
    else do
      contents <- readFile file
      case runCompiler flag contents of
        Left err -> do
          putStrLn err
          exitFailure
        Right () -> exitSuccess
run _ = exitFailure

runCompiler :: String -> (String -> (Either String ()))
runCompiler flag contents = do
  tokens <- lexer contents
  if flag == "--lex"
    then Right ()
    else do
      program <- parseProgram tokens
      if flag == "--parse"
        then Right ()
        else Left "no codegen implemented yet"

validFlag :: String -> Bool
validFlag flag = case flag of
  "--lex"     -> True
  "--parse"   -> True
  "--codegen" -> True
  "-S"        -> True
  _           -> False