module Main (main) where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.FilePath (dropExtension)
import Lexer
import Parser
import Asm

main :: IO ()
main = do
  args <- getArgs
  run args


run :: [String] -> IO ()
run [flag, file] = do
  let outFile = dropExtension file ++ ".s"
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
        Right NoOutput       -> exitSuccess
        Right (PrintAst program) -> do
          putStrLn (printProgram program)
          exitSuccess
        Right (WriteAsm asmString) -> do
          writeFile outFile asmString
          exitSuccess
run _ = exitFailure

runCompiler :: String -> (String -> (Either String Result))
runCompiler flag contents = do
  tokens  <- lexer contents
  program <- parseProgram tokens
  let asmProgram = getProgram program
  case flag of
    "--lex"     -> Right NoOutput
    "--parse"   -> Right (PrintAst program)
    "--codegen" -> Right NoOutput
    "-S"        -> Right (WriteAsm (printAsmProgram asmProgram))
    _           -> Left "Not implemented yet"

data Result
  = NoOutput
  | PrintAst Program
  | WriteAsm String
  deriving (Show, Eq)

validFlag :: String -> Bool
validFlag flag = case flag of
  "--lex"     -> True
  "--parse"   -> True
  "--codegen" -> True
  "-S"        -> True
  _           -> False