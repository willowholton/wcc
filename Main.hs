module Main (main) where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.FilePath (dropExtension)
import System.Process (callProcess)
import Lexer
import Parser
import Asm
import Tacky

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file]       -> run Nothing file
    [flag, file] -> run (Just flag) file
    _            -> exitFailure


run :: Maybe String -> FilePath -> IO ()
run flag file = do
  exists <- doesFileExist file
  if not exists
    then do
      putStrLn ("Error - file " ++ file ++ " does not exist")
      exitFailure
    else case (getStage flag) of
      Left err -> do
        putStrLn err
        exitFailure
      Right stage -> do
        contents <- readFile file
        case runCompiler stage contents of
          Left err -> do
            putStrLn err
            exitFailure
          Right NoOutput -> exitSuccess
          Right (PrintTacky program) -> do
            putStrLn (printTProgram program)
            exitSuccess
          Right (WriteAsm asmProgram) -> do
            let outFile = dropExtension file ++ ".s"
            writeFile outFile asmProgram
            if stage == AsmStage
              then exitSuccess
              else do
                callProcess "gcc" [outFile, "-o", dropExtension file]
                exitSuccess


runCompiler :: Stage -> (String -> (Either String Result))
runCompiler stage contents = do
  tokens  <- lexer contents
  if stage == LexStage
    then Right NoOutput
    else do
      program <- parseProgram tokens
      if stage == ParseStage
        then Right (PrintAst program)
        else do
          let tackyProgram = getTProgram program
          if stage == CodegenStage
            then Right NoOutput
            else if stage == TackyStage
                then Right (PrintTacky tackyProgram)
                else Left "not implemented"

data Result
  = NoOutput
  | PrintAst Program
  | PrintTacky TProgram
  | WriteAsm String
  deriving (Show, Eq)

-- stages used to mark where the process can end:
data Stage
  = FullStage
  | LexStage
  | ParseStage
  | CodegenStage
  | TackyStage
  | AsmStage
  deriving (Show, Eq)

-- take a flag (if given) and determine what stage of the process to stop at:
getStage :: Maybe String -> Either String Stage
getStage Nothing            = Right FullStage
getStage (Just "--lex")     = Right LexStage
getStage (Just "--parse")   = Right ParseStage
getStage (Just "--codegen") = Right CodegenStage
getStage (Just "--tacky")   = Right TackyStage
getStage (Just "-S")        = Right AsmStage
getStage (Just unknown)     = Left ("Error invalid flag: " ++ unknown)

-- determine whether the given flag is a valid one or not:
{-validFlag :: String -> Bool
validFlag flag = case flag of
  "--lex"     -> True
  "--parse"   -> True
  "--codegen" -> True
  "--tacky"   -> True
  "-S"        -> True
  _           -> False
  -}