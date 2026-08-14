module Main (main) where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import System.Directory (doesFileExist, removeFile)
import System.FilePath (dropExtension)
import System.Process (callProcess)
import System.IO (openTempFile, hClose)
import Lexer
import Parser
import Asm
import Tacky
import Fixup

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file]       -> run Nothing file
    [flag, file] -> run (Just flag) file
    _            -> exitFailure

-- helper that uses gccs preprocessor to deal # statements first. creates a temporary file, reads it into contents,
-- and then deletes the temp file:
preprocess:: FilePath -> IO String
preprocess file = do
  -- temp file will be where gcc puts our preprocessed file
  (path, name) <- openTempFile "." "preprocessed.c"
  -- -E indicates preprocessing only, -P removes line markers and other things wcc can't deal with:
  callProcess "gcc" ["-E", "-P", file, "-o", path] 
  contents <- readFile path
  hClose name
  removeFile path
  return contents

-- actually run compiler with maybe flag:
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
        contents <- preprocess file
        -- run compiler with preprocessed file up to indicated stage:
        case runCompiler stage contents of
          Left err -> do
            putStrLn err
            exitFailure
          -- no output stage, do nothing:
          Right NoOutput -> exitSuccess
          -- print AST only:
          Right (PrintAst program) -> do
            putStrLn (printProgram program)
            exitSuccess
          -- print intermediate representation:
          Right (PrintTacky program) -> do
            putStrLn (printTProgram program)
            exitSuccess
          -- write assembly to output file:
          Right (WriteAsm asmProgram) -> do
            let outFile = dropExtension file ++ ".s"
            writeFile outFile asmProgram
            -- if assembly only stage, do nothing:
            if stage == AsmStage
              then exitSuccess
              -- otherwise call gcc to compile from asm:
              else do
                callProcess "gcc" [outFile, "-o", dropExtension file]
                exitSuccess


runCompiler :: Stage -> (String -> (Either String Result))
runCompiler stage contents = do
  -- lex only:
  tokens  <- lexer contents
  if stage == LexStage
    then Right NoOutput
    -- then move on to parsing:
    else do
      program <- parseProgram tokens
      if stage == ParseStage
        then Right (PrintAst program)
        -- then to intermediate representation:
        else do
          let tackyProgram = getTProgram program
          if stage == CodegenStage
            then Right NoOutput
            else if stage == TackyStage
                then Right (PrintTacky tackyProgram)
                -- then to asm:
                else
                  let asmProgram = fixupProgram (getProgram tackyProgram)
                  in Right (WriteAsm (printAsmProgram asmProgram))


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