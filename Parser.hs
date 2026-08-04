module Parser where

import Lexer (Token(..))
import Distribution.Simple (KnownExtension(NegativeLiterals))


-- a program is only a single function for now:
data Program
  = Program Function
  deriving (Show, Eq)

-- a function has a name (the string) and a body (just a single statement for now),
-- Function function takes these as two separate arguments not a tuple:
data Function
  = Function String Statement
  deriving (Show, Eq)

-- there is only one kind of statement for now, a return:
data Statement
  = Return Exp
  deriving (Show, Eq)

-- there is only one kind of expression for now, a constant int:
data Exp
   = Constant Int
   | Unary UnOp Exp
  deriving (Show, Eq)

data UnOp
  = Negate
  | Complement
  | Decrement
  deriving (Show, Eq)

-- accept a list of tokens and return either an error message or a tuple containing the
-- parsed expression and the remaining list of tokens:
parseExp :: [Token] -> Either String (Exp, [Token])
-- split the constant num off and return the pair (num , remaining)
parseExp (ConstantToken num : rem) = Right (Constant num, rem)
parseExp (OpenParToken : rem) = do
  (exp, rem1) <- parseExp rem
  rem2 <- expect CloseParToken rem1 "Error - expected ')'"
  Right (exp, rem2)
parseExp (NegativeToken : rem) = do
  (exp, rem1) <- parseExp rem
  Right (Unary Negate exp, rem1)
parseExp (TildeToken : rem) = do
  (exp, rem1) <- parseExp rem
  Right (Unary Complement exp, rem1)
-- if called on anything that doesn't match the above pattern, return an error message:
parseExp _ = Left "Error - expected an expression"

-- print the parsed expression nicely using show:
printExp :: Exp -> (String)
printExp (Constant num)  = "Constant(" ++ show num ++ ")"
printExp (Unary op exp) = printOp op ++ "(" ++ printExp exp ++ ")"

printOp :: UnOp -> String
printOp Negate = "Negate"
printOp Complement = "Complement"

parseStatement :: [Token] -> Either String (Statement, [Token])
-- split the return keyword off and parse the first token of the remaining tokens: 
parseStatement (RetKeywordToken : rem) = case parseExp rem of
    -- if that token can't be parsed as an expression, return the error:
    Left err -> Left err
    -- if that token does match a valid expression:
    Right (exp, rem) -> case rem of
        -- Check for a semicolon after the expression:
        (SemicolonToken : rem) -> Right (Return exp, rem)
        -- if no semicolon, return error:
        _                      -> Left "Error - expected ;"
-- anything else that doesn't match the pattern of "return ___" is an error
parseStatement _ = Left "Error - expected 'return'"

-- print statement nicely using printExpression:
printStatement :: Statement -> Int -> (String)
printStatement (Return exp) depth =
  "Return(\n" ++ 
  indent (depth + 1) ++ printExp exp ++ "\n" ++
  indent depth ++ ")"

parseFunction :: [Token] -> Either String (Function, [Token])
-- take the given list of tokens and check for each of the following:
parseFunction tokens = do
    -- expect is a function that takes a token type, a list of tokens, and an error message to produce
    -- if it fails to find the specified token at the head of the given list of tokens:
    rem <- expect IntKeywordToken tokens "Error - expected 'int'"
    (name, rem1) <- expectIdentifier rem "Error - expected a function name"
    rem2 <- expect OpenParToken rem1 "Error - expected '('"
    rem3 <- expect VoidKeywordToken rem2 "Error - expected 'void'"
    rem4 <- expect CloseParToken rem3 "Error - expected ')'"
    rem5 <- expect OpenBraceToken rem4 "Error - expected '{'"
    (st, rem6) <- parseStatement rem5
    rem7 <- expect CloseBraceToken rem6 "Error - expected '}'"
    Right (Function name st, rem7)

printFunction :: Function -> Int -> String
printFunction (Function name st) depth = 
  indent depth ++ "Function(\n" ++
  indent (depth+1) ++ "name = " ++ name ++ "\n" ++
  indent (depth+1) ++ "body = " ++ printStatement st (depth + 1) ++ "\n" ++
  indent (depth) ++ ")"

-- expect is a function that takes a token type, a list of tokens, and an error message to produce
-- if it fails to find the specified token at the head of the given list of tokens. it returns either
-- an error message string or a list of remaining tokens
expect :: Token -> ([Token] -> (String -> (Either String [Token])))
-- if given expected, a token and a remainder, and anything else following, where token == expected, then return the remainder
expect expected (tok : rem) _ | (tok == expected) = Right rem
-- if not matching that pattern exactly, return the given error message:
expect expected toks        err                   = Left (err ++ " but found " ++ showTokens toks)

-- expectIdentifier is like expect except that it takes a list of tokens and an error message, returns a tuple containing a string
-- (the name of the identifier) and a list of remaining tokens
expectIdentifier :: [Token] -> (String -> (Either String (String, [Token]))) 
-- if given a list of tokens with the first token being name and a remainder, return the name and remainder
expectIdentifier (IdentifierToken name : rem) _      = Right (name, rem)
-- if not matching that pattern exactly, return the given error message
expectIdentifier _                            err    = Left err

parseProgram :: [Token] -> Either String Program
parseProgram tokens = do
  (func, rem) <- parseFunction tokens
  case rem of
    [] -> Right (Program func)
    _  -> Left "Error - unexpection tokens after function end"

printProgram :: Program  -> String
printProgram (Program func) =
  "Program(\n" ++
  printFunction func 1 ++ "\n" ++
  ")"

indent :: Int -> String
indent level = replicate (level * 2) ' '

-- helper to print the first token in a given list of tokens 
showTokens :: [Token] -> String
showTokens (tok : _) = printToken tok
showTokens []        = "no input"

-- helper to print a single token nicely:
printToken :: Token -> String
printToken (IdentifierToken name) = "'Identifier(" ++ name ++ ")'"
printToken (ConstantToken num)    = "'Constant(" ++ show num ++ ")'"
printToken IntKeywordToken        = "'Int'"
printToken VoidKeywordToken       = "'void'"
printToken RetKeywordToken        = "'return'"
printToken OpenParToken           = "'('"
printToken CloseParToken          = "')'"
printToken OpenBraceToken         = "'{'"
printToken CloseBraceToken        = "'}'"
printToken SemicolonToken         = "';'"
printToken NegativeToken          = "'-'"
printToken TildeToken             = "'~'"
printToken DecrementToken         = "'--'"
