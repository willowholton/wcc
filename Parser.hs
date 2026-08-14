module Parser where

import Lexer (Token(..))


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

data Exp
   = Constant Int
   | Unary UnOp Exp
   | Binary BinOp Exp Exp
  deriving (Show, Eq)

data UnOp
  = Negate
  | Complement
  | Not
  deriving (Show, Eq)

data BinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  -- bitwise operators:
  | BitAnd
  | BitOr
  | BitXor
  | BitLShift
  | BitRShift
  -- logical operators:
  | And
  | Or
  | Equal
  | NEqual
  | LThan
  | GThan
  | LEQ
  | GEQ
  deriving (Show, Eq)

-- accept a list of tokens and return either an error message or a tuple containing the
-- parsed expression and the remaining list of tokens. Factors are now differentiated from
-- expressions because they are discrete items: constants, parenthesized expressions, or unary ops.
parseFactor :: [Token] -> Either String (Exp, [Token])
-- split the constant num off and return the pair (num , remaining):
parseFactor (ConstantToken num : rem) = Right (Constant num, rem)
-- find a "(", parse the expression that follows, and look for it's matching ")":
parseFactor (OpenParToken : rem) = do
  -- parseExp needs to be given a minimum precedence to look at, so 0 here:
  (exp, rem1) <- parseExp rem 0
  rem2 <- expect CloseParToken rem1 "Error - expected ')'"
  Right (exp, rem2)
-- a negative sign means that anything following it must be parsed as a factor, i.e. a negative
-- must be followed by a constant, a parenthesized expression, or another unary op. -~5 is legal,
-- with or without parentheses:
parseFactor (NegativeToken : rem) = do
  (exp, rem1) <- parseFactor rem
  Right (Unary Negate exp, rem1)
-- same as negative, a ~ can be followed by any factor:
parseFactor (ComplementToken : rem) = do
  (exp, rem1) <- parseFactor rem
  Right (Unary Complement exp, rem1)
-- same as above, a ! can be followed by any factor:
parseFactor (NotToken : rem) = do
  (exp, rem1) <- parseFactor rem
  Right (Unary Not exp, rem1)
-- if called on anything that doesn't match the above pattern, return an error message:
parseFactor _ = Left "Error - expected an expression"

-- helper function to peek at the next token without removing it:
peek :: [Token] -> Maybe Token
peek (t: _) = Just t
peek []     = Nothing

-- parse expression takes a list of tokens and a precedence value, returns either error or the newly parsed expression
-- and a list of remaining tokens. 
parseExp :: [Token] -> (Int -> (Either String (Exp, [Token])))
parseExp tokens minPrec = do
  -- parse the first factor from the list of tokens:
  (fac, rem) <- parseFactor tokens
  -- use the given minimum precedence to continue parsing factors using the helper function:
  parseExpLoop minPrec fac rem

-- helper function that takes a precedence value, an expression, and a list of remaining tokens. loops over the remaining tokens
-- and builds onto the expression
parseExpLoop :: Int -> (Exp -> ([Token] -> (Either String (Exp, [Token]))))
parseExpLoop minPrec left rem =
  case peek rem of
    -- op is the next token found by peek, needs a guard to check that whatever peek found really is a binary op
    -- and that it really does have greater precedence.
    Just op | Just prec <- getPrecedence op, prec >= minPrec -> do
      -- only consume the token that was peeked if the above condition holds:
      (operator, rem1) <- expectBinOp rem "Error - expected a BinOp"
      -- each successive operator must have strictly stronger precedence, so the right had side gets parsed
      -- recursively with the new minimum precedence:
      (right, rem2) <- parseExp rem1 (prec + 1)
      -- The newly built expression is Binary operator left right, which gets passed back to continue parsing:
      parseExpLoop minPrec (Binary operator left right) rem2
    -- if there's nothing left to be parsed then return the left hand side:
    _  -> Right (left, rem)

expectBinOp :: [Token] -> (String -> (Either String (BinOp, [Token])))
expectBinOp (AddToken : rem) _      = Right (Add, rem)
expectBinOp (NegativeToken : rem) _ = Right (Subtract, rem)
expectBinOp (MulToken : rem) _      = Right (Multiply, rem)
expectBinOp (DivToken : rem) _      = Right (Divide, rem)
expectBinOp (ModToken : rem) _      = Right (Modulo, rem)
expectBinOp (AndToken : rem) _      = Right (BitAnd, rem)
expectBinOp (OrToken : rem) _       = Right (BitOr, rem)
expectBinOp (XorToken : rem) _      = Right (BitXor, rem)
expectBinOp (LShiftToken : rem) _   = Right (BitLShift, rem)
expectBinOp (RShiftToken : rem) _   = Right (BitRShift, rem)
expectBinOp (LAndToken : rem) _     = Right (And, rem)
expectBinOp (LOrToken : rem) _      = Right (Or, rem)
expectBinOp (EqToken : rem) _       = Right (Equal, rem)
expectBinOp (NEqToken : rem) _      = Right (NEqual, rem)
expectBinOp (LThanToken : rem) _    = Right (LThan, rem)
expectBinOp (GThanToken : rem) _    = Right (GThan, rem)
expectBinOp (LEqToken : rem) _      = Right (LEQ, rem)
expectBinOp (GEqToken : rem) _      = Right (GEQ, rem)
expectBinOp tokens err              = Left (err ++ " but found " ++ showTokens tokens)

-- print the parsed expression nicely using show:
printExp :: Exp -> (String)
printExp (Constant num)  = "Constant(" ++ show num ++ ")"
printExp (Unary op exp) = printUnOp op ++ "(" ++ printExp exp ++ ")"
printExp (Binary op exp1 exp2) = printBinOp op ++ "(" ++ printExp exp1 ++ ", " ++ printExp exp2 ++ ")"

-- print unary and binary operators:
printUnOp :: UnOp -> String
printUnOp Negate     = "Negate"
printUnOp Complement = "Complement"
printUnOp Not        = "Not"

printBinOp :: BinOp -> String
printBinOp Add      = "Add"
printBinOp Subtract = "Subtract"
printBinOp Multiply = "Multiply"
printBinOp Divide   = "Divide"
printBinOp Modulo   = "Modulo"
printBinOp BitAnd   = "BitAnd"
printBinOp BitOr    = "BitOr"
printBinOp BitXor   = "Xor"
printBinOp And      = "And"
printBinOp Or       = "Or"
printBinOp Equal    = "Equal"
printBinOp NEqual   = "Not Equal"
printBinOp LThan    = "Less Than"
printBinOp GThan    = "Greater Than"
printBinOp LEQ      = "Less or Equal"
printBinOp GEQ      = "Greater or Equal"

parseStatement :: [Token] -> Either String (Statement, [Token])
-- split the return keyword off and parse the first token of the remaining tokens. parseExp needs
-- a precedence value so it's given 0:
parseStatement (RetKeywordToken : rem) = case parseExp rem 0 of
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

-- print function using printStatement:
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

-- print an entire program (just one function for now)
printProgram :: Program  -> String
printProgram (Program func) =
  "Program(\n" ++
  printFunction func 1 ++ "\n" ++
  ")"

-- helper to create a nice 2 space indent for each level:
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
printToken DecrementToken         = "'--'"
printToken AddToken               = "'+'"
printToken MulToken               = "'*'"
printToken DivToken               = "'/'"
printToken ModToken               = "'%'"
printToken AndToken               = "'&'"
printToken ComplementToken        = "'~'"
printToken OrToken                = "'|'"
printToken XorToken               = "'^'"
printToken LShiftToken            = "'<<'"
printToken RShiftToken            = "'>>'"
printToken LAndToken              = "'&&'"
printToken LOrToken               = "'||'"
printToken EqToken                = "'=='"
printToken NEqToken               = "'!='"
printToken LThanToken             = "'<'"
printToken GThanToken             = "'>'"
printToken LEqToken               = "'<='"
printToken GEqToken               = "'>='"

-- get precedence of a token, numbers are arbitrary but do leave room for future lower precedence if needed:
getPrecedence :: Token -> Maybe Int
-- binary operators mul, div, mod, then add and sub:
getPrecedence MulToken      = Just 50
getPrecedence DivToken      = Just 50
getPrecedence ModToken      = Just 50
getPrecedence AddToken      = Just 45
getPrecedence NegativeToken = Just 45
-- bitwise shifts:
getPrecedence LShiftToken   = Just 40
getPrecedence RShiftToken   = Just 40
-- relational operators:
getPrecedence LThanToken    = Just 35
getPrecedence GThanToken    = Just 35
getPrecedence LEqToken      = Just 35
getPrecedence GEqToken      = Just 35
getPrecedence EqToken       = Just 30
getPrecedence NEqToken      = Just 30
-- bitwise and, xor, or:
getPrecedence AndToken      = Just 25
getPrecedence XorToken      = Just 20
getPrecedence OrToken       = Just 15
-- logical and, or
getPrecedence LAndToken     = Just 10
getPrecedence LOrToken      = Just 5
getPrecedence _             = Nothing