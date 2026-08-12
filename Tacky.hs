module Tacky where
import Parser


data TUnOperator
    = TComplement
    | TNegate
    deriving (Show, Eq)

data TBinOperator
    = TAdd
    | TSubtract
    | TMultiply
    | TDivide
    | TModulo
    deriving (Show, Eq)

data TVar = TVar String
    deriving (Show, Eq)

data TValue
    = TConstant Int
    | Var TVar
    deriving (Show, Eq)

data TInstruction
    = TReturn TValue
    | TUnOp TUnOperator TValue TVar -- operator, source, dest - dest must be a var not a constant
    | TBinOp TBinOperator TValue TValue TVar -- operator, source1, source2, dest - dest must be a var not a constant
    deriving (Show, Eq)

data TFunction
    = TFunction String [TInstruction]
    deriving (Show, Eq)

data TProgram
    = TProgram TFunction
   deriving (Show, Eq)

-- take an int counter n and return a variable name "tmpn" along with the incremented counter:
-- used to create unique var names e.g. var0, var1, var2, etc.
newVarName :: Int -> (TVar, Int)
newVarName n = (TVar ("tmp" ++ show n), n + 1)

-- take an int counter and a parsed expression, return a list of tacky instructions, a value where the final answer
-- will be held, and an new int counter:
getTInstructions :: Int -> Exp -> ([TInstruction], TValue, Int)
-- Constant c needs no instruction, it's value is whatever it is, and the counter remains the same:
getTInstructions n (Constant c) = ([], TConstant c, n)
-- a unary op applied to an expression, the expression is evaluated recursively using unique variable names:
getTInstructions n (Unary op exp) =
    let (instList, nestedVal, n1) = getTInstructions n exp
        (dest, n2) = newVarName n1
        newOp = case op of
            Negate     -> TNegate
            Complement -> TComplement
        newInst = TUnOp newOp nestedVal dest
    in (instList ++ [newInst], Var dest, n2)
getTInstructions n (Binary op exp1 exp2) =
    let (instList1, nestedVal1, n1) = getTInstructions n exp1
        (instList2, nestedVal2, n2) = getTInstructions n1 exp2
        (dest, n3) = newVarName n2
        newOp = case op of
            Add      -> TAdd
            Subtract -> TSubtract
            Multiply -> TMultiply
            Divide   -> TDivide
            Modulo   -> TModulo
        newInst = TBinOp newOp nestedVal1 nestedVal2 dest
    in (instList1 ++ instList2 ++ [newInst], Var dest, n3)

-- take an int counter and a parsed statement, turn it into a flat list of instructions and an updated counter:
getTStatement :: Int -> Statement -> ([TInstruction], Int)
-- the only kinds of statements we deal with right now are returns:
getTStatement n (Return exp) =
    let (instList, val, n1) = getTInstructions n exp
    in (instList ++ [TReturn val], n1)

-- take a parsed function and return a tacky function item with the correct name and the list of statements it corresponds to:
-- getFunction doesn't need to know about what temporary variables were used in other funcitons.
getTFunction :: Function -> TFunction
getTFunction (Function name st) =
    let (instList, _) = getTStatement 0 st
    in TFunction name instList

-- take a parsed program and return a tacky function.
-- programs are only a single function right now, but later they may be multiple functions:
getTProgram :: Program -> TProgram
getTProgram (Program func) = TProgram (getTFunction func)

printTVar :: TVar -> String
printTVar (TVar name) = "Var(\"" ++ name ++ "\")"

printTValue :: TValue -> String
printTValue (TConstant n) = "Constant(" ++ show n ++ ")"
printTValue (Var var) = printTVar var

printTOperator :: TUnOperator -> String
printTOperator (TComplement) = "Complement"
printTOperator (TNegate)     = "Negate"

printTBinOperator :: TBinOperator -> String
printTBinOperator (TAdd)      = "Add"
printTBinOperator (TSubtract) = "Subtract"
printTBinOperator (TMultiply) = "Multiply"
printTBinOperator (TDivide)   = "Divide"
printTBinOperator (TModulo)   = "Modulo"


printTInstruction :: TInstruction -> String
printTInstruction (TReturn val) = "Return(" ++ printTValue val ++")"
printTInstruction (TUnOp op val var) = "Unary(" ++ printTOperator op ++
                                     "," ++ printTValue val ++ "," ++ printTVar var ++ ")"
printTInstruction (TBinOp op val1 val2 var) = "Binary(" ++ printTBinOperator op ++
                                     "," ++ printTValue val1 ++ "," ++ printTValue val2 ++ "," ++ printTVar var ++ ")"

printTFunction :: TFunction -> String
printTFunction (TFunction name instList) = "Function: " ++ name ++ "\n" ++ concatMap printTInstruction instList

printTProgram :: TProgram -> String
printTProgram (TProgram func) = printTFunction func