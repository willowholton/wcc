module Tacky where
import Parser


data TUnOperator
    = TComplement
    | TNegate
    | TNot
    deriving (Show, Eq)

data TBinOperator
    = TAdd
    | TSubtract
    | TMultiply
    | TDivide
    | TModulo
    | TBitAnd
    | TBitOr
    | TBitXor
    | TBitLShift
    | TBitRShift
    | TAnd
    | TOr
    | TEq
    | TNEq
    | TLThan
    | TGThan
    | TLEq
    | TGEq
    deriving (Show, Eq)

data TVar = TVar String
    deriving (Show, Eq)

data TValue
    = TConstant Int
    | Var TVar
    deriving (Show, Eq)

data TLabel
    = TLabel String
    deriving (Show, Eq)

data TInstruction
    = TReturn TValue
    | TUnOp TUnOperator TValue TVar -- operator, source, dest - dest must be a var not a constant
    | TBinOp TBinOperator TValue TValue TVar -- operator, source1, source2, dest - dest must be a var not a constant
    | TCopy TValue TValue -- copy will copy the result of an && or || expression to a temporary value
    | TJump TLabel -- unconditional jump
    | TJumpIfZero TValue TLabel -- jump if first arg is zero, to second arg target
    | TJumpNotZero TValue TLabel -- jump if not zero
    | TLabelInst TLabel -- label to jump to
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

-- like new var name, take an name and an int counter n and return a unique label name created with that counter along
-- with the incremented counter:
newLabelName :: String -> Int -> (String, Int)
newLabelName name n = ((name ++ "_" ++ show n), n + 1)

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
            Not        -> TNot
        newInst = TUnOp newOp nestedVal dest
    in (instList ++ [newInst], Var dest, n2)

-- && short circuits, so needs to jump if the first expression evaluates to FALSE:
getTInstructions n (Binary And exp1 exp2) =
    let -- get first expression:
        (instList1, nestedVal1, n1) = getTInstructions n exp1 
         -- generate label for the case if the first exp is false: 
        (ifFalseLabel, n2) = newLabelName "and_false" n1
        -- generate label for the end of this group of instructions:
        (endLabel, n3) = newLabelName "and_end" n2
        -- get the unique variable name for the final result of the expression: 
        (result, n4) = newVarName n3
        -- then evaluate the second expression:
        (instList2, nestedVal2, n5) = getTInstructions n4 exp2
        -- combine the sequence of expressions into one ugly long list:
        instList = instList1 ++ [TJumpIfZero nestedVal1 (TLabel ifFalseLabel)] ++ 
                   instList2 ++ [TJumpIfZero nestedVal2 (TLabel ifFalseLabel)] ++
                   [TCopy (TConstant 1) (Var result)] ++ [TJump (TLabel endLabel)] ++
                   [TLabelInst (TLabel ifFalseLabel)] ++ [TCopy (TConstant 0) (Var result)] ++
                   [TLabelInst (TLabel endLabel)]
    in (instList, Var result, n5)

-- || also short circuits, so needs to jump if the first expression evaluates to TRUE:
-- or is basically the exact mirror image of the && case, it jumps to the true label if exp1 evaluates to non zero:
getTInstructions n (Binary Or exp1 exp2) =
    let -- get first expression:
        (instList1, nestedVal1, n1) = getTInstructions n exp1 
         -- generate label for the case if the first exp is false: 
        (ifTrueLabel, n2) = newLabelName "or_true" n1
        -- generate label for the end of this group of instructions:
        (endLabel, n3) = newLabelName "or_end" n2
        -- get the unique variable name for the final result of the expression: 
        (result, n4) = newVarName n3
        -- then evaluate the second expression:
        (instList2, nestedVal2, n5) = getTInstructions n4 exp2
        -- combine the sequence of expressions into one ugly long list:
        instList = instList1 ++ [TJumpNotZero nestedVal1 (TLabel ifTrueLabel)] ++ 
                   instList2 ++ [TJumpNotZero nestedVal2 (TLabel ifTrueLabel)] ++
                   [TCopy (TConstant 0) (Var result)] ++ [TJump (TLabel endLabel)] ++
                   [TLabelInst (TLabel ifTrueLabel)] ++ [TCopy (TConstant 1) (Var result)] ++
                   [TLabelInst (TLabel endLabel)]
    in (instList, Var result, n5)

-- all other binary operations get evaluated normally:
getTInstructions n (Binary op exp1 exp2) =
    let (instList1, nestedVal1, n1) = getTInstructions n exp1
        (instList2, nestedVal2, n2) = getTInstructions n1 exp2
        (dest, n3) = newVarName n2
        newOp = case op of
            Add       -> TAdd
            Subtract  -> TSubtract
            Multiply  -> TMultiply
            Divide    -> TDivide
            Modulo    -> TModulo
            BitAnd    -> TBitAnd
            BitOr     -> TBitOr
            BitXor    -> TBitXor
            BitLShift -> TBitLShift
            BitRShift -> TBitRShift
            Equal     -> TEq
            NEqual    -> TNEq
            LThan     -> TLThan
            GThan     -> TGThan
            LEQ       -> TLEq
            GEQ       -> TGEq
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

printTLabel :: TLabel -> String
printTLabel (TLabel name) = "Label(\"" ++ name ++ "\")"

printTValue :: TValue -> String
printTValue (TConstant n) = "Constant(" ++ show n ++ ")"
printTValue (Var var) = printTVar var

printTOperator :: TUnOperator -> String
printTOperator (TComplement) = "Complement"
printTOperator (TNegate)     = "Negate"
printTOperator (TNot)        = "Not"


printTBinOperator :: TBinOperator -> String
printTBinOperator (TAdd)         = "Add"
printTBinOperator (TSubtract)    = "Subtract"
printTBinOperator (TMultiply)    = "Multiply"
printTBinOperator (TDivide)      = "Divide"
printTBinOperator (TModulo)      = "Modulo"
printTBinOperator (TBitAnd)      = "BitAnd"
printTBinOperator (TBitOr)       = "BitOr"
printTBinOperator (TBitXor)      = "BitXor"
printTBinOperator (TBitLShift)   = "BitLShift"
printTBinOperator (TBitRShift)   = "BitRShift"
printTBinOperator (TAnd)         = "And"
printTBinOperator (TOr)          = "Or"
printTBinOperator (TEq)          = "Equal"
printTBinOperator (TNEq)         = "Not Equal"
printTBinOperator (TLThan)       = "Less Than"
printTBinOperator (TGThan)       = "Greater Than"
printTBinOperator (TLEq)         = "Less or Equal"
printTBinOperator (TGEq)         = "Greater or Equal"

printTInstruction :: TInstruction -> String
printTInstruction (TReturn val) = "Return(" ++ printTValue val ++")\n"
printTInstruction (TUnOp op val var) = "Unary(" ++ printTOperator op ++
                                     "," ++ printTValue val ++ "," ++ printTVar var ++ ")\n"
printTInstruction (TBinOp op val1 val2 var) = "Binary(" ++ printTBinOperator op ++
                                     "," ++ printTValue val1 ++ "," ++ printTValue val2 ++ "," ++ printTVar var ++ ")\n"
printTInstruction (TCopy val1 val2) = "Copy(" ++ printTValue val1 ++ ", " ++ printTValue val2 ++ ")\n"
printTInstruction (TJump val) = "Jump(" ++ printTLabel val ++ ")\n"
printTInstruction (TJumpIfZero val1 var) = "JumpIfZero(" ++ printTValue val1 ++ ", " ++ printTLabel var ++ ")\n"
printTInstruction (TJumpNotZero val1 var) = "JumpNotZero(" ++ printTValue val1 ++ ", " ++ printTLabel var ++ ")\n"
printTInstruction (TLabelInst var) = "Label(" ++ printTLabel var ++ ")\n"

printTFunction :: TFunction -> String
printTFunction (TFunction name instList) = "Function: " ++ name ++ "\n" ++ concatMap printTInstruction instList

printTProgram :: TProgram -> String
printTProgram (TProgram func) = printTFunction func