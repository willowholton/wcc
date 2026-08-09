module Asm where
import Tacky

-- operands are only immediate values and one register for now:
data AsmOperand
    = Imm Int
    | Reg AsmReg
    | PseudoReg AsmPseudoReg
    | Stack Int
    deriving (Show, Eq)

data AsmPseudoReg
    = Pseudo String
    deriving (Show, Eq)

data AsmReg
    = AX
    | R10
    deriving (Show, Eq)

data AsmUnOp
    = Neg
    | Not
    deriving (Show, Eq)

-- instructions are only move and return for now:
data AsmInstruction
    = Mov AsmOperand AsmOperand -- move source -> destination
    | Ret
    | AsmUnary AsmUnOp AsmOperand
    | AllocStack Int
    deriving (Show, Eq)

-- a function is a name and list of instructions
data AsmFunction  
    = AsmFunction String [AsmInstruction]
    deriving (Show, Eq)

-- a program can be a list of multiple functions (eventually):
data AsmProgram
    = AsmProgram [AsmFunction]
    deriving (Show, Eq)

-- read an expression, return the corresponding operand:
getOperand :: TValue -> AsmOperand
getOperand (TConstant num) = Imm num
-- temporary variables live in pseudoregisters:
getOperand (Var (TVar name)) = PseudoReg (Pseudo name)

-- read a statement, return list containing asm instruction(s):
getInstruction :: TInstruction -> [AsmInstruction]
getInstruction (TReturn val) = [Mov (getOperand val) (Reg AX), Ret]
getInstruction (TUnOp op src dest) = 
    -- get the destination, which must be a var (can't move TO an immediate value)
    let destOperand = getOperand (Var dest)
        -- get corresponding asm operator:
        asmOp = case op of
            TNegate     -> Neg
            TComplement -> Not
    -- use that asm operator in the instructions:
    in [Mov (getOperand src) destOperand, AsmUnary asmOp destOperand]

-- read a function with a name and a statement, generate the corresponding asm function
getFunction :: TFunction -> AsmFunction
getFunction (TFunction name instList) = AsmFunction name (concatMap getInstruction instList)

getProgram :: TProgram -> AsmProgram
getProgram (TProgram func) = AsmProgram [getFunction func]

printAsmOperand :: AsmOperand -> String
printAsmOperand (Imm num)  = "$" ++ show num
printAsmOperand (Reg reg)  = printAsmReg reg
-- stack index e.g. 8(%rbp)
printAsmOperand (Stack num) = show num ++ "(%rbp)"
-- pseudo register shouldn't ever actually get printed
printAsmOperand (PseudoReg (Pseudo name))  = name

printAsmReg :: AsmReg -> String
printAsmReg (AX) = "%eax"
printAsmReg R10 = "%r10d"

printAsmUnOp :: AsmUnOp -> String
printAsmUnOp Neg = "negl"
printAsmUnOp Not = "notl" 

printAsmInstruction :: AsmInstruction -> String
printAsmInstruction (Mov source dest) = "  movl " ++ printAsmOperand source ++ "," ++ printAsmOperand dest ++ " \n"
printAsmInstruction (Ret) = 
    -- epilogue:
    "  movq %rbp, %rsp \n" ++
    "  popq %rbp\n" ++
    -- return:
    "  ret \n"
printAsmInstruction (AsmUnary op operand) = "  " ++ printAsmUnOp op ++ " " ++ printAsmOperand operand ++ "\n"
printAsmInstruction (AllocStack num) = "  subq " ++ "$" ++ show num ++ "," ++ "%rsp\n"

printAsmFunction :: AsmFunction -> String
printAsmFunction (AsmFunction name instructions) = 
    "  .globl _" ++ name ++ "\n" ++
    "_"++name ++ ": \n" ++
    -- prologue:
    "  pushq %rbp\n" ++
    "  movq %rsp, %rbp\n" ++
    -- instructions:
    concatMap printAsmInstruction instructions ++ "\n"

printAsmProgram :: AsmProgram  -> String
printAsmProgram (AsmProgram funcs) = concatMap printAsmFunction funcs
