module Asm where
import Parser

-- operands are only immediate values and one register for now:
data AsmOperand
    = Imm Int
    | Reg
    deriving (Show, Eq)

-- instructions are only move and return for now:
data AsmInstruction
    = Mov AsmOperand AsmOperand -- move source -> destination
    | Ret
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
getOperand :: Exp -> AsmOperand
getOperand (Constant num) = Imm num

-- read a statement, return list containing asm instruction(s):
getInstruction :: Statement -> [AsmInstruction]
getInstruction (Return exp) = [Mov (getOperand exp) Reg, Ret]

-- read a function with a name and a statement, generate the corresponding asm function
getFunction :: Function -> AsmFunction
getFunction (Function name st) = AsmFunction name (getInstruction st)

getProgram :: Program -> AsmProgram
getProgram (Program func) = AsmProgram [getFunction func]

printOperand :: AsmOperand -> String
printOperand (Imm num)  = "#" ++ show num
printOperand (Reg)  = "x0"

printInstruction :: AsmInstruction -> String
printInstruction (Mov source dest) = "  mov " ++ printOperand dest ++ "," ++ printOperand source ++ " \n"
printInstruction (Ret) = "  ret \n"

printAsmFunction :: AsmFunction -> String
printAsmFunction (AsmFunction name instructions) = 
    "  .globl _" ++ name ++ "\n" ++
    "_"++name ++ ": \n" ++
    concatMap printInstruction instructions ++ "\n"

printAsmProgram :: AsmProgram  -> String
printAsmProgram (AsmProgram funcs) = concatMap printAsmFunction funcs
