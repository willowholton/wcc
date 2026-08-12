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
    | DX
    | R10
    | R11
    deriving (Show, Eq)

data AsmUnOp
    = Neg
    | Not
    deriving (Show, Eq)

data AsmBinOp
    = Add
    | Sub
    | Mul
    deriving (Show, Eq)

data AsmInstruction
    = Mov AsmOperand AsmOperand
    | AsmUnary AsmUnOp AsmOperand
    | AsmBinary AsmBinOp AsmOperand AsmOperand
    | Idiv AsmOperand
    | Cdq
    | AllocStack Int
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

getInstruction (TBinOp TAdd src1 src2 dest) = 
    [(Mov (getOperand src1) (getOperand (Var dest))), (AsmBinary Add (getOperand src2) (getOperand (Var dest)))]

getInstruction (TBinOp TSubtract src1 src2 dest) = 
    [(Mov (getOperand src1) (getOperand (Var dest))), (AsmBinary Sub (getOperand src2) (getOperand (Var dest)))]

getInstruction (TBinOp TMultiply src1 src2 dest) = 
    [(Mov (getOperand src1) (getOperand (Var dest))), (AsmBinary Mul (getOperand src2) (getOperand (Var dest)))]

getInstruction (TBinOp TDivide src1 src2 dest) =
    [Mov (getOperand src1) (Reg AX), Cdq, Idiv (getOperand src2), Mov (Reg AX) (getOperand (Var dest))]

getInstruction (TBinOp TModulo src1 src2 dest) =
    [Mov (getOperand src1) (Reg AX), Cdq, Idiv (getOperand src2), Mov (Reg DX) (getOperand (Var dest))]

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
printAsmReg AX = "%eax"
printAsmReg DX = "%edx"
printAsmReg R10 = "%r10d"
printAsmReg R11 = "%r11d"

printAsmUnOp :: AsmUnOp -> String
printAsmUnOp Neg = "negl"
printAsmUnOp Not = "notl" 

printAsmBinOp :: AsmBinOp -> String
printAsmBinOp Add = "addl"
printAsmBinOp Sub = "subl" 
printAsmBinOp Mul = "imull"

printAsmInstruction :: AsmInstruction -> String
printAsmInstruction (Mov source dest) = "  movl " ++ printAsmOperand source ++ ", " ++ printAsmOperand dest ++ " \n"
printAsmInstruction (AsmUnary op operand) = "  " ++ printAsmUnOp op ++ " " ++ printAsmOperand operand ++ "\n"
printAsmInstruction (AsmBinary op operand1 operand2) = "  " ++ printAsmBinOp op ++ " " ++ printAsmOperand operand1 ++ ", " ++ printAsmOperand operand2 ++ "\n"
printAsmInstruction (Cdq) = "  cdq\n"
printAsmInstruction (Idiv operand) = "  idivl " ++ printAsmOperand operand ++ "\n"
printAsmInstruction (AllocStack num) = "  subq " ++ "$" ++ show num ++ ", " ++ "%rsp\n"
printAsmInstruction (Ret) = 
    -- epilogue:
    "  movq %rbp, %rsp \n" ++
    "  popq %rbp\n" ++
    -- return:
    "  ret \n"

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
