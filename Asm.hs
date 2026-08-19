module Asm where
import Tacky
import Parser (BinOp(GEQ, LEQ))
import Lexer (Token(LAndToken))

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
    | CX
    deriving (Show, Eq)

data AsmUnOp
    = Neg
    | Not
    deriving (Show, Eq)

data AsmBinOp
    = Add
    | Sub
    | Mul
    | BitAnd
    | BitOr
    | BitXor
    | BitLShift
    | BitRShift
    deriving (Show, Eq)

data AsmInstruction
    = Mov AsmOperand AsmOperand  -- regular 32 bit mov
    | MovB AsmOperand AsmOperand -- 8 bit mov
    | AsmUnary AsmUnOp AsmOperand
    | AsmBinary AsmBinOp AsmOperand AsmOperand
    | Idiv AsmOperand
    | Cdq
    | AllocStack Int
    | Cmp AsmOperand AsmOperand
    | Jmp AsmLabel
    | JmpCC AsmCondCode AsmLabel
    | SetCC AsmCondCode AsmOperand
    | Label AsmLabel
    | Ret
    deriving (Show, Eq)

data AsmCondCode
    = E
    | NE
    | G
    | GE
    | L
    | LE
    deriving (Show, Eq)

data AsmLabel
    = AsmLabel String
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

getLabel :: TLabel -> AsmLabel
getLabel (TLabel name) = AsmLabel name

-- read a statement, return list containing asm instruction(s):
getInstruction :: TInstruction -> [AsmInstruction]
getInstruction (TReturn val) = [Mov (getOperand val) (Reg AX), Ret]
getInstruction (TUnOp TNot src dest) = [(Cmp (Imm 0) (getOperand src)),
                                        (Mov (Imm 0) (getOperand (Var dest))), 
                                        (SetCC E (getOperand (Var dest)))]

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

getInstruction (TBinOp TBitAnd src1 src2 dest) = 
    [(Mov (getOperand src1) (getOperand (Var dest))), (AsmBinary BitAnd (getOperand src2) (getOperand (Var dest)))]

getInstruction (TBinOp TBitOr src1 src2 dest) = 
    [(Mov (getOperand src1) (getOperand (Var dest))), (AsmBinary BitOr (getOperand src2) (getOperand (Var dest)))]
    
getInstruction (TBinOp TBitXor src1 src2 dest) = 
    [(Mov (getOperand src1) (getOperand (Var dest))), (AsmBinary BitXor (getOperand src2) (getOperand (Var dest)))]

-- shift by an immediate value just shifts:
getInstruction (TBinOp TBitLShift src (TConstant amt) dest) = [(Mov (getOperand src) (getOperand (Var dest))), 
                                                                 (AsmBinary BitLShift (Imm amt) (getOperand (Var dest)))]
-- shift by anything else needs to use cl register:
getInstruction (TBinOp TBitLShift src1 src2 dest) = [(Mov (getOperand src1) (getOperand (Var dest))), 
                                                     (MovB (getOperand src2) (Reg CX)), 
                                                     (AsmBinary BitLShift (Reg CX) (getOperand (Var dest)))]

-- shift by an immediate value just shifts:
getInstruction (TBinOp TBitRShift src (TConstant amt) dest) = [(Mov (getOperand src) (getOperand (Var dest))), 
                                                                 (AsmBinary BitRShift (Imm amt) (getOperand (Var dest)))]
-- shift by anything else needs to use cl register:
getInstruction (TBinOp TBitRShift src1 src2 dest) = [(Mov (getOperand src1) (getOperand (Var dest))), 
                                                     (MovB (getOperand src2) (Reg CX)), 
                                                     (AsmBinary BitRShift (Reg CX) (getOperand (Var dest)))]

-- jump if zero/not zero compare val to 0 and then use jmpcc with the correct condition code, equal or not equal:
getInstruction(TJumpIfZero val dest) = [Cmp (Imm 0) (getOperand val), JmpCC E (getLabel dest)]
getInstruction(TJumpNotZero val dest) = [Cmp (Imm 0) (getOperand val), JmpCC NE (getLabel dest)]

getInstruction (TBinOp TEq src1 src2 dest) = [(Cmp (getOperand src2) (getOperand src1)),
                                              (Mov (Imm 0) (getOperand (Var dest))), 
                                              (SetCC E (getOperand (Var dest)))]

getInstruction (TBinOp TNEq src1 src2 dest) = [(Cmp (getOperand src2) (getOperand src1)),
                                               (Mov (Imm 0) (getOperand (Var dest))), 
                                               (SetCC NE (getOperand (Var dest)))]

getInstruction (TBinOp TLThan src1 src2 dest) = [(Cmp (getOperand src2) (getOperand src1)),
                                                 (Mov (Imm 0) (getOperand (Var dest))), 
                                                 (SetCC L (getOperand (Var dest)))]

getInstruction (TBinOp TGThan src1 src2 dest) = [(Cmp (getOperand src2) (getOperand src1)),
                                                 (Mov (Imm 0) (getOperand (Var dest))), 
                                                 (SetCC G (getOperand (Var dest)))]

getInstruction (TBinOp TLEq src1 src2 dest) = [(Cmp (getOperand src2) (getOperand src1)),
                                               (Mov (Imm 0) (getOperand (Var dest))), 
                                               (SetCC LE (getOperand (Var dest)))]

getInstruction (TBinOp TGEq src1 src2 dest) = [(Cmp (getOperand src2) (getOperand src1)),
                                               (Mov (Imm 0) (getOperand (Var dest))), 
                                               (SetCC GE (getOperand (Var dest)))]  

getInstruction (TCopy src dest) = [Mov (getOperand src) (getOperand dest)]
getInstruction (TJump label) = [Jmp (getLabel label)]
getInstruction (TLabelInst label) = [Label (getLabel label)]


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

-- print 1 byte register:
printAsmOperandByte :: AsmOperand -> String
printAsmOperandByte (Reg reg) = printAsmRegByte reg
printAsmOperandByte other     = printAsmOperand other

printAsmLabel :: AsmLabel -> String
printAsmLabel (AsmLabel name) = ".L" ++ name

printAsmCondCode :: AsmCondCode -> String
printAsmCondCode (E) = "e"
printAsmCondCode (NE) = "ne"
printAsmCondCode (G) = "g"
printAsmCondCode (GE) = "ge"
printAsmCondCode (L) = "l"
printAsmCondCode (LE) = "le"

-- 4 byte versions:
printAsmReg :: AsmReg -> String
printAsmReg AX   = "%eax"
printAsmReg DX   = "%edx"
printAsmReg CX   = "%ecx"
printAsmReg R10  = "%r10d"
printAsmReg R11  = "%r11d"
 
-- 1 byte versions:
printAsmRegByte :: AsmReg -> String
printAsmRegByte AX = "%al"
printAsmRegByte DX = "%dl"
printAsmRegByte CX = "%cl"

printAsmUnOp :: AsmUnOp -> String
printAsmUnOp Neg = "negl"
printAsmUnOp Not = "notl"

printAsmBinOp :: AsmBinOp -> String
printAsmBinOp Add        = "addl"
printAsmBinOp Sub        = "subl" 
printAsmBinOp Mul        = "imull"
printAsmBinOp BitAnd     = "andl"
printAsmBinOp BitOr      = "orl"
printAsmBinOp BitXor     = "xorl"
printAsmBinOp BitLShift  = "sall" -- arithmetic shifts not shl and shr
printAsmBinOp BitRShift  = "sarl"

printAsmInstruction :: AsmInstruction -> String
printAsmInstruction (MovB src dest)            = "  movb " ++ printAsmOperandByte src ++ ", " ++ printAsmOperandByte dest ++ " \n"
printAsmInstruction (Mov src dest)             = "  movl " ++ printAsmOperand src ++ ", " ++ printAsmOperand dest ++ " \n"
printAsmInstruction (AsmUnary op oper)         = "  " ++ printAsmUnOp op ++ " " ++ printAsmOperand oper ++ "\n"
printAsmInstruction (AsmBinary op oper1 oper2) = "  " ++ printAsmBinOp op ++ " " ++ printAsmOperand oper1 ++ ", " ++ printAsmOperand oper2 ++ "\n"
printAsmInstruction (Cdq)                      = "  cdq\n"
printAsmInstruction (Idiv operand)             = "  idivl " ++ printAsmOperand operand ++ "\n"
printAsmInstruction (AllocStack num)           = "  subq " ++ "$" ++ show num ++ ", " ++ "%rsp\n"
printAsmInstruction (Cmp src dest)             = "  cmpl " ++ printAsmOperand src ++ ", " ++ printAsmOperand dest ++ " \n"
printAsmInstruction (Jmp label)                = "  jmp " ++ printAsmLabel label ++ " \n"
printAsmInstruction (JmpCC code label)         = "  j" ++ printAsmCondCode code ++ " " ++ printAsmLabel label ++ " \n"
printAsmInstruction (SetCC code src)           = "  set" ++ printAsmCondCode code ++ " " ++ printAsmOperandByte src ++ " \n"
printAsmInstruction (Label (AsmLabel name))    = ".L" ++ name ++ ":\n"
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
