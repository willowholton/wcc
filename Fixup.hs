module Fixup where

import Asm
import qualified Data.Map as Map

-- fixup a single operand, takes a data table and an asmoperand, returns the updated operand and the updated table:
fixupOperand :: Map.Map String Int -> (AsmOperand -> (AsmOperand, Map.Map String Int))
-- find a pseudo register and look up it's name in the table:
fixupOperand table (PseudoReg (Pseudo name)) =
    case Map.lookup name table of
        -- lookup returns a maybe int (the stack offset value), so return the Stack Int asmOperand with that value if found:
        Just offset -> (Stack offset, table)
        -- otherwise that new key needs to be added to the table at a new offset value:
        Nothing ->
            -- new offset is the total num of items in the table times 4 bytes each, negative bc stack grows downwards
            let offset = negate ((Map.size table + 1) * 4)
                tableNew = Map.insert name offset table
            -- return new Stack operand with new offset, plus the updated table:
            in (Stack offset, tableNew)
-- any other operand besides pseudoreg can just be returned:
fixupOperand table other = (other, table)

-- go through one instruction and fixup each operand, returning an updated instruction and an updated map:
fixupInstruction :: Map.Map String Int -> AsmInstruction -> (AsmInstruction, Map.Map String Int)
-- given a mov src dest instruction,
fixupInstruction table (MovB src dest) = 
    -- fix both source operand and dest operand separately updating the table in between, in case source and dest
    -- end up being the same register:
    let (src1, table1) = fixupOperand table src
        (dest1, table2) = fixupOperand table1 dest
    -- return new instruction with fixed source and dest:
    in (MovB src1 dest1, table2)

fixupInstruction table (Mov src dest) = 
    -- fix both source operand and dest operand separately updating the table in between, in case source and dest
    -- end up being the same register:
    let (src1, table1) = fixupOperand table src
        (dest1, table2) = fixupOperand table1 dest
    -- return new instruction with fixed source and dest:
    in (Mov src1 dest1, table2)

fixupInstruction table (AsmBinary op src dest) = 
    -- fix both source operand and dest operand separately updating the table in between, in case source and dest
    -- end up being the same register:
    let (src1, table1) = fixupOperand table src
        (dest1, table2) = fixupOperand table1 dest
    -- return new instruction with fixed source and dest:
    in (AsmBinary op src1 dest1, table2)

fixupInstruction table (AsmUnary op oper) = 
    let (oper1, table1) = fixupOperand table oper
    in (AsmUnary op oper1, table1)

fixupInstruction table (Idiv src) = 
    let (src1, table1) = fixupOperand table src
    in (Idiv src1, table1)

fixupInstruction table (Cmp src1 src2) = 
    -- fix both operands separately, updating the table in between:
    let (src3, table1) = fixupOperand table src1
        (src4, table2) = fixupOperand table1 src2
    -- return new instruction with fixed sources:
    in (Cmp src3 src4, table2)

fixupInstruction table (SetCC code src) = 
    -- fix up operand first
    let (src1, table1) = fixupOperand table src
    in (SetCC code src1, table1)

-- jmp, jmpcc, label, cdq, ret and allocstack are just themselves, no fixup needed:
fixupInstruction table (Jmp label) = (Jmp label, table)
fixupInstruction table (JmpCC code label) = (JmpCC code label, table)
fixupInstruction table (Label label) = (Label label, table)
fixupInstruction table Cdq = (Cdq, table)
fixupInstruction table Ret = (Ret, table)
fixupInstruction table (AllocStack offset) = (AllocStack offset, table)

-- helper function to determind whether an operand is a stack address or not:
isStack :: AsmOperand -> Bool
isStack (Stack _) = True
isStack _         = False

-- mov instructions can't allow both src and dest to be stack addresses, so any stack addresses created by the fixup
-- functions above that violate this rule need to be updated to use an intermediate scratch register (R10):
fixupMov :: AsmInstruction -> [AsmInstruction]
fixupMov (Mov src dest)
    | isStack src && isStack dest = [Mov src (Reg R10), Mov (Reg R10) dest]
    | otherwise = [Mov src dest]
fixupMov other = [other]

-- idiv can't take a constant operand, e.g. idiv $3 needs to be rewritten as movl $3 %r10d, idivl %r10d:
fixupIdiv :: AsmInstruction -> [AsmInstruction]
fixupIdiv (Idiv (Imm src)) = [Mov (Imm src) (Reg R10), Idiv (Reg R10)]
fixupIdiv other = [other]

-- add and sub can't use memory addresses as both the source and destination:
fixupAddSub :: AsmInstruction -> [AsmInstruction]
fixupAddSub (AsmBinary Add (Stack num1) (Stack num2)) = [Mov (Stack num1) (Reg R10), AsmBinary Add (Reg R10) (Stack num2)]
fixupAddSub (AsmBinary Sub (Stack num1) (Stack num2)) = [Mov (Stack num1) (Reg R10), AsmBinary Sub (Reg R10) (Stack num2)]
fixupAddSub other = [other]

-- and, or, and xor can't do memory-memory operations, also can't have an immediate value as the dest:
fixupBitwise :: AsmInstruction -> [AsmInstruction]
fixupBitwise (AsmBinary BitAnd (Stack num1) (Stack num2)) = [Mov (Stack num1) (Reg R10), AsmBinary BitAnd (Reg R10) (Stack num2)]
fixupBitwise (AsmBinary BitXor (Stack num1) (Stack num2)) = [Mov (Stack num1) (Reg R10), AsmBinary BitXor (Reg R10) (Stack num2)]
fixupBitwise (AsmBinary BitOr (Stack num1) (Stack num2))  = [Mov (Stack num1) (Reg R10), AsmBinary BitOr (Reg R10) (Stack num2)]
fixupBitwise other = [other]

-- Shifts can't have a memory address as the destination if the amount to be shifted is in the cl register:
fixupShifts :: AsmInstruction -> [AsmInstruction]
fixupShifts (AsmBinary BitLShift src (Stack num)) = [(Mov (Stack num) (Reg R10)),
                                                     (AsmBinary BitLShift) src (Reg R10), Mov (Reg R10) (Stack num)]
fixupShifts (AsmBinary BitRShift src (Stack num)) = [(Mov (Stack num) (Reg R10)),
                                                     (AsmBinary BitRShift) src (Reg R10), Mov (Reg R10) (Stack num)]
fixupShifts other = [other]                                                    

-- mul can't have a memory address as the destination, use r11 to avoid collision with r10:
fixupMul :: AsmInstruction -> [AsmInstruction]
fixupMul (AsmBinary Mul src (Stack num)) = [Mov (Stack num) (Reg R11), AsmBinary Mul src (Reg R11), Mov (Reg R11) (Stack num)]
fixupMul other = [other]

-- cmp can't use memory addresses on both sides and also can't have an immediate value as the dest:
fixupCmp :: AsmInstruction -> [AsmInstruction]
fixupCmp (Cmp (Stack num1) (Stack num2)) = [Mov (Stack num1) (Reg R10), Cmp (Reg R10) (Stack num2)]
fixupCmp (Cmp src (Imm num))             = [Mov (Imm num) (Reg R11), Cmp src (Reg R11)]
fixupCmp other = [other]

-- take a data table and a list of instructions, return the fixed up instruction list and the updated table:
fixupInstList :: Map.Map String Int -> ([AsmInstruction] -> ([AsmInstruction], Map.Map String Int))
-- base case, empty list:
fixupInstList table [] = ([], table)
-- recursively fix up each instruction in the list by splitting off the first element one at a time:
fixupInstList table (inst : rem1) =
    let (inst1, table1) = fixupInstruction table inst
        (remInsts, table2) = fixupInstList table1 rem1
    in ((inst1 : remInsts), table2)

-- finally fix up a whole function, which is actually just a list of instructions:
fixupFunction :: AsmFunction -> AsmFunction
fixupFunction (AsmFunction name insts) =
    -- fixupinstlist needs a table, so give it an empty table to fill:
    let (insts1, table) = fixupInstList Map.empty insts
        -- fix any mov, add, sub, mul, and idiv instructions that need fixing:
        insts2 = concatMap fixupMov insts1
        insts3 = concatMap fixupIdiv insts2
        insts4 = concatMap fixupAddSub insts3
        insts5 = concatMap fixupMul insts4
        insts6 = concatMap fixupBitwise insts5
        insts7 = concatMap fixupShifts insts6
        insts8 = concatMap fixupCmp insts7
        -- calculate total size of the stack:
        stackSize = ((Map.size table) * 4)
    -- function needs to know exactly how much space to reserve on the stack FIRST, so add an alloc inst:
    in AsmFunction name (AllocStack stackSize: insts8)

-- an asmProgram is a list of asm functions, so fixupProgram uses map to call fixupFunction on each one: 
fixupProgram :: AsmProgram -> AsmProgram
fixupProgram (AsmProgram funcs) = AsmProgram (map fixupFunction funcs)