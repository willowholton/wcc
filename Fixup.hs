module Fixup where

import Asm
import qualified Data.Map as Map
import GHC.Base (TrName(TrNameD))

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
fixupInstruction table (Mov src dest) = 
    -- fix both source operand and dest operand separately updating the table in between, in case source and dest
    -- end up being the same register:
    let (src1, table1) = fixupOperand table src
        (dest1, table2) = fixupOperand table1 dest
    -- return new instruction with fixed source and dest:
    in (Mov src1 dest1, table2)

fixupInstruction table (AsmUnary op oper) = 
    let (oper1, table1) = fixupOperand table oper
    in (AsmUnary op oper1, table1)

-- ret and allocstack are just themselves, no fixup needed:
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

-- take a data table and a list of instructions, return the fixed up instruction list and the updated table:
fixupInstList :: Map.Map String Int -> ([AsmInstruction] -> ([AsmInstruction], Map.Map String Int))
-- base case, empty list:
fixupInstList table [] = ([], table)
-- recursively fix up each instruction in the list by splitting off the first element one at a time:
fixupInstList table (inst : rem) =
    let (inst1, table1) = fixupInstruction table inst
        (remInsts, table2) = fixupInstList table1 rem
    in ((inst1 : remInsts), table2)

-- finally fix up a whole function, which is actually just a list of instructions:
fixupFunction :: AsmFunction -> AsmFunction
fixupFunction (AsmFunction name insts) =
    -- fixupinstlist needs a table, so give it an empty table to fill:
    let (insts1, table) = fixupInstList Map.empty insts
        -- fix any movs that need fixing:
        insts2 = concatMap fixupMov insts1
        -- calculate total size of the stack:
        stackSize = ((Map.size table) * 4)
    -- function needs to know exactly how much space to reserve on the stack FIRST, so add an alloc inst:
    in AsmFunction name (AllocStack stackSize: insts2)

-- an asmProgram is a list of asm functions, so fixupProgram uses map to call fixupFunction on each one: 
fixupProgram :: AsmProgram -> AsmProgram
fixupProgram (AsmProgram funcs) = AsmProgram (map fixupFunction funcs)