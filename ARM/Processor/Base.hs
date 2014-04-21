{-# LANGUAGE
    GADTs
  , DataKinds
  , PolyKinds
  , ConstraintKinds
  , TypeFamilies
  , ScopedTypeVariables
  , TemplateHaskell
  #-}

module ARM.Processor.Base
       ( Reg(..)
       , ALUFlags(..)
       -- generated
       , SReg, Sing(..)
       , NotSP, NotLR, NotPC, NotR12
       , EvenReg, OddReg, SuccReg
       ) 
where
    
import Data.Singletons.TH
import ARM.Utils.TH

-- | ARM general purpose registers,
--   stack pointer, link register and program counter.
data Reg
  = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12
  | SP | LR | PC

-- | Types that correspond to the promoted Reg and has one inhabitant.
$(genSingletons [''Reg])

$(genClassExclude ''Reg 'R12)
$(genClassExclude ''Reg 'SP)
$(genClassExclude ''Reg 'LR)
$(genClassExclude ''Reg 'PC)

class EvenReg (r :: Reg)
class OddReg (r :: Reg)
instance EvenReg R0
instance EvenReg R2
instance EvenReg R4
instance EvenReg R6
instance EvenReg R8
instance EvenReg R10
instance EvenReg R12
instance OddReg R1
instance OddReg R3
instance OddReg R5
instance OddReg R7
instance OddReg R9
instance OddReg R11
-- Special registers are excluded from Even/Odd rule
instance EvenReg SP
instance EvenReg LR
instance EvenReg PC
instance OddReg SP
instance OddReg LR
instance OddReg PC

$(genFamilySucc ''Reg)

-- | ALU status flags (in APSR)
data ALUFlags
  = ALUFlags { flagN :: Bool   -- ^ Negative
             , flagZ :: Bool   -- ^ Zero
             , flagC :: Bool   -- ^ Curry
             , flagV :: Bool } -- ^ Overflow
