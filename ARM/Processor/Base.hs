{-# LANGUAGE
    GADTs
  , DataKinds
  , ConstraintKinds
  , TypeFamilies
  , ScopedTypeVariables
  , TemplateHaskell
  #-}

module ARM.Processor.Base
       ( Reg(..)
       , SReg, Sing(..), NotSP, NotLR, NotPC
       , ALUFlags
       ) 
where
    
import Data.Singletons.TH
import ARM.Utils.TH

-- | ARM general purpose registers, stack pointer, link register and program counter.
data Reg
  = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12
  | SP | LR | PC

$(genSingletons [''Reg])

$(genClassExclude ''Reg 'SP)
$(genClassExclude ''Reg 'LR)
$(genClassExclude ''Reg 'PC)
  
-- | ALU status flags (in APSR)
data ALUFlags
  = ALUFlags { flagN :: Bool -- ^ Negative
             , flagZ :: Bool -- ^ Zero
             , flagC :: Bool -- ^ Curry
             , flagV :: Bool -- ^ Overflow
             }
