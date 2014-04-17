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
       , SReg, Sing(..)
       , ALUFlags
       ) 
where
    
import Data.Singletons.TH

-- | ARM general purpose registers, stack pointer, link register and program counter.
data Reg
  = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12
  | SP | LR | PC

$(genSingletons [''Reg])

-- | ALU status flags (in APSR)
data ALUFlags
  = ALUFlags { n :: Bool -- ^ Negative
             , z :: Bool -- ^ Zero
             , c :: Bool -- ^ Curry
             , v :: Bool -- ^ Overflow
             }
