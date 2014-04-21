{-# LANGUAGE
    GADTs
  , DataKinds
  , TypeOperators
  , TemplateHaskell
  , Rank2Types
  #-}

-- | Smart and Pretty constructors
--   deal with the exceptions and corner cases when constructing
--   ARM assembly instructions.
--
module ARM.Assembler.Smart
     ( -- Instructions
       add, sub, rsb, adc, sbc, rsc )
where

import ARM.Processor.Base
import ARM.Assembler.Types

-- Arithmetic ------------------------------------------------------------------

add, sub
  :: ( NotPC d, NotPC n
     , NotPC m, NotPC s )
   => InsArithmetic d n m s
add s cond rd rn op
  | (or         [ accept_sp s rd rn op ]) ||
    (not . or $ [ reject_sp rd ])
  = ADD s cond rd rn op
  | otherwise = error $ "ADD: rejected usage of SP as Rd."
sub s cond rd rn op
  | (or         [ accept_sp s rd rn op ]) ||
    (not . or $ [ reject_sp rd ])
  = SUB s cond rd rn op
  | otherwise = error $ "SUB: rejected usage of SP as Rd."

rsb, adc, sbc, rsc
  :: ( NotSP d, NotSP n
     , NotPC d, NotPC n
     , NotPC m, NotPC s )
   => InsArithmetic d n m s
rsb = RSB
adc = ADC
sbc = SBC
rsc = RSC

accept_sp :: Bool -> SReg d -> SReg n -> Op m s e -> Bool
accept_sp True SSP SSP (ShiftReg (NoShift _)) = True
accept_sp True SSP SSP (ShiftReg (LSL_n _ 1)) = True
accept_sp True SSP SSP (ShiftReg (LSL_n _ 2)) = True
accept_sp True SSP SSP (ShiftReg (LSL_n _ 3)) = True
accept_sp _ _ _ _ = False

-- Memory Access ---------------------------------------------------------------

accept_offset :: MemType a -> OffsetReg n m s i e -> Bool
accept_offset m r
  | Just n <- takeOffsetN r = inRange m n
  | otherwise = True
  where takeOffsetN :: OffsetReg m n s i e -> Maybe Int
        takeOffsetN (OffsetRegN_ _ n)   = Just n
        takeOffsetN (OffsetRegN  _ n _) = Just n
        takeOffsetN _ = Nothing

inRange :: MemType a -> Int -> Bool
inRange W  n | range_4096 n = True
inRange B  n | range_4096 n = True
inRange SB n | range_256  n = True
inRange H  n | range_256  n = True
inRange SH n | range_256  n = True
inRange _ _ = False

range_256  n = n >= (-255)  && n <= 255
range_4096 n = n >= (-4095) && n <= 4095

-- Helpers ---------------------------------------------------------------------

reject_sp :: SReg r -> Bool
reject_sp SSP = True
reject_sp _   = False
