{-# LANGUAGE
    GADTs
  , DataKinds
  , TypeOperators
  , TemplateHaskell
  , Rank2Types
  , ViewPatterns
  #-}

-- | Smart and Pretty constructors
--   deal with the exceptions and corner cases when constructing
--   ARM assembly instructions.
--
module ARM.Assembler.Smart
     ( -- Instructions
       add, sub, rsb, adc, sbc, rsc
     , ldr, str, ldrd, strd
     )
where

import Data.Proxy
import Data.Singletons
import GHC.TypeLits

import ARM.Processor.Base
import ARM.Assembler.Types

-- Arithmetic ------------------------------------------------------------------

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
accept_sp True SSP SSP (ShiftReg (LSL_n _ (natVal -> 1))) = True
accept_sp True SSP SSP (ShiftReg (LSL_n _ (natVal -> 2))) = True
accept_sp True SSP SSP (ShiftReg (LSL_n _ (natVal -> 3))) = True
accept_sp _ _ _ _ = False

reject_sp :: SReg r -> Bool
reject_sp SSP = True
reject_sp _   = False

-- Memory Access ---------------------------------------------------------------

ldr d c t off
  | accept_offset (fromSing d) off = LDR d c t off
  | otherwise = error $ "LDR: immediate offset out of allowed range"
str d c t off
  | accept_offset (fromSing d) off = STR d c t off
  | otherwise = error $ "STR: immediate offset out of allowed range"

ldrd d c t off
  | accept_offset_doubleword off = LDR d c t off
  | otherwise = error $ "LDRD: immediate offset out of allowed range"
strd d c t off
  | accept_offset (fromSing d) off = STR d c t off
  | otherwise = error $ "STRD: immediate offset out of allowed range"

accept_offset :: Data -> OffsetReg n m s i e -> Bool
accept_offset m r
  | Just n <- takeOffsetN r = inRange m n
  | otherwise = True

accept_offset_doubleword :: OffsetReg n m s i e -> Bool
accept_offset_doubleword r
  | Just n <- takeOffsetN r = range_255 n
  | otherwise = True

inRange :: Data -> Int -> Bool
inRange Word           n | range_4095 n = True
inRange Byte           n | range_4095 n = True
inRange SignedByte     n | range_255  n = True
inRange Halfword       n | range_255  n = True
inRange SignedHalfword n | range_255  n = True
inRange _ _ = False

takeOffsetN :: OffsetReg m n s i e -> Maybe Int
takeOffsetN (OffsetRegN_ _ n)   = Just n
takeOffsetN (OffsetRegN  _ n _) = Just n
takeOffsetN _ = Nothing

range_255  n = n >= (-255)  && n <= 255
range_4095 n = n >= (-4095) && n <= 4095
