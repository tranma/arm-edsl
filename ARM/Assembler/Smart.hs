{-# LANGUAGE
    GADTs
  , DataKinds
  , TypeOperators
  #-}

module ARM.Assembler.Smart

where

import ARM.Processor.Base
import ARM.Assembler.Types

--------------------------------------------------------------------------------
-- Arithmetic ------------------------------------------------------------------

_add = mk1 ADD
_sub = mk1 SUB

mk1 f True c SSP SSP e@(Reg _)               = f True c SSP SSP e
mk1 f True c SSP SSP e@(RegShiftN _ (LSL 1)) = f True c SSP SSP e
mk1 f True c SSP SSP e@(RegShiftN _ (LSL 2)) = f True c SSP SSP e
mk1 f True c SSP SSP e@(RegShiftN _ (LSL 3)) = f True c SSP SSP e
mk1 _ _    _ SSP _   _       = error $ "Cannot use SP for Rd"
mk1 _ _    _ SPC _   _       = error $ "Cannot use PC for Rd"
mk1 _ _    _ _   SPC _       = error $ "Cannot use PC for Rn"
mk1 _ _    _ _   _   (RegShiftR _ s)
  | Just SPC <- takeShiftR s = error $ "Cannot use PC for Shift"
mk1 f b c d n e = f b c d n e

_rsb = mk2 RSB
_adc = mk2 ADC
_sbc = mk2 SBC
_rsc = mk2 RSC

mk2 f True c SSP SSP e@(Reg _)               = f True c SSP SSP e
mk2 f True c SSP SSP e@(RegShiftN _ (LSL 1)) = f True c SSP SSP e
mk2 f True c SSP SSP e@(RegShiftN _ (LSL 2)) = f True c SSP SSP e
mk2 f True c SSP SSP e@(RegShiftN _ (LSL 3)) = f True c SSP SSP e
mk2 _ _    _ SSP _   _       = error $ "Cannot use SP for Rd"
mk2 _ _    _ _   SSP _       = error $ "Cannot use SP for Rn"
mk2 _ _    _ SPC _   _       = error $ "Cannot use PC for Rd"
mk2 _ _    _ _   SPC _       = error $ "Cannot use PC for Rn"
mk2 _ _    _ _   _   (RegShiftR _ s)
  | Just SPC <- takeShiftR s = error $ "Cannot use PC for Shift"
mk2 f b c d n e = f b c d n e

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

takeShiftR (ASR s) = Just s
takeShiftR (LSL s) = Just s
takeShiftR (LSR s) = Just s
takeShiftR (ROR s) = Just s 
takeShiftR RRX     = Nothing
