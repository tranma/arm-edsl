{-# LANGUAGE
      GADTs
    , ViewPatterns
    , TypeSynonymInstances #-}

module Pretty where

import Data.Singletons
import Text.PrettyPrint.Leijen
import GHC.TypeLits
import ARM.Processor.Base
import ARM.Assembler.Types

instance Pretty Reg where
  pretty = text . show

instance Pretty (SReg r) where
  pretty = pretty . fromSing

instance KnownNat n => Pretty (Proxy n) where
  pretty p = pretty $ show $ natVal p

-- | ^ Shifted registers as operands, e.g.
--
-- @
-- R0 {, ASR #2}
-- R0 {, LSL R1}
-- @
--
instance Pretty (ShiftReg m s e) where
  pretty (NoShift m) = pretty m
  pretty (ASR_n m n) = shiftBraces (pretty m)
                     $ text "ASR" <+> char '#' <> pretty n
  pretty (LSL_n m n) = shiftBraces (pretty m)
                     $ text "LSL" <+> char '#' <> pretty n
  pretty (LSR_n m n) = shiftBraces (pretty m)
                     $ text "LSL" <+> char '#' <> pretty n
  pretty (ROR_n m n) = shiftBraces (pretty m)
                     $ text "LSL" <+> char '#' <> pretty n
  pretty (RRX_n m)   = shiftBraces (pretty m)
                     $ text "RRX"
  pretty (ASR_s m n) = shiftBraces (pretty m)
                     $ text "ASR" <+> pretty n
  pretty (LSL_s m n) = shiftBraces (pretty m)
                     $ text "LSL" <+> pretty n
  pretty (LSR_s m n) = shiftBraces (pretty m)
                     $ text "LSL" <+> pretty n
  pretty (ROR_s m n) = shiftBraces (pretty m)
                     $ text "LSL" <+> pretty n

shiftBraces :: Doc -> Doc -> Doc
shiftBraces m n = m <+> char '{' <> char ',' <+> n <+> char '}'
