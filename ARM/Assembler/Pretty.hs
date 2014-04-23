{-# LANGUAGE
      GADTs
    , ViewPatterns
    , TypeSynonymInstances #-}

module ARM.Assembler.Pretty (pretty) where

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
                     $ text "LSR" <+> char '#' <> pretty n
  pretty (ROR_n m n) = shiftBraces (pretty m)
                     $ text "ROR" <+> char '#' <> pretty n
  pretty (RRX_n m)   = shiftBraces (pretty m)
                     $ text "RRX"
  pretty (ASR_s m n) = shiftBraces (pretty m)
                     $ text "ASR" <+> pretty n
  pretty (LSL_s m n) = shiftBraces (pretty m)
                     $ text "LSL" <+> pretty n
  pretty (LSR_s m n) = shiftBraces (pretty m)
                     $ text "LSR" <+> pretty n
  pretty (ROR_s m n) = shiftBraces (pretty m)
                     $ text "ROR" <+> pretty n

instance Pretty (Op m s e) where
  pretty (Constant n) = pretty $ show n
  pretty (ShiftReg s) = pretty s

instance Pretty Cond where
  pretty AL = empty
  pretty c  = pretty $ show c

instance Pretty (Ins e) where
  pretty (ADD b c d n o) = insArith "ADD" b c d n o
  pretty (SUB b c d n o) = insArith "SUB" b c d n o
  pretty (RSB b c d n o) = insArith "RSB" b c d n o
  pretty (ADC b c d n o) = insArith "ADC" b c d n o
  pretty (SBC b c d n o) = insArith "SBC" b c d n o
  pretty (RSC b c d n o) = insArith "RSC" b c d n o

insArith :: String -> Bool -> Cond -> SReg d -> SReg n -> Op m s e -> Doc
insArith s b c d n o =   text s <> suffix b <> pretty c
                     <+> pretty d <> comma
                     <+> pretty n <> comma
                     <+> pretty o

suffix :: Bool -> Doc
suffix b | b = char 'S'
         | otherwise = empty

shiftBraces :: Doc -> Doc -> Doc
shiftBraces m n = m <+> braces (comma <+> n)
