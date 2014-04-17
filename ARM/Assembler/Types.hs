{-# LANGUAGE
    GADTs
  , PolyKinds
  , DataKinds
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
  #-}

module ARM.Assembler.Types
       ( N
       , Block(..)
       , Ins(..)
       , FlexibleOperand(..)
       , Shift(..)
       , CondCode(..)
       , MemType(..)
       , Effect(..)
       , (:+:)
       , (:++:)
       )
where

import ARM.Processor.Base

type N = Int

-- | A flexible second operand (@Operand2@) can be:
--
-- * constant
-- * register with optional shift
--
data FlexibleOperand :: [Effect] -> * where
  Constant  :: FlexibleOperand '[]
  Reg       :: SReg m
            -> FlexibleOperand ('Read m ': '[])
  RegShiftN :: SReg m
            -> Shift N
            -> FlexibleOperand ('Read m ': '[])
  RegShiftR :: SReg m
            -> Shift (SReg s)
            -> FlexibleOperand ['Read s, 'Read m]

-- | Shift operations (used in conjunction with instruction operands), with:
--
-- * some number of bits (@N@)
-- * register-controlled (@SReg s@), using the least significant byte
--
data Shift s where
  ASR :: s -> Shift s -- Arithmetic shift right, n in [1..32]
  LSL :: s -> Shift s -- Logical shift left, n in [0..31]
  LSR :: s -> Shift s -- Logical shift right, n in [1..32]
  ROR :: s -> Shift s -- Rotate right, n in [1..32]
  RRX :: Shift ()     -- Rotate right one bit, with extend

-- | Conditional code suffixes
data CondCode
  = EQ      -- ^ Z set
  | NE      -- ^ Z clear
  | CS | HS -- ^ C set
  | CC | LO -- ^ C clear
  | MI      -- ^ N set
  | PL      -- ^ N clear
  | VS      -- ^ V set
  | VC      -- ^ V clear
  | HI      -- ^ C set and Z clear
  | LS      -- ^ C clear or Z set
  | GE      -- ^ N == V
  | LT      -- ^ N != V
  | GT      -- ^ Z clear, N == V
  | LE      -- ^ Z set, N != V
  | AL      -- ^ Any (default)  

-- | Memory type
data MemType s where
  B  :: MemType ()      -- unsigned byte
  SB :: MemType Signed  -- signed byte
  H  :: MemType ()      -- unsigned halfword
  SH :: MemType Signed  -- signed halfword
  W  :: MemType ()      -- word
data Signed

-- | Effect on a specific register or on some memory.
data Effect
  = Read Reg | Write Reg
  | Load | Store

-- | Effect set insert
type family (:+:) (a :: e) (as :: [e]) :: [e] where
  (:+:) x '[]        = '[x]
  (:+:) x  (x ': ys) = x ': ys
  (:+:) x  (y ': ys) = y ': (x :+: ys)

-- | Effect set merge
type family (:++:) (a :: [e]) (b :: [e]) :: [e] where
  (:++:) '[] ys = ys
  (:++:) (x ': xs) ys = x :+: (xs :++: ys)
  
-- | Instruction Set
data Ins :: [Effect] -> * where
  -- Add, Subtract, Reverse Subtract with/without curry
  ADD :: Bool              -- update arithmetic flags?
      -> CondCode          -- conditional execution
      -> SReg d            -- destination
      -> SReg n            -- register holding the first operand
      -> FlexibleOperand e -- flexible second operand
      -> Ins (['Write d, 'Read n] :++: e)
  SUB :: Bool
      -> CondCode
      -> SReg d -> SReg n
      -> FlexibleOperand e
      -> Ins (['Write d, 'Read n] :++: e) 
  RSB :: Bool
      -> CondCode
      -> SReg d -> SReg n
      -> FlexibleOperand e
      -> Ins (['Write d, 'Read n] :++: e) 
  ADC :: Bool
      -> CondCode
      -> SReg d -> SReg n
      -> FlexibleOperand e
      -> Ins (['Write d, 'Read n] :++: e) 
  SBC :: Bool
      -> CondCode
      -> SReg d -> SReg n
      -> FlexibleOperand e
      -> Ins (['Write d, 'Read n] :++: e) 
  RSC :: Bool
      -> CondCode
      -> SReg d -> SReg n
      -> FlexibleOperand e
      -> Ins (['Write d, 'Read n] :++: e) 

  -- Load and Store (register offset)
  LDR :: MemType a   -- type to load
      -> CondCode    -- conditional execution
      -> SReg t      -- register holding the value to load 
      -> SReg n      -- register holding the base address
      -> SReg m      -- register holding the offset value
      -> Shift N     -- offset shift 
      -> Ins ['Read t, 'Read n, 'Read m, 'Store]
  STR :: MemType ()  -- type to store, no signed
      -> CondCode    
      -> SReg t
      -> SReg n
      -> SReg m
      -> Shift N
      -> Ins ['Read t, 'Read n, 'Read m, 'Store]

data Block :: [Effect] -> * where
     Nil :: Block '[]
     Cons :: Ins e1 -> Block e2 -> Block (e1 :++: e2)
