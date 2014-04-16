{-# LANGUAGE
    GADTs
  , PolyKinds
  , DataKinds
  , ConstraintKinds
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
  -- singletons
  , ScopedTypeVariables
  , TemplateHaskell
  , RankNTypes
  , KindSignatures
  , FlexibleContexts
  , FlexibleInstances
  , QuasiQuotes #-}

module Language.ARM.Types where

import Data.Singletons.TH

-- | ARM general purpose registers, stack pointer, link register and program counter.
data Register
  = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12
  | SP | LR | PC

$(genSingletons [''Register])

-- | A flexible second operand (@Operand2@) can be:
--
-- * constant
-- * register with optional shift
--
data FlexibleOperand :: [Effect] -> * where
  Constant :: FlexibleOperand '[]
  Register :: SRegister m -> FlexibleOperand ('Read m ': '[])
  -- Shifts
  ASR  :: N -> SRegister m -> FlexibleOperand ('Read m ': '[])
  LSL  :: N -> SRegister m -> FlexibleOperand ('Read m ': '[])
  ROR  :: N -> SRegister m -> FlexibleOperand ('Read m ': '[])
  RRX  :: N -> SRegister m -> FlexibleOperand ('Read m ': '[])
  -- Register-controlled shifts
  ASRn :: SRegister s -> SRegister m -> FlexibleOperand ['Read s, 'Read m]
  LSLn :: SRegister s -> SRegister m -> FlexibleOperand ['Read s, 'Read m]
  RORn :: SRegister s -> SRegister m -> FlexibleOperand ['Read s, 'Read m]
  RRXn :: SRegister s -> SRegister m -> FlexibleOperand ['Read s, 'Read m]

type N = Int

-- | Effect on a specific register or on some memory.
data Effect
  = Read Register | Write Register
  | Load | Store

-- | Instruction Set
data Ins :: [Effect] -> * where
  -- Add, Subtract, Reverse Subtract with/without curry
  ADD :: SRegister d       -- destination
      -> SRegister n       -- register holding the first operand
      -> FlexibleOperand e -- flexible second operand
      -> Ins (['Read d, 'Read n] :++: e)
  -- Load and Store with register offset
  STR :: SRegister t -- register holding the value to store
      -> SRegister n -- register holding the base address
      -> SRegister m -- register holding the offset value
      -> Ins ['Read t, 'Read n, 'Read m, 'Store]

type family (:+:) (a :: e) (as :: [e]) :: [e] where
  (:+:) x '[]        = '[x]
  (:+:) x  (x ': ys) = x ': ys
  (:+:) x  (y ': ys) = y ': (x :+: ys)

type family (:++:) (a :: [e]) (b :: [e]) :: [e] where
  (:++:) '[] ys = ys
  (:++:) (x ': xs) ys = x :+: (xs :++: ys)

data Block :: [Effect] -> * where
     Nil :: Block '[]
     Cons :: Ins e1 -> Block e2 -> Block (e1 :++: e2)
