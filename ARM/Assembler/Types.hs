{-# LANGUAGE
    GADTs
  , PolyKinds
  , DataKinds
  , TypeFamilies
  , TypeOperators
  , ScopedTypeVariables
  , FlexibleInstances
  , UndecidableInstances
  , Rank2Types
  , MultiParamTypeClasses
  , ConstraintKinds
  , TemplateHaskell
  #-}

module ARM.Assembler.Types
       ( -- Program
         Block(..)
         -- Instruction
       , Ins(..)
       , InsArithmetic
       , Cond(..), Data(..)
       , Effect(..), (:+:), (:++:)
         -- Register shifts and offsets
       , ShiftReg(..)
       , OffsetReg(..), Index(..), SIndex(..), PM(..)
         -- Operands
       , Op(..)
       )
where

import Data.Singletons.TH
import Data.Proxy
import GHC.TypeLits
import GHC.Exts (Constraint)

import ARM.Processor.Base

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

-- | Effect set membership
type family Elem (a :: e) (b :: [e]) :: Bool where
  Elem x '[]       = False
  Elem x (x ': ys) = True
  Elem x (y ': ys) = Elem x ys

-- Data Processing Operations --------------------------------------------------

-- | Shifted registers
data ShiftReg
     :: Reg      -- m Register to be controlled
     -> *        -- s What controls the shift?
     -> [Effect] -- e Effects of the shift
     -> * where
  NoShift :: SReg m -> ShiftReg m () '[]
  -- Value shifts
  ASR_n :: (KnownNat n, 1 <= n, n <= 32)
        => SReg m -> Proxy n -> ShiftReg m N ('Read m ': '[])
  LSL_n :: (KnownNat n, 0 <= n, n <= 31)
        => SReg m -> Proxy n -> ShiftReg m N ('Read m ': '[])
  LSR_n :: (KnownNat n, 1 <= n, n <= 32)
        => SReg m -> Proxy n -> ShiftReg m N ('Read m ': '[])
  ROR_n :: (KnownNat n, 1 <= n, n <= 31)
        => SReg m -> Proxy n -> ShiftReg m N ('Read m ': '[])
  RRX_n :: SReg m -> ShiftReg m N ('Read m ': '[])
  -- Register-controlled shifts
  ASR_s :: SReg m -> SReg s -> ShiftReg m (SReg s) ['Read m, 'Read s]
  LSL_s :: SReg m -> SReg s -> ShiftReg m (SReg s) ['Read m, 'Read s]
  LSR_s :: SReg m -> SReg s -> ShiftReg m (SReg s) ['Read m, 'Read s]
  ROR_s :: SReg m -> SReg s -> ShiftReg m (SReg s) ['Read m, 'Read s]


-- | Immediate constant is an 8-bit value with a 4-bit right rotation.
--   It has a finite range of possible values, but currently we don't check
--   this at compile time. (maybe there's no point)
--
type Immediate = Int

-- | Flexible second operand (@Operand2@)
data Op
     :: *        -- m a register or immediate constant
     -> *        -- s If op is a register: the shift
     -> [Effect] -- e If op is a register: effects of the shift
     -> * where
  Constant :: Immediate -> Op Immediate () '[]
  ShiftReg :: ShiftReg m s e -> Op (SReg m) s e

-- | Constant for offsets (range is too large to be encoded in the types)
type N = Int

-- | OffsetRegs
data OffsetReg
     :: Reg      -- n Register to be offsetted
     -> *        -- m What controls the offset?
     -> *        -- s If offset is controlled by a register: the shift
     -> Index    -- i Pre or post indexed?
     -> [Effect] -- e Effects of the offset
     -> * where

  -- [Rn {, #offset}]       pre-indexed, no writeback, immediate
  -- [Rn, +/-Rm {, shift}]  pre-indexed, no writeback, register-controlled.
  -- referred to as "immediate form" in the docs.
  OffsetRegN_ :: SReg n -> N
              -> OffsetReg n () () 'Pre ('Read n ': '[])
  OffsetRegR_ :: SReg n -> PM -> (ShiftReg m s e)
              -> OffsetReg n (SReg m) s 'Pre (('Read n) :+: e)

  -- [Rn, #offset]! pre-indexed, writeback, immediate
  -- [Rn, +/-Rm {, shift}]! pre-indexed, writeback, register-controlled.
  -- referred to as "pre-indexed form" in the docs.
  -- [Rn], #offset  post-indexed, writeback, immediate.
  -- [Rn], +/-Rm {, shift}  post-indexed, writeback, register-controlled.
  -- referred to as "post-indexed form" in the docs
  OffsetRegN  :: SReg n -> N -> (SIndex i)
              -> OffsetReg n () () i ['Read n, 'Write n]
  OffsetRegR  :: SReg n -> PM -> (ShiftReg m s e) -> (SIndex i)
              -> OffsetReg n (SReg m) s i (['Read n, 'Write n] :++: e)

data PM = Plus | Minus
data Index = Pre | Post
data SIndex n where
  SPre :: SIndex Pre
  SPost :: SIndex Post

-- Instructions ----------------------------------------------------------------


-- | Memory type
data Data = Byte | SignedByte | Halfword | SignedHalfword | Word

$(genSingletons [''Data])

-- | Conditional code suffixes
data Cond
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
  deriving Show

type InsArithmetic d n m s
  = forall e .
         Bool
      -> Cond
      -> SReg d -> SReg n
      -> Op (SReg m) (SReg s) e
      -> Ins (['Write d, 'Read n] :++: e)

class NotPC' (a :: *)
instance NotPC' ()
instance NotPC' Immediate
instance NotPC r => NotPC' (SReg r)

-- | Instruction Set
data Ins :: [Effect] -> * where
  -- Add, Subtract, Reverse Subtract with/without curry
  ADD :: ( NotPC d, NotPC n
         , NotPC' s, NotPC' m)
      => Bool                   -- update arithmetic flags?
      -> Cond                   -- conditional execution
      -> SReg d                 -- destination
      -> SReg n                 -- register holding the first operand
      -> Op m s e               -- flexible second operand
      -> Ins (['Write d, 'Read n] :++: e)
  SUB :: ( NotPC d, NotPC n
         , NotPC' s, NotPC' m)
      => Bool
      -> Cond
      -> SReg d -> SReg n
      -> Op m s e
      -> Ins (['Write d, 'Read n] :++: e)
  RSB :: ( NotSP d, NotSP n
         , NotPC d, NotPC n
         , NotPC' s, NotPC' m)
      => Bool
      -> Cond
      -> SReg d -> SReg n
      -> Op m s e
      -> Ins (['Write d, 'Read n] :++: e)
  ADC :: ( NotSP d, NotSP n
         , NotPC d, NotPC n
         , NotPC' s, NotPC' m)
      => Bool
      -> Cond
      -> SReg d -> SReg n
      -> Op m s e
      -> Ins (['Write d, 'Read n] :++: e)
  SBC :: ( NotSP d, NotSP n
         , NotPC d, NotPC n
         , NotPC' s, NotPC' m)
      => Bool
      -> Cond
      -> SReg d -> SReg n
      -> Op m s e
      -> Ins (['Write d, 'Read n] :++: e)
  RSC :: ( NotSP d, NotSP n
         , NotPC d, NotPC n
         , NotPC' s, NotPC' m)
      => Bool
      -> Cond
      -> SReg d -> SReg n
      -> Op m s e
      -> Ins (['Write d, 'Read n] :++: e)

  -- Load and Store (immediate and register-controlled offset)
  LDR  :: ( EvenReg t
          , NotLR   t
          , NotR12  t                 -- "strongly recommended"
          , Elem ('Write t) e ~ False -- If there is writeback, Rt != Rn
          , WordOrNotPC d t           -- PC as Rt is allowed if type is Word
          , AllowedOffset d m e
          )
       => SData d             -- Type to load
       -> Cond                -- Conditional execution
       -> SReg t              -- Register holding the value to load
       -> OffsetReg n (SReg m) s i e -- Registers holding the base address & offset
       -> Ins (['Read t, 'Store] :++: e)
  STR  :: ( EvenReg t
          , NotLR   t
          , NotR12  t
          , Elem ('Write t) e ~ False
          , WordOrNotPC d t   -- PC as Rd is allowed if type is Word
          , NoPCWrite e       -- PC as Rn is allowed if there is no writeback
          , AllowedOffset d m e
          )
       => SData d
       -> Cond
       -> SReg t
       -> OffsetReg n (SReg m) s i e
       -> Ins (['Read t, 'Store] :++: e)
  LDRD :: ( EvenReg t
          , NotLR   t
          , NotR12  t
          , Elem ('Write t)  e ~ False
          , t2 ~ SuccReg t
          , Elem ('Write t2) e ~ False
          , m :!~: t, m :!~: t2
          , Elem ('Read m) e ~ False -- No shift allowed
          )
       => Cond
       -> SReg t
       -> SReg t2
       -> OffsetReg n (SReg m) s i e
       -> Ins (['Read t, 'Store] :++: e)
  STRD :: ( EvenReg t
          , NotLR t
          , NotR12 t
          , Elem ('Write t)  e ~ False
          , t2 ~ SuccReg t
          , NotPC t2
          , Elem ('Write t2) e ~ False
          , Elem ('Read m) e ~ False
          )
       => Cond
       -> SReg t
       -> SReg t2
       -> OffsetReg n (SReg m) s i e
       -> Ins (['Read t, 'Store] :++: e)

type family (:!~:) (a :: Reg) (b :: Reg) :: Constraint where
  (:!~:) x x = FConstraint
  (:!~:) x y = TConstraint

type family WordOrNotPC (d :: Data) (t :: Reg) :: Constraint where
  WordOrNotPC d 'PC = d ~ 'Word
  WordOrNotPC d t   = NotPC t

type family NoPCWrite (e :: [Effect]) :: Constraint where
  NoPCWrite '[] = TConstraint
  NoPCWrite (('Write PC) ': es) = FConstraint
  NoPCWrite (e ': es) = NoPCWrite es

-- | LDR/STR (register offsets) allows normal shifts for word/byte, and
--   disallows shifts otherwise.
--
type family AllowedOffset
  (a :: Data) (b :: Reg) (e :: [Effect]) :: Constraint where
  AllowedOffset 'SignedByte     m e = Elem ('Read m) e ~ False
  AllowedOffset 'Halfword       m e = Elem ('Read m) e ~ False
  AllowedOffset 'SignedHalfword m e = Elem ('Read m) e ~ False
  AllowedOffset d               m e = TConstraint

type TConstraint = True ~ True
type FConstraint = True ~ False

data Block :: [Effect] -> * where
     Nil :: Block '[]
     Cons :: Ins e1 -> Block e2 -> Block (e1 :++: e2)
