{-# LANGUAGE TemplateHaskell #-}
module ARM.Utils.TH
       ( genClassExclude
       , genFamilySucc )
       where

import Data.List
import Control.Monad (liftM)
import Language.Haskell.TH

conName :: Con -> Name
conName (NormalC n _)   = n
conName (RecC    n _)   = n
conName (InfixC  _ n _) = n
conName (ForallC _ _ c) = conName c

decClass :: Name -> Name -> Q Dec
decClass t cname = do
  tv   <- newName "a"
  k    <- liftM (KindedTV tv) $ conT t
  classD (cxt []) cname [k] [] []
  
decInstance :: Name -> Name -> Q Dec
decInstance cname c = instanceD (cxt []) (appT (conT cname) (promotedT c)) []

-- | Guarantee uniqueness yourself
prefixName :: String -> Name -> Name
prefixName p n = mkName $ p ++ (nameBase n)

-- | Generate a typeclass that excludes one constructor of a datatype
--   (the type must be promotable, so no GADTs yet), e.g.
--
-- @
--   class NotA
--   instance NotA B
-- @
--
genClassExclude :: Name -> Name -> Q [Dec]
genClassExclude t con = do
  TyConI (DataD _ _ _ cons _) <- reify t
  let cname = prefixName "Not" con
  cdec <- decClass t cname
  cins <- mapM (decInstance cname) $ delete con $ map conName cons
  return $ cdec:cins

-- | Generate a type family that yields the next constructor
--   (lexicographically) given a constructor, e.g.
--
-- @
--   type family Succ (t :: Foo) :: Foo where
--     Succ First = Second
--     ...
-- @
--
genFamilySucc :: Name -> Q [Dec]
genFamilySucc kn = do
  TyConI (DataD _ _ _ cs _) <- reify kn
  let cons = map conName cs
  let familyName = prefixName "Succ" kn
  let eqn c c' = tySynEqn [promotedT c] (promotedT c')
  tv  <- newName "t"
  k   <- conT kn
  liftM (:[]) $ closedTypeFamilyKindD familyName [KindedTV tv k] k
                                      (zipWith eqn (init cons) (tail cons))
