{-# LANGUAGE TemplateHaskell #-}
-- | Generate code used elsewhere in the package.
module ARM.Utils.TH
       ( genClassExclude
       , genClassBounded
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

decIns :: Name -> Q Type -> Q Dec
decIns cname t = instanceD (cxt []) (appT (conT cname) t) []

decInsPromoted :: Name -> Name -> Q Dec
decInsPromoted cname c = decIns cname (promotedT c)

-- | Guarantee uniqueness yourself
prefixName :: String -> Name -> Name
prefixName p n = mkName $ p ++ (nameBase n)


genClassBounded :: String -> Int -> Name -> Q [Dec]
genClassBounded s i tname
  | i < 2 = error $ "at least 2"
  | otherwise = do
      TyConI (DataD _ _ _ ((NormalC zn [])
                          :(NormalC sn [(_, ConT _)])
                          :[]) _) <- reify tname
      let cname = mkName s
      let mkSuccT p = appT (promotedT sn) p
      let succs = take i $ iterate mkSuccT $ promotedT zn
      cdec <- decClass tname cname
      cins <- mapM (decIns cname) succs
      return $ cdec:cins

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
  cins <- mapM (decInsPromoted cname) $ delete con $ map conName cons
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
