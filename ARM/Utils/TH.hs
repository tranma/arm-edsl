{-# LANGUAGE TemplateHaskell #-}
module ARM.Utils.TH (genClassExclude) where

import Data.List
import Control.Monad
import Language.Haskell.TH

conName :: Con -> Name
conName (NormalC n _)   = n
conName (RecC    n _)   = n
conName (InfixC  _ n _) = n
conName (ForallC _ _ c) = conName c

-- | Generate a typeclass that excludes one constructor of a datatype.
--   (the type must be promotable, so no GADTs yet)
genClassExclude :: Name -> Name -> Q [Dec]
genClassExclude t con = do
  TyConI (DataD _ _ _ cons _) <- reify t
  let cname = mkName $ "Not" ++ (nameBase con)
  let ins c = instanceD (cxt []) (appT (conT cname) (promotedT c)) []
  tv   <- newName "a"
  k    <- liftM (KindedTV tv) $ conT t
  cdec <- classD (cxt []) cname [k] [] []
  cins <- mapM ins $ delete con $ map conName cons
  return $ cdec:cins
