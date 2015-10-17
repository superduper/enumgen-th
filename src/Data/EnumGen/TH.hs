{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Data.EnumGen.TH (
  mkEnum
, enumKey) where

{-

Poor man's Enum TH generator:

Example code:

{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import TH

$(mkEnum "Enumz" $ do
          enumKey "Alpha" 1
          enumKey "Bravo" 2)

Expands into:

data Enumz = Alpha
           | Bravo deriving (Show, Eq, Enum)

toEnumz :: Int -> Maybe Enumz
toEnumz 1 = Just Alpha
toEnumz 2 = Just Bravo
toEnumz _ = Nothing

instance Enum Enumz where
    fromEnum Alpha = 1
    fromEnum Bravo = 2
    toEnum n = case (toEnumz n) of
                   (Just e) -> e
                   Nothing  -> error "Enumz.toEnum: bad argument"


-}

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Control.Monad.Writer

type EnumKey = (String, Int)
type EnumDecl = Writer [EnumKey] ()

enumKey :: String -> Int -> EnumDecl
enumKey k n = tell [(k,n)]

topDeclaration :: String -> [EnumKey] -> Dec
topDeclaration name es =
  let cons   = fmap (\(k, n) -> NormalC (mkName k) []) es
      derivs = [(mkName "Show"), (mkName "Eq"), (mkName "Enum")]
  in DataD [] (mkName name) [] cons derivs

fromEnumFn :: String -> [EnumKey] -> [Dec]
fromEnumFn name es =
  let typName     = mkName name
      funName     = mkName $ "fromEnum"
      fun         = FunD funName $ (fmap mkCl es)
      mkCl (k, n) = Clause [mkPat k] (mkBody n) []
      mkPat  k    = ConP (mkName k) []
      mkBody n    = NormalB $ LitE $ IntegerL $ toInteger n
  in  return fun

toEnumFunName :: String -> Name
toEnumFunName name = mkName $ "to" ++ name
-- | Generates data type specific
toEnumFn :: String -> [EnumKey] -> [Dec]
toEnumFn name es =
  let typName     = mkName name
      funName     = toEnumFunName name
      mkPat  n    = LitP $ IntegerL $ toInteger n
      mkBody k    = NormalB $ AppE (ConE $ mkName "Just") (ConE $ mkName k)
      fun         = FunD funName $ (fmap mkCl es) ++ fallthrough
      mkCl (k, n) = Clause [mkPat n] (mkBody k) []
      fallthrough = [ Clause [WildP] (NormalB $ ConE $ mkName "Nothing") []]
      sig = SigD funName (AppT (AppT ArrowT (ConT $ mkName "Int")) (AppT (ConT $ mkName "Maybe") (ConT typName)))
  in  [sig, fun]

-- | Function that wraps our toEnumFn into a Enum.toEnum compatible function.
-- Basically it errors out if there's no match
wrapEnumFn :: String -> [Dec]
wrapEnumFn name =
  let toEnumFnVar = VarP $ toEnumFunName name
      nVar        = VarP $ mkName "n"
      funName     = mkName "toEnum"
      applyEnum   = AppE (VarE $ toEnumFunName name) (VarE $ mkName "n")
      clauseVars  = [VarP $ mkName "n"]
      caseMatchJ  = Match (ConP (mkName "Just") [VarP $ mkName "e"]) (NormalB $ VarE $ mkName "e") []
      caseMatchN  = Match (ConP (mkName "Nothing") []) (NormalB $ AppE (VarE $ mkName "error") (LitE (StringL errorMsg))) []
      errorMsg    = name ++ ".toEnum: bad argument"
      clCase      = CaseE applyEnum [caseMatchJ, caseMatchN]
      clause      = Clause clauseVars (NormalB clCase)  []
  in  return $ FunD funName [clause]

-- | Enum instance for our data type
enumInstance :: String  -- ^ Type name
             -> [Dec]   -- ^ fromEnum function
             -> [Dec]   -- ^ Result
enumInstance name from =
 let inst = InstanceD [] (AppT (ConT $ mkName "Enum") (ConT $ mkName name)) (from ++ wrap)
     wrap = wrapEnumFn name
 in return inst

-- | Generates a sum data type, Enum instance and simple serializer form Int
--
-- Example usage:
--
-- $(mkEnum "MyDataType" $ do
--           enumKey "Constructor1" 1
--           enumKey "Constructor2" 2)
--
mkEnum :: String     -- ^ Data Type Name
       -> EnumDecl   -- ^ Set of Constructor -> Int declarations
       -> Q [Dec]    -- ^ Result template
mkEnum name decls =
  let decls' = execWriter decls
      topDec = topDeclaration name decls'
      fromF  = (fromEnumFn name decls')
      inst   = enumInstance name fromF
  in return $ [topDec] ++ (toEnumFn name decls') ++ inst


