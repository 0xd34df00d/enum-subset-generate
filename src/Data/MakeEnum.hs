{-# LANGUAGE LambdaCase, RecordWildCards, ViewPatterns #-}

module Data.MakeEnum(
    makeEnum,
    makeEnumWith,
    Options(..)
  ) where

import Control.Monad
import Control.Monad.Extra
import Data.Maybe
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data OptionsT f = Options
  { newEnumName :: f String
  }

type Options = OptionsT Maybe

defaultOptions :: Options
defaultOptions = Options Nothing

makeEnum :: Name -> [Name] -> Q [Dec]
makeEnum = makeEnumWith defaultOptions

makeEnumWith :: Options -> Name -> [Name] -> Q [Dec]
makeEnumWith options tyName omit = reify tyName >>= \case
  TyConI (unwrapDec -> Just dec) -> do
    let (dec', origCons, name) = buildReducedEnum options omit' dec
    (fromSig, fromFun) <- buildFromFun options name origCons
    pure [dec', fromSig, fromFun]
  _ -> fail "unsupported type"
  where omit' = Just <$> omit

data DataDef = DataDef Cxt Name [TyVarBndr] (Maybe Kind) [Con] [DerivClause]

unwrapDec :: Dec -> Maybe DataDef
unwrapDec (DataD cx name bndrs kind cons derivs) = Just $ DataDef cx name bndrs kind cons derivs
unwrapDec _ = Nothing

buildReducedEnum :: Options -> [Maybe Name] -> DataDef -> (Dec, [Con], Name)
buildReducedEnum Options { .. } omit (DataDef cx name bndrs kind cons derivs) = (DataD cx name' bndrs kind cons' derivs, filtered, name)
  where filtered = filterCons omit cons
        cons' = updateName unmodule <$> filtered
        name' = fromMaybe (unmodule name) $ mkName <$> newEnumName

buildFromFun :: Name -> [Con] -> Q (Dec, Dec)
buildFromFun name cons = do
  Module _ (ModName thisModName) <- thisModule

  let funName = mkName $ "from" <> nameBase name
  let funSig = SigD funName $ ArrowT `AppT` ConT name `AppT` (ConT (mkName "Maybe") `AppT` ConT (thisModName <.> name))

  clauses <- mapMaybeM (mkClause thisModName) cons
  let fallback = Clause [WildP] (NormalB $ ConE $ mkName "Nothing") []
  let funDef = FunD funName $ clauses ++ [fallback]

  pure (funSig, funDef)

  where
    mkClause thisModName (NormalC n ts) = do
      binders <- replicateM (length ts) $ newName "p"
      let body = NormalB $ AppE (ConE $ mkName "Just") $ ConE $ thisModName <.> n
      pure $ Just $ Clause [ConP n $ VarP <$> binders] body []
    mkClause _ p = fail $ "this type of constructor is not supported yet:\n" <> pprint p

(<.>) :: String -> Name -> Name
(<.>) modName n = mkName $ modName <> "." <> nameBase n

filterCons :: [Maybe Name] -> [Con] -> [Con]
filterCons omit = filter $ (`notElem` omit) . conName

conName :: Con -> Maybe Name
conName (NormalC n _) = Just n
conName (RecC n _) = Just n
conName (InfixC _ n _) = Just n
conName (ForallC _ _ n) = conName n
conName GadtC {} = Nothing
conName RecGadtC {} = Nothing

updateName :: (Name -> Name) -> Con -> Con
updateName f (NormalC n bts) = NormalC (f n) bts
updateName f (RecC n vbts) = RecC (f n) vbts
updateName f (InfixC bt1 n bt2) = InfixC bt1 (f n) bt2
updateName f (ForallC bndrs cx con) = ForallC bndrs cx $ updateName f con
updateName _ g@GadtC {} = g
updateName _ r@RecGadtC {} = r

unmodule :: Name -> Name
unmodule = mkName . nameBase
