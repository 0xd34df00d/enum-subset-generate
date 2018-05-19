{-# LANGUAGE LambdaCase, RecordWildCards, ViewPatterns #-}

module Data.MakeEnum(
    makeEnum,
    makeEnumWith,
  ) where

import Control.Monad
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.MakeEnum.Options

makeEnum :: Name -> [Name] -> Q [Dec]
makeEnum tyName omit = makeEnumWith tyName omit defaultOptions

makeEnumWith :: Name -> [Name] -> Options -> Q [Dec]
makeEnumWith tyName omit options = reify tyName >>= \case
  TyConI (unwrapDec -> Just dec) -> do
    let deducedOpts = deduceOptions dec options
    let (dec', origCons, name) = buildReducedEnum deducedOpts omit' dec
    (fromSig, fromFun) <- buildFromFun deducedOpts name origCons
    (toSig, toFun) <- buildToFun deducedOpts name origCons
    pure [dec', fromSig, fromFun, toSig, toFun]
  _ -> fail "unsupported type"
  where omit' = Just <$> omit

data DataDef = DataDef Cxt Name [TyVarBndr] (Maybe Kind) [Con] [DerivClause]
             deriving (Eq, Ord, Show)

unwrapDec :: Dec -> Maybe DataDef
unwrapDec (DataD cx name bndrs kind cons derivs) = Just $ DataDef cx name bndrs kind cons derivs
unwrapDec _ = Nothing

type DeducedOptions = OptionsT Identity

deduceOptions :: DataDef -> Options -> DeducedOptions
deduceOptions (DataDef _ name _ _ _ _) Options { .. } =
  Options
    { newEnumName = Identity $ fromMaybe (nameBase name) newEnumName
    , fromFunctionName = Identity $ fromMaybe ("from" <> nameBase name) fromFunctionName
    , toFunctionName = Identity $ fromMaybe ("to" <> nameBase name) toFunctionName
    , ..
    }

buildReducedEnum :: DeducedOptions -> [Maybe Name] -> DataDef -> (Dec, [Con], Name)
buildReducedEnum Options { .. } omit (DataDef cx name bndrs kind cons derivs) = (DataD cx name' bndrs kind cons' derivs, filtered, name)
  where filtered = filterCons omit cons
        cons' = updateName (mkName . ctorNameModifier . nameBase) <$> filtered
        name' = mkName $ runIdentity newEnumName

buildFromFun :: DeducedOptions -> Name -> [Con] -> Q (Dec, Dec)
buildFromFun Options { .. } name cons = do
  Module _ (ModName thisModName) <- thisModule

  let funName = mkName $ runIdentity fromFunctionName
  let funSig = SigD funName $ ArrowT `AppT` ConT name `AppT` (ConT (mkName "Maybe") `AppT` ConT (mkName $ runIdentity newEnumName))

  clauses <- mapM (mkClause thisModName) cons
  let fallback = Clause [WildP] (NormalB $ ConE $ mkName "Nothing") []
  let funDef = FunD funName $ clauses ++ [fallback]

  pure (funSig, funDef)

  where
    mkClause thisModName (NormalC n ts) = do
      let thisName = mkName $ thisModName <> "." <> ctorNameModifier (nameBase n)
      binders <- replicateM (length ts) $ newName "p"
      let body = NormalB $ ConE (mkName "Just") `AppE` (ConE thisName `foldBinders` binders)
      pure $ Clause [ConP n $ VarP <$> binders] body []
    mkClause _ p = fail $ "this type of constructor is not supported yet:\n" <> pprint p

buildToFun :: DeducedOptions -> Name -> [Con] -> Q (Dec, Dec)
buildToFun Options { .. } name cons = do
  Module _ (ModName thisModName) <- thisModule

  let funName = mkName $ runIdentity toFunctionName
  let funSig = SigD funName $ ArrowT `AppT` ConT (mkName $ runIdentity newEnumName) `AppT` ConT name

  clauses <- mapM (mkClause thisModName) cons
  let funDef = FunD funName clauses

  pure (funSig, funDef)

  where
    mkClause thisModName (NormalC n ts) = do
      let thisName = mkName $ thisModName <> "." <> ctorNameModifier (nameBase n)
      binders <- replicateM (length ts) $ newName "p"
      let body = NormalB $ ConE n `foldBinders` binders
      pure $ Clause [ConP thisName $ VarP <$> binders] body []
    mkClause _ p = fail $ "this type of constructor is not supported yet:\n" <> pprint p

foldBinders :: Exp -> [Name] -> Exp
foldBinders name = foldl AppE name . map VarE

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
