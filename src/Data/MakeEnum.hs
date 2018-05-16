{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.MakeEnum(makeEnum) where

import Control.Monad
import Control.Monad.Extra
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

makeEnum :: Name -> [Name] -> Q [Dec]
makeEnum tyName omit = reify tyName >>= \case
  TyConI dec ->
    case buildReducedEnum omit' dec of
      Left err -> fail err
      Right (dec', origCons, name) -> do
        fromFun <- buildFromFun name origCons
        runIO $ putStrLn $ pprint fromFun
        pure [dec', fromFun]
  _ -> fail "unsupported type"
  where omit' = Just <$> omit

buildReducedEnum :: [Maybe Name] -> Dec -> Either String (Dec, [Con], String)
buildReducedEnum omit (DataD cx name bndrs kind cons derivs) = Right (DataD cx (unmodule name) bndrs kind cons' derivs, filtered, nameBase name)
  where filtered = filterCons omit cons
        cons' = updateName unmodule <$> filtered
buildReducedEnum _ _ = Left "unsupported type"

buildFromFun :: String -> [Con] -> Q Dec
buildFromFun name cons = do
  Module _ (ModName thisModName) <- thisModule
  clauses <- mapMaybeM (mkClause thisModName) cons
  pure $ FunD (mkName $ "from" <> name) clauses
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
