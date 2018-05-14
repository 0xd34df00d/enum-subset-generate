{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.MakeEnum(makeEnum) where

import Language.Haskell.TH 

makeEnum :: Name -> [Name] -> Q [Dec]
makeEnum tyName omissions = reify tyName >>= \case
  TyConI (DataD cx name bndrs kind cons derivs) -> do
    let cons' = updateName unmodule <$> filterCons omissions cons
    let dec' = DataD cx (unmodule name) bndrs kind cons' derivs
    pure [dec']
  _ -> fail "unsupported data type"

filterCons :: [Name] -> [Con] -> [Con]
filterCons omit = filter $ (`notElem` omit') . conName
  where omit' = Just <$> omit

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
