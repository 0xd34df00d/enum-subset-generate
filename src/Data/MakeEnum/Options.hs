module Data.MakeEnum.Options(
    OptionsT(..),
    Options,
    defaultOptions
  ) where

data OptionsT f = Options
  { newEnumName :: f String
  , fromFunctionName :: f String
  , toFunctionName :: f String
  , ctorNameModifier :: String -> String
  }

type Options = OptionsT Maybe

defaultOptions :: Options
defaultOptions = Options Nothing Nothing Nothing id
