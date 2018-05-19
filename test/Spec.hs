{-# LANGUAGE TemplateHaskell #-}

import Data.MakeEnum
import Data.MakeEnum.Options

import Enum

makeEnumWith ''OtherModuleEnum ['OMEUnknown]
  defaultOptions { newEnumName = Just "OtherModuleEnumDerived" }

data SameModuleEnum = SMEUnknown | SMEVar1 | SMEVar2 | SMEVar3 deriving (Eq, Ord, Show)

makeEnumWith ''SameModuleEnum ['SMEUnknown]
  defaultOptions { newEnumName = Just "SameModuleEnumDerived", ctorNameModifier = ("Der" ++) }

main :: IO ()
main = putStrLn "If it compiles, it works"
