{-# LANGUAGE TemplateHaskell #-}

import Data.MakeEnum
import Data.MakeEnum.Options

import Enum

makeEnumWith ''OtherModuleEnum ['OMEUnknown]
  defaultOptions { newEnumName = Just "OtherModuleEnumDerived" }

data SameModuleEnum = SMEUnknown | SMEVar1 | SMEVar2 | SMEVar3 deriving (Eq, Ord, Show)

makeEnumWith ''SameModuleEnum ['SMEUnknown]
  defaultOptions { newEnumName = Just "SameModuleEnumDerived", ctorNameModifier = ("Der" ++) }

data EnumWithFields = EWFVar1 String
                    | EWFVar2 Int Int
                    | EWFUnknown
                    deriving (Eq, Ord, Show)

makeEnumWith ''EnumWithFields ['EWFUnknown]
  defaultOptions { newEnumName = Just "EnumWithFieldsDerived", ctorNameModifier = ("Der" ++) }

main :: IO ()
main = putStrLn "If it compiles, it works"
