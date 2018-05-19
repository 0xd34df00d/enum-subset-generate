{-# LANGUAGE TemplateHaskell #-}

import Data.MakeEnum

import Enum

data SameModuleEnum = SMEUnknown | SMEVar1 | SMEVar2 | SMEVar3 deriving (Eq, Ord, Show)

makeEnumWith ''OtherModuleEnum ['OMEUnknown]
  defaultOptions { newEnumName = Just "OtherDerivedEnum" }

makeEnumWith ''SameModuleEnum ['SMEUnknown]
  defaultOptions { newEnumName = Just "SameDerivedEnum", ctorNameModifier = ("Der" ++) }

main :: IO ()
main = putStrLn "If it compiles, it works"
