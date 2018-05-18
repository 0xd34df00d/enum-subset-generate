{-# LANGUAGE TemplateHaskell #-}

import Data.MakeEnum

import Enum

makeEnumWith ''OtherModuleEnum ['OMEUnknown]
  defaultOptions { newEnumName = Just "OtherDerivedEnum" }

main :: IO ()
main = putStrLn "If it compiles, it works"
