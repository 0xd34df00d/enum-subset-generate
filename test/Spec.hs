{-# LANGUAGE TemplateHaskell #-}

import Data.MakeEnum

import Enum

makeEnumWith defaultOptions { newEnumName = Just "OtherDerivedEnum" } ''OtherModuleEnum ['OMEUnknown]

main :: IO ()
main = putStrLn "If it compiles, it works"
