{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Char
import Data.List
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework
import Test.HUnit hiding (test)
import Yaskl.Util.Directory

case_sayHi = do "Hello mr. bitch" @=? (sayHi "bitch")

main :: IO ()
main = defaultMain [$testGroupGenerator]
