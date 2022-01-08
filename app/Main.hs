module Main where

import CombinatorialMethods (variateRep)
import Options.Applicative
import ParserOptions (invokeParser)

main :: IO ()
main = invokeParser
