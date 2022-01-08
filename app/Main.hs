module Main where

import CombinatorialMethods (variateRep)
import Options.Applicative
import ParserOptions (invokeParser)

main :: IO ()
main = invokeParser

-- main = do
--   let n = ["defined", "undefined"]
--   k <- readLn
--   let result = variateRep k n
--   putStrLn $ "List: " ++ show result ++ "\n"
--   putStrLn $ "Length: " ++ show (length result)
