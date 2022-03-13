{-# LANGUAGE TemplateHaskell #-}

module ParserSpec
  ( mainParserTests,
  )
where

import Control.Monad
import Options.Applicative
import qualified ParserOptions
import System.Exit
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Property

assertError ::
  Show a =>
  ParserResult a ->
  (ParserFailure ParserHelp -> Property) ->
  Property
assertError x f = case x of
  Success r -> counterexample ("expected failure, got success: " ++ show r) failed
  Failure e -> f e
  CompletionInvoked _ -> counterexample "expected failure, got completion" failed

checkHelpTextWith ::
  Show a =>
  ExitCode ->
  ParserPrefs ->
  String ->
  ParserInfo a ->
  [String] ->
  Property
checkHelpTextWith ecode pprefs name p args = ioProperty $ do
  let result = execParserPure pprefs p args
  expected <- readFile $ "test/" ++ name ++ ".err.txt"
  return $
    assertError result $ \failure ->
      let (msg, code) = renderFailure failure name
       in (expected === msg ++ "\n") .&&. (ecode === code)

checkHelpText :: Show a => String -> ParserInfo a -> [String] -> Property
checkHelpText = checkHelpTextWith ExitSuccess defaultPrefs

prop_combinatorial :: Property
prop_combinatorial =
  once $
    checkHelpText "sample-set-size" ParserOptions.opts ["--help"]

return []

mainParserTests :: IO ()
mainParserTests = do
  result <- $(quickCheckAll)
  unless result exitFailure
