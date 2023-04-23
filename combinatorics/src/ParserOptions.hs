module ParserOptions where

import CombinatorialMethods
import Options.Applicative
import SharedTypes
import Text.Show.Pretty

parserOpts :: Parser ParserType
parserOpts =
  ParserType
    <$> many (argument str (metavar "SET"))
    <*> option
      auto
      ( long "sample-set-size"
          <> metavar "K"
          <> help "Specify the sample size"
      )

opts :: ParserInfo ParserType
opts =
  info
    (parserOpts <**> helper)
    ( fullDesc
        <> progDesc
          "Prints all possible combinations of the elements of\
          \ SET with sample size K where repetitions allowed."
        <> header "combine - a combinatorial tool"
    )

invokeParser :: IO ()
invokeParser = displayCombinations =<< execParser opts
  where
    displayCombinations :: ParserType -> IO ()
    displayCombinations (ParserType s k) =
      putStrLn $ ppShow (variateRep k s)
