module ParserOptions
  ( parserOpts,
    ParserType (..),
    opts,
  )
where

import BaseSetFunctions
import Options.Applicative
import SharedTypes

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
