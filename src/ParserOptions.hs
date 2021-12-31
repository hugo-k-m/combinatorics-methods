module ParserOptions
  ( parserOpts,
  )
where

import Options.Applicative

data ParserType = ParserType
  { baseSet :: String,
    sampleSetSize :: Int
  }

parserOpts :: Parser ParserType
parserOpts =
  ParserType
    <$> strOption
      ( long "base-set"
          <> metavar "SET"
          <> help "Base set for the combinatorial methods"
      )
    <*> option
      auto
      ( long "sample-set-size"
          <> metavar "K"
          <> help "Specify the sample size"
      )
