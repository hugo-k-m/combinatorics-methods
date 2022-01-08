module ParserOptions
  ( parserOpts,
    ParserType (..),
    opts,
  )
where

import Options.Applicative

data ParserType = ParserType
  { baseSet :: [String],
    sampleSetSize :: Int
  }

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
