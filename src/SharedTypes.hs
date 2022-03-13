module SharedTypes
  ( ParserType (..),
  )
where

data ParserType = ParserType
  { baseSet :: [String],
    sampleSetSize :: Int
  }
  deriving (Show)
