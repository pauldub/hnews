module HNews.Entry where

data Entry
  = Entry
      { url :: Maybe String,
        title :: String,
        timestamp :: Maybe String
      }
  deriving (Show, Eq)
