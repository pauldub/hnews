module HNews.Entry where

import qualified GitHub

data Entry
  = Entry
      { url :: Maybe String,
        title :: String,
        timestamp :: Maybe String
      }

  deriving (Show, Eq)
