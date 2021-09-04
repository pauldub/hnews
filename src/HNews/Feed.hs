module HNews.Feed where

import HNews.Entry (Entry)

class HasFeed a where
  loadTitle :: a -> String
  entries :: a -> [Entry]

data Feed
  = Feed
      { url :: String,
        title :: String,
        tags :: [String]
      }

-- Loads a Feed from a URL
loadFeed :: String -> Feed
loadFeed url =
  Feed
    { url = url,
      title = "unknown",
      tags = []
    }
