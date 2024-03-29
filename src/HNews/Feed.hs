module HNews.Feed where

import Data.Text (Text)
import Control.Monad.IO.Class

data EntriesParams = EntriesParams
  { limit :: Integer }

class HasFeed a where
  type FeedEntry a :: *

  loadTitle :: a -> IO Text
  entries :: a -> EntriesParams -> IO [FeedEntry a]

data Feed
  = Feed
      { url :: Text,
        title :: Text,
        tags :: [Text]
      }

-- Loads a Feed from a URL
loadFeed :: Text -> Feed
loadFeed url =
  Feed
    { url = url,
      title = "unknown",
      tags = []
    }
