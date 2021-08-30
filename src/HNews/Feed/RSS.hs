{-# LANGUAGE NamedFieldPuns #-}

module HNews.Feed.RSS where

import qualified Data.Text as Text
import HNews.Entry (Entry (..))
import HNews.Feed (HasFeed, entries, loadTitle)
import qualified Text.Atom.Feed as Atom
import Text.Feed.Query (feedItems, getFeedTitle)
import Text.Feed.Types (Feed, Item (..))

newtype RSS = RSS Feed

instance HasFeed RSS where
  loadTitle (RSS feed) = Just $ Text.unpack $ getFeedTitle feed
  entries (RSS feed) = map toEntry (feedItems feed)
    where
      toEntry (AtomItem Atom.Entry {Atom.entryId, Atom.entryTitle, Atom.entryUpdated}) =
        Entry (Text.unpack entryId) (Atom.txtToString entryTitle) (Text.unpack entryUpdated)
      toEntry _ = Entry "unsupported" "unsupported" "unsupported"
