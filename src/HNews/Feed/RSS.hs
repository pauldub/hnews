{-# LANGUAGE NamedFieldPuns #-}

module HNews.Feed.RSS where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import HNews.Entry (Entry (..))
import HNews.Feed (HasFeed, entries, loadTitle)
import qualified Text.Atom.Feed as Atom
import Text.Feed.Import as Import
import Text.Feed.Query (feedItems, getFeedTitle)
import Text.Feed.Types (Feed, Item (..))
import Text.RSS.Syntax (rssItemLink, rssItemPubDate, rssItemTitle)
import qualified Text.RSS.Syntax as RSS
import qualified Text.RSS1.Syntax as RSS1

newtype RSS = RSS Feed

instance HasFeed RSS where
  loadTitle (RSS feed) = Text.unpack $ getFeedTitle feed
  entries (RSS feed) = mapMaybe toEntry (feedItems feed)
    where
      toEntry (AtomItem item) = atomItemToEntry item
      toEntry (RSSItem item) = rssItemToEntry item
      toEntry (RSS1Item item) = rss1ItemToEntry item
      toEntry _ = Nothing

parseFeedString :: String -> Maybe RSS
parseFeedString content = do
  feed <- Import.parseFeedString content
  pure $ RSS feed

atomItemToEntry :: Atom.Entry -> Maybe Entry
atomItemToEntry Atom.Entry {Atom.entryLinks, Atom.entryTitle, Atom.entryUpdated} =
  let url = Text.unpack $ atomFirstLink entryLinks
      title = Atom.txtToString entryTitle
      timestamp = Text.unpack entryUpdated
   in Just $ Entry url title timestamp

atomFirstLink :: [Atom.Link] -> Text
atomFirstLink (link : xs) = Atom.linkHref link
atomFirstLink [] = "unknown"

rssItemToEntry :: RSS.RSSItem -> Maybe Entry
rssItemToEntry RSS.RSSItem {rssItemLink, rssItemTitle, rssItemPubDate} =
  let url = maybe "" Text.unpack rssItemLink
      title = maybe "" Text.unpack rssItemTitle
      timestamp = maybe "" Text.unpack rssItemPubDate
   in Just $ Entry url title timestamp

rss1ItemToEntry :: RSS1.Item -> Maybe Entry
rss1ItemToEntry RSS1.Item {RSS1.itemURI, RSS1.itemTitle} =
  let url = Text.unpack itemURI
      title = Text.unpack itemTitle
   in Just $ Entry url title ""
