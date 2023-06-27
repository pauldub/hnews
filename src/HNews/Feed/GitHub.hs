module HNews.Feed.GitHub where

import HNews.Feed

import Data.Text (Text)
import GitHub (github')
import qualified GitHub
import GitHub.Data.Request ()
import GitHub.Internal.Prelude (unpack, toList)

instance HasFeed GitHub.Repo where
  type FeedEntry GitHub.Repo = GitHub.Release

  loadTitle (GitHub.Repo { .. }) =
    pure $ GitHub.untagName repoName

  entries (GitHub.Repo { .. }) (EntriesParams { limit }) = do
    let ownerName = GitHub.simpleOwnerLogin repoOwner
    result <- github' GitHub.releasesR ownerName repoName $ fromInteger limit
    case result of
      Left _ -> pure []
      Right releases -> pure $ toList releases

fromRepo :: Text -> Text -> IO (Either GitHub.Error GitHub.Repo)
fromRepo ownerName repoName =
  let ownerName' = GitHub.mkOwnerName ownerName
      repoName' = GitHub.mkRepoName repoName
  in do github' GitHub.repositoryR ownerName' repoName'

fromUserStars :: Text -> GitHub.FetchCount -> IO (Either GitHub.Error [GitHub.Repo])
fromUserStars ownerName count =
  let ownerName' = GitHub.mkOwnerName ownerName
  in do
    result <- github' GitHub.reposStarredByR ownerName' count
    case result of
      Left e -> pure $ Left e
      Right repos -> pure $ Right $ toList repos
