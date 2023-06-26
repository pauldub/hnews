module HNews.Feed.GitHub where

import HNews.Feed

import Data.Text (Text)
import GitHub (github')
import qualified GitHub
import GitHub.Internal.Prelude (unpack, toList)

instance HasFeed GitHub.Repo where
  type FeedEntry GitHub.Repo = GitHub.Release

  loadTitle (GitHub.Repo { .. }) =
    pure $ GitHub.untagName repoName

  entries (GitHub.Repo { .. }) = do
    let ownerName = GitHub.simpleOwnerLogin repoOwner
    result <- github' GitHub.releasesR ownerName repoName 10
    case result of
      Left _ -> pure []
      Right releases -> pure $ toList releases

fromRepo :: Text -> Text -> IO (Either GitHub.Error GitHub.Repo)
fromRepo ownerName repoName =
  let ownerName' = GitHub.mkOwnerName ownerName
      repoName' = GitHub.mkRepoName repoName
  in do github' GitHub.repositoryR ownerName' repoName'
