module HNews.Feed.GitHubSpec where

import qualified HNews.Feed as Feed
import qualified HNews.Feed.GitHub as GH

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
        "HNews.Feed.GitHubSpec"
        [ testCase "loads GitHub repo title" $ do
                result <- GH.fromRepo "pauldub" "hnews"
                case result of
                        Left _ -> assertFailure "could not load repo"
                        Right repo -> do
                          title <- Feed.loadTitle repo
                          title @?= "hnews"
        , testCase "loads GitHub repo releases" $ do
                result <- GH.fromRepo "pauldub" "hnews"
                case result of
                        Left _ -> assertFailure "could not load repo"
                        Right repo -> do
                          entries <- Feed.entries repo
                          entries @?= []
        ]
