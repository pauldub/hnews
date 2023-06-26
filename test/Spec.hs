module Main (main) where

import HNews.Entry (Entry (..))
import qualified HNews.Entry as Entry
import qualified HNews.Feed as Feed
import qualified HNews.Feed.RSS as RSS
import qualified HNews.Feed.GitHubSpec as GitHubSpec
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [rssFeed, GitHubSpec.tests]

rssFeed :: TestTree
rssFeed =
  testGroup
    "Feed.RSS"
    [ testCase "parses atom feeds" $
        case RSS.parseFeedString sampleAtomFeed of
          Just rss -> do
            title <- Feed.loadTitle rss
            title @?= "Example Feed"

            entries <- Feed.entries rss
            entries @?= [ Entry
                      { url = Just "http://example.org/2003/12/13/atom03",
                        title = "Atom-Powered Robots Run Amok",
                        timestamp = Just "2003-12-13T18:30:02Z"
                      }
                  ]
            pure ()
          _ -> assertFailure "could not parse feed",
      testCase "parses RSS 2.0 feeds" $
        case RSS.parseFeedString sampleRSSFeed of
          Just rss -> do
            title <- Feed.loadTitle rss
            title @?= "Sample Feed - Favorite RSS Related Software & Resources"

            entries <- Feed.entries rss
            entries @?= [ Entry
                      { url = Just "http://www.feedforall.com",
                        title = "RSS Resources",
                        timestamp = Just "Tue, 26 Oct 2004 14:01:01 -0500"
                      },
                    Entry
                      { url = Just "http://www.feedforall.com/feedforall-partners.htm",
                        title = "Recommended Desktop Feed Reader Software",
                        timestamp = Just "Tue, 26 Oct 2004 14:03:25 -0500"
                      },
                    Entry
                      { url = Just "http://www.feedforall.com/feedforall-partners.htm",
                        title = "Recommended Web Based Feed Reader Software",
                        timestamp = Just "Tue, 26 Oct 2004 14:06:44 -0500"
                      }
                  ]
            pure ()
          _ -> assertFailure "could not parse feed",
      testCase "parses RSS 1.0 feeds" $
        case RSS.parseFeedString sampleRSS1Feed of
          Just rss -> do
            title <- Feed.loadTitle rss
            title @?= "XML.com"
            entries <- Feed.entries rss
            entries @?= [ Entry
                      { url = Just "http://xml.com/pub/2000/08/09/xslt/xslt.html",
                        title = "Processing Inclusions with XSLT",
                        timestamp = Nothing
                      },
                    Entry
                      { url = Just "http://xml.com/pub/2000/08/09/rdfdb/index.html",
                        title = "Putting RDF to Work",
                        timestamp = Nothing
                      }
                  ]
            pure ()
          _ -> assertFailure "could not parse feed"
    ]

sampleAtomFeed :: String
sampleAtomFeed =
  "<?xml version=\"1.0\" encoding=\"utf-8\"?> \n\
  \ <feed xmlns=\"http://www.w3.org/2005/Atom\"> \n\
  \\n\
  \ <title>Example Feed</title> \n\
  \ <link href=\"http://example.org/\"/> \n\
  \ <updated>2003-12-13T18:30:02Z</updated> \n\
  \ <author> \n\
  \   <name>John Doe</name> \n\
  \ </author> \n\
  \ <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id> \n\
  \\n\
  \ <entry> \n\
  \   <title>Atom-Powered Robots Run Amok</title> \n\
  \   <link href=\"http://example.org/2003/12/13/atom03\"/> \n\
  \   <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id> \n\
  \   <updated>2003-12-13T18:30:02Z</updated> \n\
  \   <summary>Some text.</summary> \n\
  \ </entry> \n\
  \\n\
  \</feed>"

sampleRSSFeed =
  "<?xml version=\"1.0\" encoding=\"windows-1252\"?> \n\
  \ <rss version=\"2.0\"> \n\
  \   <channel> \n\
  \     <title>Sample Feed - Favorite RSS Related Software &amp; Resources</title> \n\
  \     <description>Take a look at some of FeedForAll&apos;s favorite software and resources for learning more about RSS.</description> \n\
  \     <link>http://www.feedforall.com</link> \n\
  \     <category domain=\"www.dmoz.com\">Computers/Software/Internet/Site Management/Content Management</category> \n\
  \     <copyright>Copyright 2004 NotePage, Inc.</copyright> \n\
  \     <docs>http://blogs.law.harvard.edu/tech/rss</docs> \n\
  \     <language>en-us</language> \n\
  \     <lastBuildDate>Mon, 1 Nov 2004 13:17:17 -0500</lastBuildDate> \n\
  \     <managingEditor>marketing@feedforall.com</managingEditor> \n\
  \     <pubDate>Tue, 26 Oct 2004 14:06:44 -0500</pubDate> \n\
  \     <webMaster>webmaster@feedforall.com</webMaster> \n\
  \     <generator>FeedForAll Beta1 (0.0.1.8)</generator> \n\
  \     <image> \n\
  \       <url>http://www.feedforall.com/feedforall-temp.gif</url> \n\
  \       <title>FeedForAll Sample Feed</title> \n\
  \       <link>http://www.feedforall.com/industry-solutions.htm</link> \n\
  \       <description>FeedForAll Sample Feed</description> \n\
  \       <width>144</width> \n\
  \       <height>117</height> \n\
  \     </image> \n\
  \     <item> \n\
  \       <title>RSS Resources</title> \n\
  \       <description>Be sure to take a look at some of our favorite RSS Resources&lt;br&gt; \n\
  \ &lt;a href=&quot;http://www.rss-specifications.com&quot;&gt;RSS Specifications&lt;/a&gt;&lt;br&gt; \n\
  \ &lt;a href=&quot;http://www.blog-connection.com&quot;&gt;Blog Connection&lt;/a&gt;&lt;br&gt; \n\
  \ &lt;br&gt;</description> \n\
  \       <link>http://www.feedforall.com</link> \n\
  \       <pubDate>Tue, 26 Oct 2004 14:01:01 -0500</pubDate> \n\
  \     </item> \n\
  \     <item> \n\
  \       <title>Recommended Desktop Feed Reader Software</title> \n\
  \       <description>&lt;b&gt;FeedDemon&lt;/b&gt; enables you to quickly read and gather information from hundreds of web sites - without having to visit them. Don&apos;t waste any more time checking your favorite web sites for updates. Instead, use FeedDemon and make them come to you. &lt;br&gt; \n\
  \ More &lt;a href=&quot;http://store.esellerate.net/a.asp?c=1_SKU5139890208_AFL403073819&quot;&gt;FeedDemon Information&lt;/a&gt;</description> \n\
  \       <link>http://www.feedforall.com/feedforall-partners.htm</link> \n\
  \       <pubDate>Tue, 26 Oct 2004 14:03:25 -0500</pubDate> \n\
  \     </item> \n\
  \     <item> \n\
  \       <title>Recommended Web Based Feed Reader Software</title> \n\
  \       <description>&lt;b&gt;FeedScout&lt;/b&gt; enables you to view RSS/ATOM/RDF feeds from different sites directly in Internet Explorer. You can even set your Home Page to show favorite feeds. Feed Scout is a plug-in for Internet Explorer, so you won&apos;t have to learn anything except for how to press 2 new buttons on Internet Explorer toolbar. &lt;br&gt; \n\
  \ More &lt;a href=&quot;http://www.bytescout.com/feedscout.html&quot;&gt;Information on FeedScout&lt;/a&gt;&lt;br&gt; \n\
  \ &lt;br&gt; \n\
  \ &lt;br&gt; \n\
  \ &lt;b&gt;SurfPack&lt;/b&gt; can feature search tools, horoscopes, current weather conditions, LiveJournal diaries, humor, web modules and other dynamically updated content. &lt;br&gt; \n\
  \ More &lt;a href=&quot;http://www.surfpack.com/&quot;&gt;Information on SurfPack&lt;/a&gt;&lt;br&gt;</description> \n\
  \       <link>http://www.feedforall.com/feedforall-partners.htm</link> \n\
  \       <pubDate>Tue, 26 Oct 2004 14:06:44 -0500</pubDate> \n\
  \     </item> \n\
  \   </channel> \n\
  \ </rss>"

sampleRSS1Feed =
  "<?xml version=\"1.0\"?>\n\
  \\n\
  \<rdf:RDF \n\
  \  xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n\
  \  xmlns=\"http://purl.org/rss/1.0/\"\n\
  \>\n\
  \\n\
  \  <channel rdf:about=\"http://www.xml.com/xml/news.rss\">\n\
  \    <title>XML.com</title>\n\
  \\n\
  \    <link>http://xml.com/pub</link>\n\
  \    <description>\n\
  \      XML.com features a rich mix of information and services \n\
  \      for the XML community.\n\
  \    </description>\n\
  \\n\
  \    <image rdf:resource=\"http://xml.com/universal/images/xml_tiny.gif\" />\n\
  \\n\
  \    <items>\n\
  \      <rdf:Seq>\n\
  \        <rdf:li resource=\"http://xml.com/pub/2000/08/09/xslt/xslt.html\" />\n\
  \        <rdf:li resource=\"http://xml.com/pub/2000/08/09/rdfdb/index.html\" />\n\
  \      </rdf:Seq>\n\
  \    </items>\n\
  \\n\
  \  </channel>\n\
  \    <image rdf:about=\"http://xml.com/universal/images/xml_tiny.gif\">\n\
  \    <title>XML.com</title>\n\
  \    <link>http://www.xml.com</link>\n\
  \\n\
  \    <url>http://xml.com/universal/images/xml_tiny.gif</url>\n\
  \  </image>\n\
  \    <item rdf:about=\"http://xml.com/pub/2000/08/09/xslt/xslt.html\">\n\
  \    <title>Processing Inclusions with XSLT</title>\n\
  \    <link>http://xml.com/pub/2000/08/09/xslt/xslt.html</link>\n\
  \\n\
  \    <description>\n\
  \     Processing document inclusions with general XML tools can be \n\
  \     problematic. This article proposes a way of preserving inclusion \n\
  \     information through SAX-based processing.\n\
  \    </description>\n\
  \  </item>\n\
  \    <item rdf:about=\"http://xml.com/pub/2000/08/09/rdfdb/index.html\">\n\
  \    <title>Putting RDF to Work</title>\n\
  \\n\
  \    <link>http://xml.com/pub/2000/08/09/rdfdb/index.html</link>\n\
  \    <description>\n\
  \     Tool and API support for the Resource Description Framework \n\
  \     is slowly coming of age. Edd Dumbill takes a look at RDFDB, \n\
  \     one of the most exciting new RDF toolkits.\n\
  \    </description>\n\
  \  </item>\n\
  \\n\
  \</rdf:RDF>"
