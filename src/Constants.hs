module Constants where

import Hakyll

feed :: FeedConfiguration
feed =
  FeedConfiguration
    { feedTitle = siteName
    , feedDescription = siteDescription
    , feedAuthorName = "Site Author"
    , feedAuthorEmail = "author@example.com"
    , feedRoot = siteRoot
    }

snapshotDir :: String
snapshotDir = "content"

siteName :: String
siteName = "My Site"

siteRoot :: String
siteRoot = "https://example.com"

siteDescription :: String
siteDescription = "A static site built with Hakyll and Typst"
