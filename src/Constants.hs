module Constants where

import Hakyll

feed :: FeedConfiguration
feed =
  FeedConfiguration
    { feedTitle = "Functors and Systems"
    , feedDescription = siteDescription
    , feedAuthorName = "functor.systems"
    , feedAuthorEmail = "blog@functor.systems"
    , feedRoot = siteRoot
    }

snapshotDir :: String
snapshotDir = "content"

siteName :: String
siteName = "Functors & Systems"

siteRoot :: String
siteRoot = "https://blog.functor.systems"

siteDescription :: String
siteDescription = "A math and technology blog from functor.systems."
