{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates where

import BlazeSupport
import Constants
import Control.Monad (forM_, unless, when)
import Data.ByteString.Lazy
import Data.Maybe
import Hakyll
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Utils

data PageMetadata = PageMetadata
  { title :: Maybe String
  , pagetitle :: Maybe String
  , slug :: Maybe String
  , description :: Maybe String
  , thumbnail :: Maybe String
  , url :: Maybe String
  }

pageHead ::
  PageMetadata ->
  Html
pageHead (PageMetadata{title, pagetitle, slug, description, thumbnail, url}) = do
  let description' =
        toValue $
          fromMaybe
            siteDescription
            description
  let thumbnail' =
        toValue $
          fromMaybe
            "/favicon.svg"
            thumbnail
  let title' = case pagetitle of
        Just pagetitle' -> pagetitle'
        Nothing -> case slug of
          Just slug' -> slug'
          Nothing -> fromMaybe "Functors & Systems" title
  let url' = toValue $ fromMaybe siteRoot url
  H.head $
    do
      meta ! charset "utf-8"
      meta ! httpEquiv "x-ua-compatible" ! content "ie=edge"
      meta ! name "viewport" ! content "width=device-width, initial-scale=1"
      H.title $ toHtml title'
      meta ! name "og:title" ! (content . toValue) title'
      meta ! name "og:site_name" ! content (toValue siteName)
      meta
        ! name "og:description"
        ! content description'
      meta
        ! name "description"
        ! content description'
      meta ! name "og:image" ! content thumbnail'
      meta ! name "og:type" ! content "website"
      meta ! name "og:url" ! content url'
      meta ! name "robots" ! content "index, follow"
      link ! rel "stylesheet" ! href "/css/main.css"
      link ! rel "icon" ! type_ "image/png" ! href "/favicon-96x96.png" ! sizes "96x96"
      link ! rel "icon" ! type_ "image/svg+xml" ! href "/favicon.svg"
      link ! rel "apple-touch-icon" ! sizes "180x180" ! href "/apple-touch-icon.png"

siteHeader :: Html
siteHeader =
  header ! class_ "site-header mb-8 pb-4" $ do
    H.div ! class_ "flex items-center justify-between" $ do
      a ! href "/" ! class_ "wordmark" $ "Functors & Systems"
      nav ! class_ "flex gap-6" $ do
        a ! href "/archive" ! class_ "nav-link" $ "Archive"
        a ! href "/about" ! class_ "nav-link" $ "About"
        a ! href "/atom.xml" ! class_ "nav-link" $ "RSS"

pageFooter :: String -> String -> String -> Html
pageFooter commit ghc time =
  footer ! class_ "site-footer mt-12 pt-6 pb-8" $ do
    H.div ! class_ "flex flex-col sm:flex-row sm:justify-between gap-2" $ do
      H.div $ do
        a ! href "/" $ toHtml siteName
        " · "
        a ! href "https://github.com/youwen5/functor-systems-blog" $ "Source"
      H.div $ do
        "Built with "
        a ! href "https://jaspervdj.be/hakyll/" $ "Hakyll"
        " · GHC "
        toHtml ghc

defaultTemplate :: Context String -> Item String -> Compiler Html
defaultTemplate ctx item =
  do
    title <- getField' "title"
    author <- getField' "author"
    date <- getField' "date"
    pagetitle <- getField' "pagetitle"
    location <- getField' "location"
    slug <- getField' "slug"
    enableComments' <- getField' "enable-comments"
    ghc <- getField' "ghc-version"
    time <- getField' "last-commit-timestamp"
    commitHash <- getField' "commit-hash"
    url <- getField' "url"
    thumbnail <- getField' "thumbnail"
    description <- getField' "description"
    let ghc' = fromMaybe "GHC_VER_PLACEHOLDER" ghc
    let commitHash' = fromMaybe "COMMIT_HASH_PLACEHOLDER" commitHash
    let time' = fromMaybe "TIME_PLACEHOLDER" time
    return $ docTypeHtml ! lang "en" $ do
      pageHead PageMetadata{title, pagetitle, slug, thumbnail, description, url}
      body
        ! class_ "mx-auto max-w-2xl px-6 py-8"
        $ do
          siteHeader
          main $ do
            article $ do
              header ! class_ "mb-8" $ do
                h1 ! class_ "article-title" $ forM_ title toHtml
                H.div ! class_ "article-meta mt-2" $ do
                  forM_ date $ \date' -> H.span $ toHtml date'
                  forM_ author $ \author' -> do
                    " · "
                    H.span $ toHtml author'
              H.div ! class_ "prose" $ preEscapedToHtml (itemBody item)
          pageFooter commitHash' ghc' time'
 where
  getField' = getStringField ctx item

postListItem :: Context t -> Item t -> Compiler Html
postListItem ctx item = do
  title <- getField' "title"
  description <- getField' "description"
  url <- getField' "url"
  date <- getField' "date"
  pure
    $ a
      ! href (toValue $ fromMaybe "#" url)
      ! class_ "post-item"
    $ do
      H.div ! class_ "flex justify-between items-baseline gap-4" $ do
        H.span ! class_ "post-title" $ toHtml $ fromMaybe "Untitled" title
        forM_ date $ \d -> H.span ! class_ "post-date" $ toHtml d
      forM_ description $ \desc ->
        p ! class_ "post-description" $ toHtml desc
 where
  getField' = getStringField ctx item

archivePage :: Context String -> Item String -> Compiler Html
archivePage ctx item = do
  let getList' = getList ctx item
  ListData innerCtx posts <- getList' "posts"
  sortedPosts <- recentFirst posts
  postsRendered <- mapM (postListItem innerCtx) sortedPosts
  pure $ do
    H.div ! class_ "mb-6" $ do
      p ! class_ "text-muted" $ do
        "All posts, sorted by date. Subscribe via "
        a ! href "/feed.xml" $ "RSS"
        " or "
        a ! href "/atom.xml" $ "Atom"
        "."
    H.div ! class_ "flex flex-col" $ mconcat postsRendered

indexPage :: Context String -> Item String -> Compiler Html
indexPage ctx item = do
  let getList' = getList ctx item
  ListData innerCtx posts <- getList' "posts"
  sortedPosts <- Prelude.take 5 <$> recentFirst posts
  postsRendered <- mapM (postListItem innerCtx) sortedPosts
  pure $ do
    H.div ! class_ "mb-10" $ do
      p ! class_ "text-lg mb-4" $ do
        "A math and technology blog from "
        a ! href "https://functor.systems/" ! target "_blank" $ "functor.systems"
        ". Writing contributed by members as well as news about our organization."
      p ! class_ "text-muted" $ do
        "Read more on the "
        a ! href "/about" $ "about page"
        "."
    unless (Prelude.null sortedPosts) $ H.div $ do
      h2 ! class_ "section-title" $ "Recent Posts"
      H.div ! class_ "flex flex-col gap-y-2" $ mconcat postsRendered
      p ! class_ "mt-6" $ a ! href "/archive" $ "View all posts →"
