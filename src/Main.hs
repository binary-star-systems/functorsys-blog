module Main where

import Compilers
import Constants
import Templates
import Utils

import Data.Maybe (fromMaybe)
import Data.UnixTime (UnixTime (..), formatUnixTime, webDateFormat)
import Data.Version (showVersion)
import Hakyll
import System.Environment.Blank (getEnvDefault)
import System.FilePath (
  dropExtension,
  replaceBaseName,
  replaceExtension,
  takeBaseName,
  takeDirectory,
  takeFileName,
  (</>),
 )
import System.Info
import Text.Blaze.Html.Renderer.String
import Text.Pandoc.UTF8 (toString)
import Text.Read (readMaybe)

main :: IO ()
main = generateSite

data EnvData = EnvData {gitHash :: IO String, ghcVersion :: String, lastCommitTimestamp :: IO UnixTime}

envData :: EnvData
envData =
  EnvData
    { gitHash = getEnvDefault "GIT_COMMIT_HASH" "PLACEHOLDER_HASH"
    , ghcVersion = showVersion compilerVersion
    , lastCommitTimestamp = do
        x <- getEnvDefault "LAST_COMMIT_TIMESTAMP" "0"
        pure $ UnixTime (fromMaybe 0 $ readMaybe x) 0
    }

generateSite :: IO ()
generateSite = do
  gitHash' <- gitHash envData
  lastCommitTimestamp' <- lastCommitTimestamp envData
  formattedTime <- formatUnixTime webDateFormat lastCommitTimestamp'
  let extraContext =
        constField "commit-hash" gitHash'
          <> constField "ghc-version" (ghcVersion envData)
          <> constField "last-commit-timestamp" (toString formattedTime)
  let defaultContext =
        extraContext
          <> websiteContext
  let postContext = extraContext <> Utils.postContext
  hakyll $ do
    match "root/404.typ" $ do
      reroute $ takeFileName . flip replaceExtension "html"
      compile $
        typstHtmlCompiler defaultContext
          >>= blazeTemplater Templates.defaultTemplate defaultContext
          >>= universalOptimizer

    create ["index.html"] $ do
      route idRoute
      compile $ do
        posts <- loadAll "posts/**"
        let indexCtx =
              listField "posts" postContext (return posts)
                <> constField "title" "Home"
                <> defaultContext
        makeItem ""
          >>= blazeTemplater Templates.indexPage indexCtx
          >>= blazeTemplater Templates.defaultTemplate indexCtx
          >>= universalOptimizer

    create ["archive.html"] $ do
      reroute expandRoute
      compile $ do
        posts <- loadAll "posts/**"
        let archiveCtx =
              listField "posts" postContext (return posts)
                <> constField "title" "Archive"
                <> defaultContext
        makeItem ""
          >>= blazeTemplater Templates.archivePage archiveCtx
          >>= blazeTemplater Templates.defaultTemplate archiveCtx
          >>= universalOptimizer

    match "root/**.typ" $ do
      reroute toRootHTML

      compile $
        typstHtmlCompiler defaultContext
          >>= blazeTemplater Templates.defaultTemplate defaultContext
          >>= universalOptimizer

    match "posts/**.typ" $ do
      reroute toRootHTML

      compile $
        typstHtmlCompiler postContext
          >>= saveSnapshot snapshotDir
          >>= blazeTemplater Templates.defaultTemplate postContext
          >>= universalOptimizer

    match "static/**" $ do
      sameRoute
      compile copyFileCompiler

    match "fonts/*" $ do
      sameRoute
      compile copyFileCompiler

    match "root/favicon*" $ do
      reroute takeFileName
      compile copyFileCompiler

    match "css/main.css" $ do
      sameRoute
      compile $ makeCompiler tailwindProcessor >>= universalOptimizer

    create ["atom.xml"] $ makeFeed renderAtom
    create ["feed.xml"] $ makeFeed renderRss
    create ["feed.json"] $ makeFeed renderJson
