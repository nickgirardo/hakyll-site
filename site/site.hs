--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.List ((\\))
import Hakyll
import System.Environment (lookupEnv)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    lastUpdated <- maybe "" ("last updated " <>) <$> (lookupEnv "LAST_UPDATED")
    hakyll $ siteRules $ (defaultContext <> lastUpdatedCtx lastUpdated)

siteRules :: Context String -> Rules ()
siteRules ctx = do
    -- Externally defined dependencies
    -- Remove the "extern/" prefix but otherwise copy as is
    match "extern/**" $ do
        route $ customRoute ((\\ "extern/") . toFilePath)
        compile copyFileCompiler

    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Basic markdown files
    match (fromList ["index.md"]) $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- Markdown posts
    match "posts/*.md" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" (postCtx <> ctx)
                >>= loadAndApplyTemplate "templates/default.html" (postCtx <> ctx)
                >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let postsCtx = constField "title" "Posts"
                    <> listField "posts" (postCtx <> ctx) (pure posts)
                    <> ctx
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" postsCtx
                >>= loadAndApplyTemplate "templates/default.html" postsCtx
                >>= relativizeUrls

    -- Basic html files
    match (fromList ["projects.html"]) $ do
        route idRoute
        compile $
            getResourceBody
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


lastUpdatedCtx :: String -> Context String
lastUpdatedCtx lastUpdated = constField "lastUpdated" lastUpdated

postCtx :: Context String
postCtx  =
    dateField "date" "%B %e, %Y"