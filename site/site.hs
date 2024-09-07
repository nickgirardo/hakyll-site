--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.List ((\\))
import Hakyll
import System.Environment (lookupEnv)
import Data.Hash.MD5 (md5s, Str(..))

--------------------------------------------------------------------------------

main :: IO ()
main = do
    lastUpdated <- maybe "" ("last updated " <>) <$> (lookupEnv "LAST_UPDATED")
    hakyll $ siteRules $ (defaultContext <> lastUpdatedCtx lastUpdated)

stylesheets :: [String]
stylesheets = ["css/default.css"]

siteRules :: Context String -> Rules ()
siteRules ctx = do
    -- CSS Files
    -- These actually shouldn't be used by the frontend of the application at all but are here to ensure
    -- that they are available as dependencies for the stylesheet we build with a hash suffix
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Create a path for our stylesheet by calculating an md5 hash of their concatenation
    stylesheetPath <- preprocess $ do
        styles <- traverse readFile stylesheets
        let h = md5s $ Str $ compressCss $ mconcat styles
        pure $ "css/style-" <> h <> ".css"

    let ctxWithStyles = (constField "cssPath" stylesheetPath) <> ctx

    -- Generate our concatenated and hashed stylesheet with the path calculated above
    create [fromFilePath stylesheetPath] $ do
        route idRoute
        compile $ do
            styles <- traverse (load . fromFilePath) stylesheets
            let styleCtx = listField "styles" ctx (pure styles)
            makeItem (""::String) >>= loadAndApplyTemplate "templates/merge.css" styleCtx

    -- Externally defined dependencies
    -- Remove the "extern/" prefix but otherwise copy as is
    match "extern/**" $ do
        route $ customRoute ((\\ "extern/") . toFilePath)
        compile copyFileCompiler

    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    -- Basic markdown files
    match (fromList ["index.md"]) $ do
        route $ setExtension "html"
        styleDependencies <- makePatternDependency "css/*"
        rulesExtraDependencies [styleDependencies] $ compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" ctxWithStyles
                >>= relativizeUrls

    -- Markdown posts
    match "posts/*.md" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" (postCtx <> ctxWithStyles)
                >>= loadAndApplyTemplate "templates/default.html" (postCtx <> ctxWithStyles)
                >>= relativizeUrls

    -- Blog posts
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let postsCtx = constField "title" "Posts"
                    <> listField "posts" (postCtx <> ctxWithStyles) (pure posts)
                    <> ctxWithStyles
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" postsCtx
                >>= loadAndApplyTemplate "templates/default.html" postsCtx
                >>= relativizeUrls

    -- Basic html files
    match (fromList ["projects.html"]) $ do
        route idRoute
        compile $
            getResourceBody
                >>= loadAndApplyTemplate "templates/default.html" ctxWithStyles
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


lastUpdatedCtx :: String -> Context String
lastUpdatedCtx lastUpdated = constField "lastUpdated" lastUpdated

postCtx :: Context String
postCtx  =
    dateField "date" "%B %e, %Y"