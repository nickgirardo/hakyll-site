--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.List ((\\))
import Hakyll

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
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
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Basic html files
    match (fromList ["projects.html"]) $ do
        route idRoute
        compile $
            getResourceBody
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
        `mappend` defaultContext
