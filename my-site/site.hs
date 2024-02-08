--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.List ((\\))
import Hakyll

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match "extern/**" $ do
        route $ customRoute ((\\ "extern/") . toFilePath)
        compile copyFileCompiler

    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList ["contact.markdown", "projects.markdown"]) $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "index.html" $ do
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
