--------------------------------------------------------------------------------
{-# LANGUAGE Arrows              #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where


--------------------------------------------------------------------------------
import           Hakyll
import           System.FilePath   (joinPath, splitFileName, splitPath, (</>))


--------------------------------------------------------------------------------
pageCompiler :: Compiler (Item String)
pageCompiler =
    pandocCompiler                                                   >>=
    loadAndApplyTemplate "web/templates/default.html" defaultContext >>=
    relativizeUrls


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "README.md" $ do
        route $ constRoute "index.html"
        compile $ pageCompiler >>= relativizeUrls

    match "example/server.lhs" $ do
        route $ setExtension ".html"
        compile $ pageCompiler >>= relativizeUrls

    match (fromList
            [ "example/client.html"
            , "example/client.js"
            , "example/screen.css"
            ]) $ do
        route $ idRoute
        compile copyFileCompiler

    match "web/css/*" $ do
        route dropWebRoute
        compile compressCssCompiler

    match "web/templates/*" $
        compile templateCompiler
  where
    config = defaultConfiguration
        { deployCommand = "rsync --checksum -ave 'ssh -p 2222' _site/* \
                          \jaspervdj@jaspervdj.be:jaspervdj.be/websockets"
        }

-- | Drop the `web/` part from a route.
dropWebRoute :: Routes
dropWebRoute = customRoute $ \ident ->
    let path0 = toFilePath ident in
    case splitPath path0 of
        "web/" : path1 -> joinPath path1
        _              -> path0
