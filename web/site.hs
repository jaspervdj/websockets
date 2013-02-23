--------------------------------------------------------------------------------
{-# LANGUAGE Arrows              #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where


--------------------------------------------------------------------------------
import           Control.Exception      (SomeException, handle)
import           Hakyll
import           System.Posix.Directory (changeWorkingDirectory)
import           System.Posix.Files     (createSymbolicLink)
import           System.Process         (rawSystem)


--------------------------------------------------------------------------------
createSymbolicLink' :: FilePath -> FilePath -> IO ()
createSymbolicLink' dst src = handle handler $ createSymbolicLink dst src
  where
    handler :: SomeException -> IO ()
    handler _ = putStrLn $ "Could not link " ++ src ++ " -> " ++ dst ++
        ", perhaps link already exists?"


--------------------------------------------------------------------------------
makeLinks :: IO ()
makeLinks = do
    createSymbolicLink' "../README"                   "README.markdown"
    createSymbolicLink' "../example/server.lhs"       "example.lhs"
    createSymbolicLink' "../dist/doc/html/websockets" "reference"


--------------------------------------------------------------------------------
makeHaddock :: IO ()
makeHaddock = do
    putStrLn "Generating documentation..."
    changeWorkingDirectory ".."
    sh "cabal" ["configure"]
    sh "cabal" ["haddock", "--hyperlink-source"]
    changeWorkingDirectory "web"
  where
    -- Ignore exit code
    sh c as = rawSystem c as >>= \_ -> return ()


--------------------------------------------------------------------------------
pageCompiler :: Compiler (Item String)
pageCompiler =
    pandocCompiler                                               >>=
    loadAndApplyTemplate "templates/default.html" defaultContext >>=
    relativizeUrls


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    preprocess $ do
        makeLinks
        makeHaddock

    match "README.markdown" $ do
        route $ customRoute $ \_ -> "index.html"
        compile pageCompiler

    match "example.lhs" $ do
        route $ setExtension "html"
        compile pageCompiler

    match "reference/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "templates/*" $
        compile templateCompiler
  where
    config = defaultConfiguration
        { deployCommand = "rsync --checksum -ave 'ssh -p 2222' _site/* \
                          \jaspervdj@jaspervdj.be:jaspervdj.be/websockets"
        }
