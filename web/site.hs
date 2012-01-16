{-# LANGUAGE Arrows, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Control.Arrow ((>>>))
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Files (createSymbolicLink)
import System.Process (rawSystem)

import Hakyll

createSymbolicLink' :: FilePath -> FilePath -> IO ()
createSymbolicLink' dst src = createSymbolicLink dst src `catch` \_ ->
    putStrLn $ "Could not link " ++ src ++ " -> " ++ dst ++
        ", perhaps link already exists?"

makeLinks :: IO ()
makeLinks = do
    createSymbolicLink' "../README"                   "README.markdown"
    createSymbolicLink' "../example/server.lhs"       "example.lhs"
    createSymbolicLink' "../dist/doc/html/websockets" "reference"

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

pageCompiler' :: Compiler Resource (Page String)
pageCompiler' =
    pageCompiler >>>
    applyTemplateCompiler "templates/default.html" >>>
    relativizeUrlsCompiler

main :: IO ()
main = do
    makeLinks
    hakyllWith config $ do
        match "README.markdown" $ do
            route $ customRoute $ \_ -> "index.html"
            compile pageCompiler'
        match "example.lhs" $ do
            route $ setExtension "html"
            compile pageCompiler'
        match "reference/*" $ do
            route idRoute
            compile $ proc x -> do
                () <- require "haddock" (\() () -> ()) -< ()
                copyFileCompiler -< x

        -- | A virtual target which generates the documentation
        create "haddock" $ unsafeCompiler $ const makeHaddock

        match "templates/*" $ compile templateCompiler
        match "css/*" $ do
            route idRoute
            compile compressCssCompiler
  where
    config = defaultHakyllConfiguration
        { deployCommand = "rsync --checksum -ave 'ssh -p 2222' _site/* \
                          \jaspervdj@jaspervdj.be:jaspervdj.be/websockets"
        }
