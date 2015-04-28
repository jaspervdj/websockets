{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
module Network.WebSockets.Routing.Tests
    ( tests
    ) where

--------------------------------------------------------------------------------
import           Control.Concurrent             (forkIO, killThread,
                                                 threadDelay)
import           Control.Exception              (SomeException, handle)
import           Control.Monad                  (msum)
import           Data.Text                      (Text, pack)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert)

--------------------------------------------------------------------------------
import           Network.WebSockets
import           Network.WebSockets.Routing


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Routing.Tests"
    [ testCase "dir/dirs routes"    testDirRoutes
    , testCase "trailing routes"    testTrailingRoutes
    , testCase "path routes"        testPathRoutes
    , testCase "subprotocol routes" testSubprotocolRoutes
    , testCase "query routes"       testQueryRoutes
    ]


--------------------------------------------------------------------------------
testDirRoutes :: Assertion
testDirRoutes = withServer port server $ do

    assert =<< runClient "127.0.0.1" port "/echo"        echoOnce
    assert =<< runClient "127.0.0.1" port "/echo/twice"  echoTwice
    assert =<< runClient "127.0.0.1" port "/echo/twice2" echoTwice

  where
    port = 42950
    text = "echo" :: Text

    server = routeWebSockets $ msum

        -- test 'dirs'
        [ dirs "echo/twice" $ routeAccept $ \con -> do
            msg <- receive con
            send con msg
            send con msg

        -- test (nested) 'dir' and 'nullDir'
        , dir "echo" $ msum
            [ do
                nullDir
                routeAccept $ \con -> do
                msg <- receive con
                send con msg
            , dir "twice2" $ routeAccept $ \con -> do
                msg <- receive con
                send con msg
                send con msg
            ]
        ]

    echoOnce con = do
        sendTextData con text
        msg <- receiveData con
        return $ msg == text

    echoTwice con = do
        sendTextData con text
        msg1 <- receiveData con
        msg2 <- receiveData con
        return $ msg1 == text && msg2 == text


--------------------------------------------------------------------------------
testTrailingRoutes :: Assertion
testTrailingRoutes = withServer port server $ do

    assert =<< runClient "127.0.0.1" port "/trailing/"  expectTrailing
    assert =<< runClient "127.0.0.1" port "/trailing"   expectNoTrailing

  where
    port = 42951

    server = routeWebSockets $ dir "trailing" $ msum

        -- test 'trailingSlash'
        [ do trailingSlash
             routeAccept $ sendTextData `flip` ("Trailing" :: Text)

        -- test 'noTrailingSlash'
        , do noTrailingSlash
             routeAccept $ sendTextData `flip` ("No trailing" :: Text)
        ]

    expectTrailing con = do
        msg <- receiveData con
        return $ msg == ("Trailing" :: Text)

    expectNoTrailing con = do
        msg <- receiveData con
        return $ msg == ("No trailing" :: Text)


--------------------------------------------------------------------------------
testPathRoutes :: Assertion
testPathRoutes = withServer port server $ do

    assert =<< runClient "127.0.0.1" port "/path/foo"   (expect "foo")
    assert =<< runClient "127.0.0.1" port "/path/bar"   (expect "bar")
    assert =<< runClient "127.0.0.1" port "/path/"      (expect "none")

  where
    port = 42952

    server = routeWebSockets $ dir "path" $ msum
        [ path $ \p -> do
            routeAccept $ sendTextData `flip` (pack p)
        , routeAccept $ sendTextData `flip` ("none" :: Text)
        ]

    expect expected con = do
        msg <- receiveData con
        return $ (msg :: Text) == expected


--------------------------------------------------------------------------------
testSubprotocolRoutes :: Assertion
testSubprotocolRoutes = withServer port server $ do

    assert =<< runClientWith "127.0.0.1" port "/" opts (proto "foo") (expect "foo")
    assert =<< runClientWith "127.0.0.1" port "/" opts (proto "bar") (expect "bar")
    assert =<< runClient     "127.0.0.1" port "/"                    (expect "none")

  where
    port = 42953
    opts = defaultConnectionOptions

    -- Header with subprotocol definition
    proto p = [("Sec-WebSocket-Protocol", p)]

    server = routeWebSockets $ msum
        [ subprotocol "foo" $ routeAccept $ sendTextData `flip` ("foo" :: Text)
        , subprotocol "bar" $ routeAccept $ sendTextData `flip` ("bar" :: Text)
        , do
            noSubprotocols
            routeAccept $ sendTextData `flip` ("none" :: Text)
        ]

    expect expected con = do
        msg <- receiveData con
        return $ (msg :: Text) == expected

--------------------------------------------------------------------------------
testQueryRoutes :: Assertion
testQueryRoutes = withServer port server $ do

    assert =<< runClient "127.0.0.1" port "/foo?q=a"            (expect "foo")
    assert =<< runClient "127.0.0.1" port "/foo/bar?q=a"        (expect "bar")
    assert =<< runClient "127.0.0.1" port "/foo/bar2?q=a"       (expect "bar2")
    assert =<< runClient "127.0.0.1" port "/path/foo?q=a"       (expect "foo")
    assert =<< runClient "127.0.0.1" port "/path/?q=a"          (expect "none")
    assert =<< runClient "127.0.0.1" port "/trailing/?q=a"      (expect "trailing")
    assert =<< runClient "127.0.0.1" port "/trailing?q=a"       (expect "no trailing")
    assert =<< runClient "127.0.0.1" port "/full?path=reply"    (expect "/full?path=reply")
    assert =<< runClient "127.0.0.1" port "/full/2?path=reply"  (expect "/2?path=reply")

  where
    port = 42954

    reply t = routeAccept $ sendTextData `flip` (t :: Text)
    expect expected con = do
        msg <- receiveData con
        return $ (msg :: Text) == expected

    server = routeWebSockets $ msum

        -- dir / nullDir test
        [ dir "foo" $ msum
            [ nullDir >> reply "foo"
            , dir "bar" $ reply "bar"
            ]

        -- dirs test
        , dirs "foo/bar2" $ reply "bar2"

        -- path test
        , dir "path" $ msum
            [ path $ reply . pack
            , reply "none"
            ]

        -- trailing / test
        , dir "trailing" $ msum
            [ trailingSlash   >> reply "trailing"
            , noTrailingSlash >> reply "no trailing"
            ]

        -- full path test, with and without previous 'dir'
        , dir "full" $ notNullDir >> do
            p <- askPending
            routeAccept $ sendTextData `flip` (requestPath $ pendingRequest p)
        , do
            p <- askPending
            routeAccept $ sendTextData `flip` (requestPath $ pendingRequest p)
        ]


--------------------------------------------------------------------------------
-- | Helper function to run simple server-client interactions
withServer :: Int -> ServerApp -> IO a -> IO a
withServer port server action = do
    serverThread <- forkIO $ retry $ runServer "0.0.0.0" port (\c -> server c)
    waitSome
    result <- action
    waitSome
    killThread serverThread
    return result
  where
    waitSome = threadDelay $ 200 * 1000
    -- see Network.WebSockets.Server.Tests on 'retry'
    retry act = (\(_ :: SomeException) -> waitSome >> act) `handle` act
