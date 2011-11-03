websockets
==========

Introduction
------------

Provides a sensible, clean and simple way to write WebSocket-capable servers in
Haskell.

The following program echoes messages back after appending `meow`:

    meow :: TextProtocol p => WebSockets p ()
    meow = forever $ do
        msg <- receiveData
        sendTextData $ msg `T.append` ", meow."

Installation is provided using cabal:

    cabal install websockets

Compatibility
-------------

Protocol  Firefox  Chrome  Opera  Safari
--------- -------- ------- ------ ---------
`Hybi00`  >= 4     >= 6    >= 11  >= 5.0.1
`Hybi10`  >= 7     >= 14

Authors
-------

The library is written and maintained Jasper Van der Jeugt.

Contributors:

- Alex Lang
- Steffen Schuldenzucker

Development
-----------

Pull requests are always welcome!

This library is production-quality. Therefore we have very high standards in
terms of code style, API quality and testing.

We have two kinds of tests: Haskell-based tests (`tests/haskell`), which use the
`test-framework` library. Additionally, there are integration tests available
in `tests/javascript`, which require a browser to run.
