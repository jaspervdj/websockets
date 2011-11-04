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

Both the `Hybi00` and `Hybi10` protocols are supported by the library.

Protocol  Firefox  Chrome  Opera  Safari
--------- -------- ------- ------ ---------
`Hybi00`  >= 4     >= 6    >= 11  >= 5.0.1
`Hybi10`  >= 7     >= 14

Authors
-------

An initial WebSockets library was written in 2010 by Siniša Biđin. In 2011, it
was rewritten from scratch, and extended to it's current state by Jasper Van der
Jeugt, who is also the current maintainer.

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
