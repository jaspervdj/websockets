websockets
==========

Introduction
------------

Provides a sensible, clean and simple way to write WebSocket-capable servers in
Haskell.

The following program echoes messages back after appending `meow`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad      (forever)
import qualified Data.Text          as T
import qualified Network.WebSockets as WS

meow :: WS.Connection -> IO ()
meow conn = forever $ do
    msg <- WS.receiveData conn
    WS.sendTextData conn $ msg `T.append` ", meow"
```

Installation is provided using cabal:

```
$ cabal install websockets
```

Authors
-------

An initial WebSockets library was written in 2010 by Siniša Biđin. In 2011, it
was rewritten from scratch, and extended to its current state by Jasper Van der
Jeugt, who is also the current maintainer.

Contributors:

- Alex Lang
- Carl Chatfield
- Fedor Gogolev
- Marcin Tolysz
- Nathan Howell
- Steffen Schuldenzucker
- Yi Huang

Development
-----------

Pull requests are always welcome!

This library is production-quality. Therefore we have very high standards in
terms of code style, API quality and testing.

We have three kinds of tests:

- Haskell-based tests (`tests/haskell`), which use the `test-framework` library
- Integration tests, available in `tests/javascript`. These require a browser to
  run.
- We also run the extensive [autobahn testsuite].

[autobahn testsuite]: https://github.com/crossbario/autobahn-testsuite
