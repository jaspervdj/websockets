# websockets

![Hackage Version](https://img.shields.io/hackage/v/websockets)
![GitHub Workflow Status (with event)](https://img.shields.io/github/actions/workflow/status/jaspervdj/websockets/ci.yml)

Provides a sensible, clean and simple way to write WebSocket
server and client in Haskell.

## Features

- Provides Server/Client implementations of the websocket protocol
- Ping/Pong building blocks for stale connection checking
- TLS support via [wuss](https://hackage.haskell.org/package/wuss) package

## Caveats

- [Doesn't implement client ping/pong](https://github.com/jaspervdj/websockets/issues/159)
- [Send doesn't support streaming](https://github.com/jaspervdj/websockets/issues/119)
- [Requires careful handling of exceptions](https://github.com/jaspervdj/websockets/issues/48)
- [DeflateCompression isn't thread-safe](https://github.com/jaspervdj/websockets/issues/208)

## Introduction

See [server](./example/server.lhs) and [client](./example/client.hs) implementations.

## Installation

Using cabal:

```
$ cabal install websockets
```

## Authors

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
- Domen Kožar

## Development

Pull requests are always welcome!

This library is production-quality. Therefore we have very high standards in
terms of code style, API quality and testing.

We have three kinds of tests:

- Haskell-based tests (`tests/haskell`), which use the `test-framework` library
- Integration tests, available in `tests/javascript`. These require a browser to
  run.
- We also run the extensive [autobahn testsuite].

[autobahn testsuite]: https://github.com/crossbario/autobahn-testsuite
