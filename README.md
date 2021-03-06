## irc-dcc - A DCC message parsing and helper library for IRC clients

[![Build Status](https://travis-ci.org/JanGe/irc-dcc.svg?branch=master)](https://travis-ci.org/JanGe/irc-dcc)
[![irc-dcc on Hackage](https://img.shields.io/hackage/v/irc-dcc.svg?maxAge=2592000)](https://hackage.haskell.org/package/irc-dcc)
[![irc-dcc on Stackage Nightly](http://stackage.org/package/irc-dcc/badge/nightly)](http://stackage.org/nightly/package/irc-dcc)

DCC (Direct Client-to-Client) is an IRC sub-protocol for establishing
and maintaining direct connections to exchange messages and files.

See http://www.irchelp.org/irchelp/rfc/ctcpspec.html for more details.

### Supported Variants:

* (Standard) DCC
* Reverse DCC

## Usage

1. Add `irc-dcc` to the `build-depends` section of your `.cabal` file.
1. Import `Network.IRC.DCC`.
1.  
    * Parse a CTCP message using `runParser` and any of the `parse*`
      methods exported by this module. If the CTCP message was the
      desired DCC command, the function will return a `Right` value.
    * Encode a DCC command by using `encodeCtcp` function of its
      `CtcpCommand` instance.

See [this package's documentation](https://hackage.haskell.org/package/irc-dcc)
on Hackage.

## Development

1. Install [Stack](http://docs.haskellstack.org/en/stable/README/).
1. Run `stack build`.
