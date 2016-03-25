## irc-dcc - A DCC message parsing and helper library for IRC clients

DCC (Direct Client-to-Client) is an IRC sub-protocol for establishing
and maintaining direct connections to exchange messages and files.

See http://www.irchelp.org/irchelp/rfc/ctcpspec.html for more details.

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
2. Run `stack build`.
