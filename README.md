# pop3s-client
A Haskell POP3 over TLS client, similar to pop3-client

The existing haskell pop3-client does not do SSL, which is necessary.

The current implementation is bare bones.

There are 3 areas where the implementation may differ from pop3-client:

  - support SSL, or enforce SSL/TLS
  - use ByteStrings for messages
  - make use of some dependent haskell features

time will tell.
