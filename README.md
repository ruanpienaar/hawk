## Hawk
[![BuildlsStatus](https://travis-ci.org/ruanpienaar/hawk.svg?branch=master)](https://travis-ci.org/ruanpienaar/hawk)
[![Coverage Status](https://coveralls.io/repos/github/ruanpienaar/hawk/badge.svg?branch=master)](https://coveralls.io/github/ruanpienaar/hawk?branch=master)

A simple library with the eyesight of a hawk.
The current implementation connects to a specified node,
triggers your own callback once connected, monitors the node, and if it should
happen to disconnect, then call the disconnect callback, and attempt to reconnect periodically.

Hawk will Only work with OTP Erlang versions with map support.