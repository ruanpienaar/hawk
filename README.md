## Hawk
[![BuildlsStatus](https://travis-ci.org/ruanpienaar/hawk.svg?branch=master)](https://travis-ci.org/ruanpienaar/hawk)
[![Coverage Status](https://coveralls.io/repos/github/ruanpienaar/hawk/badge.svg?branch=master)](https://coveralls.io/github/ruanpienaar/hawk?branch=master)

OTP vsn 21+

A simple library with the eyesight of a hawk.
The current implementation connects to a specified node,
triggers your own callback once connected, monitors the node, and if it should
happen to disconnect, calls the disconnect callback, and attempt to reconnect periodically.

