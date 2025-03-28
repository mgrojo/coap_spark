[![Build](https://github.com/mgrojo/coap_spark/actions/workflows/main.yml/badge.svg)](https://github.com/mgrojo/coap_spark/actions/workflows/main.yml)
[![AppImage](https://github.com/mgrojo/coap_spark/actions/workflows/appimage.yml/badge.svg)](https://github.com/mgrojo/coap_spark/actions/workflows/appimage.yml)
[![GNATprove](https://github.com/mgrojo/coap_spark/actions/workflows/prove.yml/badge.svg)](https://github.com/mgrojo/coap_spark/actions/workflows/prove.yml)
[![codecov](https://codecov.io/gh/mgrojo/coap_spark/graph/badge.svg?token=2AEXL06XHU)](https://codecov.io/gh/mgrojo/coap_spark)

# CoAP-SPARK
CoAP-SPARK is a library implementing the Constrained Application Protocol (CoAP)
as defined in
[RFC 7252](https://www.rfc-editor.org/rfc/rfc7252), developed in the SPARK 
language, the formally verified subset of the Ada programming language.

The objective is to build a formally verified working CoAP
implementation, mainly for academic purposes.

## Dependencies
CoAP-SPARK uses:
* Alire as source package manager (independently installed)
* RecordFlux as a tool facilitating the implementation of the verifiable
  protocol parser and the state machine of a session (included as Git submodule)
* WolfSSL as library for implementing the DTLS communications
  (included as Git submodule)
* GNAT 14.2.1 (toolchain managed by Alire)
* GNATProve 14.1.1 (dependency managed by Alire)
* bbt 0.0.6 for testing (installable via Alire)

## How to build

Clone the repository recursing on submodules:
```
git clone --recurse-submodules https://github.com/mgrojo/coap_spark.git
```

Build RecordFlux following instructions on
[`tools/RecordFlux/doc/development_guide/index.rst`](tools/RecordFlux/doc/development_guide/index.rst).

Generate source code from the CoAP model using RecordFlux with:
```
make generate
```

(The source code generated in the previous step might be added in the future
to this repository so the previous two steps are optional)

Install [Alire](https://alire.ada.dev/) and build the library running:
```
alr build
```

For building the client program:
```
cd client ; alr build
```

## How to test
Install `bbt` running:
```
alr install bbt
```

And then run the tests with:
```
cd client/tests ; make
```

See [`client/tests/coap_client_tests.md`](client/tests/coap_client_tests.md)
for the specification of these tests.

## How to prove
The project (library and client) is currently proved up to the silver mode.

The [`proof/`](proof/) directory constains the results of passing GNATProve. You can replay it running:
```
alr gnatprove --replay
```

Or if you want to start afresh,for example, using a different version of
`gnatprove`, simply run:
```
alr gnatprove
```

The project is using `gnatprove` from the Alire community index. You can install it running this:
```
alr install gnatprove
```

The Colibri solver has to be installed separately, for example, from the
[GNAT Community Edition 2021](https://www.adacore.com/download). After installation,
and before running `gnatprove`, you have to put `{GNAT-2021-HOME}/libexec/spark/bin`
in the `PATH`, so `gnatprove` can find the `colibri` executable.

## Status
CoAP-SPARK is a working and verified implementation of CoAP from the client side.

The main objective of CoAP-SPARK is to be the subject of my Master's Thesis, but
I think it can be used in scenarios where this limitations are not an issue:
- There are no retransmissions.
- Block-wise transfers are not implemented. This isn't part of main RFC 7252
  for CoAP, but of the [RFC 7959](https://datatracker.ietf.org/doc/rfc7959/).

There's no server implementation for the moment, but it wouldn't be too difficult
to add, following the example of the client.

Regarding the implemented client is able to
substitute libcoap's coap-client when called from a project like
[ikea-smartlight](https://github.com/slokhorst/ikea-smartlight/).

Bug reports, feedback or suggestions on how to improve the library or to pass
more verification conditions are very welcome.
