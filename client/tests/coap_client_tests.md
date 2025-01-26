## Feature: CoAP Client to coap://coap.me

These are the test scenarios for `coap_client` to be run using the `bbt` tool.
These scenarios require connectivity to coap://coap.me in Internet.

You can install `bbt` with `alr install bbt`.

Use this command to run the tests: `bbt coap_client_tests.md`

### Scenario: version message
  - When I run `../bin/coap_client --version`
  - Then I get no error
  - And output contains `coap_client v0.1.`

### Scenario: help message
  - When I run `../bin/coap_client --help`
  - Then I get no error
  - And output contains `Usage: coap_client`

### Scenario: usage message on unrecognized option
  - When I run `../bin/coap_client --unknown-option`
  - Then I get an error
  - And output contains `Usage: coap_client`

### Scenario: coaps not supported by server
  - When I run `../bin/coap_client -m get coaps://coap.me/`
  - Then I get an error

### Scenario: invalid method
  - When I run `../bin/coap_client -m get http://coap.me/`
  - Then I get an error

### Scenario: get method with test path and high verbosity
  - When I run `../bin/coap_client coap://coap.me/test`
  - Then I get no error
  - And the output contains 
```
Method: GET
Scheme: coap
Host: coap.me
Port: 5683
Path: /test
Query: 
REQUEST: 
Option: URI_HOST
  - Length:  7
  - Value: coap.me
Option: URI_PORT
  - Length:  2
  - Value:  5683
Option: URI_PATH
  - Length:  4
  - Value: test

RESPONSE: 
Server answered with success.
Option: ETAG
  - Length:  8
  - Value:  217 170  27 123 139 211 160 191
Option: CONTENT_FORMAT
  - Length:  0
  - Value: 
Content-Format: text/plain; charset=utf-8
Payload: welcome to the ETSI plugtest! last change: 
```

### Scenario: get method without path
  - When I run `../bin/coap_client coap://coap.me`
  - Then I get no error
  - And output contains `</test>;rt="test";ct=0,</validate>;rt="validate"`

### Scenario: get method with port and no path
  - When I run `../bin/coap_client coap://coap.me:5683`
  - Then I get no error
  - And output contains `</test>;rt="test";ct=0,</validate>;rt="validate"`

### Scenario: explicit get method with path
  - When I run `../bin/coap_client -m get coap://coap.me/test`
  - Then I get no error
  - And output contains `welcome to the ETSI plugtest! last change:`

### Scenario: get method with port and path
  - When I run `../bin/coap_client coap://coap.me:5683/test`
  - Then I get no error
  - And output contains `welcome to the ETSI plugtest! last change:`

### Scenario: get "validate" path
  - When I run `../bin/coap_client coap://coap.me/validate`
  - Then I get no error

### Scenario: get "Type1" path
  - When I run `../bin/coap_client coap://coap.me/Type1`
  - Then I get no error

### Scenario: get "blåbærsyltetøy" path
  - When I run `../bin/coap_client coap://coap.me/blåbærsyltetøy`
  - Then I get no error

### Scenario: get "sink" path
  - When I run `../bin/coap_client coap://coap.me/sink`
  - Then I get no error

### Scenario: get "separate" path
  - When I run `../bin/coap_client coap://coap.me/separate`
  - Then I get no error

### Scenario: get "secret" path
  - When I run `../bin/coap_client coap://coap.me/secret`
  - Then I get no error
  - And the output is
```
4.01 Not authorized
```

### Scenario: get "weird33" path
  - When I run `../bin/coap_client coap://coap.me/weird33`
  - Then I get no error
  - And output does not contain `MALFORMED_MESSAGE`

### Scenario: get "weird44" path
  - When I run `../bin/coap_client coap://coap.me/weird44`
  - Then I get no error
  - And output does not contain `MALFORMED_MESSAGE`

### Scenario: get "weird55" path
  - When I run `../bin/coap_client coap://coap.me/weird55`
  - Then I get no error
  - And output does not contain `MALFORMED_MESSAGE`

### Scenario: get "weird333" path
  - When I run `../bin/coap_client coap://coap.me/weird333`
  - Then I get no error
  - And output does not contain `MALFORMED_MESSAGE`

### Scenario: get "weird3333" path
  - When I run `../bin/coap_client coap://coap.me/weird3333`
  - Then I get no error
  - And output does not contain `MALFORMED_MESSAGE`

### Scenario: get "weird33333" path
  - When I run `../bin/coap_client coap://coap.me/weird33333`
  - Then I get no error
  - And output does not contain `MALFORMED_MESSAGE`

### Scenario: get "123412341234123412341234" path
  - When I run `../bin/coap_client coap://coap.me/123412341234123412341234`
  - Then I get no error
  - And the output contains
```
very long resource name
```

### Scenario: get "create1" path
  - When I run `../bin/coap_client coap://coap.me/create1`
  - Then I get no error

### Scenario: get "query" path
  - When I run `../bin/coap_client coap://coap.me/query`
  - Then I get no error
  - And the output contains
```
You asked me about: Nothing particular.
```

### Scenario: get "seg1" path
  - When I run `../bin/coap_client coap://coap.me/seg1`
  - Then I get no error

### Scenario: get "path" path
  - When I run `../bin/coap_client coap://coap.me/path`
  - Then I get no error

### Scenario: get "location1" path
  - When I run `../bin/coap_client coap://coap.me/location1`
  - Then I get no error

### Scenario: get "3" path
  - When I run `../bin/coap_client coap://coap.me/3`
  - Then I get no error

### Scenario: get "4" path
  - When I run `../bin/coap_client coap://coap.me/4`
  - Then I get no error

### Scenario: get "5" path
  - When I run `../bin/coap_client coap://coap.me/5`
  - Then I get no error

### Scenario: get "location1/location2/location3" path
  - When I run `../bin/coap_client coap://coap.me/location1/location2/location3`
  - Then I get no error

### Scenario: get with server error
  - When I run `../bin/coap_client coap://coap.me/broken`
  - Then I get no error
```
5.00 Oops: broken
```

### Scenario: get with short query
  - When I run `../bin/coap_client  coap://coap.me/query?n=v`
  - Then I get no error
  - And output is
```
You asked me about: n=v
```

### Scenario: get with long query
  - When I run `../bin/coap_client coap://coap.me/query?a=12345678901234567890abcdefghijklmnopqrstuvwxyz`
  - Then I get no error
  - And output is
```
You asked me about: a=12345678901234567890abcdefghijklmnopqrstuvwxyz
```

### Scenario: explicit get with error 4.04
  - When I run `../bin/coap_client -m get coap://coap.me/hola`
  - Then I get no error
  - And output is
```
4.04 Not found
```

### Scenario: put method
  - When I run `../bin/coap_client -m put -e Hola coap://coap.me/test`
  - Then I get no error
  - And output is
```
Hola
PUT OK
```

### Scenario: post method
  - When I run `../bin/coap_client -m post -e "This is a test" coap://coap.me/test`
  - Then I get no error
  - And output is
```
This is a test
POST OK
```

### Scenario: post method error 4.05
  - When I run `../bin/coap_client -m post -e "This is a test" coap://coap.me/forbidden`
  - Then I get no error
  - And output is
```
4.05 Method not supported here
```

