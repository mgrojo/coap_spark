# Black box tests for CoAP Client
These are the test scenarios for `coap_client` to be run using the `bbt` tool.

You can install `bbt` with `alr install bbt`.

Use this command to run the tests: `bbt coaps_client_tests.md`

These scenarios require connectivity to a server running locally in this way:
```
coap-server-openssl -k COAP_SPARK_KEY_5684 -u coap_spark -d 100
```

## Feature: CoAP secure communication with server

See https://libcoap.net for details.

### Scenario: get method with test path and high verbosity I
This test has to be broken in two contains checks, because the ETAG
option will be determined by the server, so we have to exclude it,
and it is in the middle of the output.

  - When I run `../bin/coap_client -v 4 -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/`
  - Then I get no error
  - And the output contains 
```
Method: GET
Scheme: coaps
Host: localhost
Port: 5684
Path: /
Query: 
REQUEST: 
Option: URI_HOST
  - Length:  9
  - Value: localhost
Option: URI_PORT
  - Length:  2
  - Value:  5684
Hint: CoAP

RESPONSE: 
Server answered with success.
Option: CONTENT_FORMAT
  - Length: 0
  - Value: (empty)
Option: MAX_AGE
  - Length:  3
  - Value:  196607
Content-Format: text/plain; charset=utf-8
Payload: This is a test server made with libcoap
```

### Scenario: get method without path
  - When I run `../bin/coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost`
  - Then I get no error
  - And output contains `This is a test server made with libcoap`


### Scenario: get method with empty path
  - When I run `../bin/coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/`
  - Then I get no error
  - And output contains `This is a test server made with libcoap`

### Scenario: get method with port and no path
  - When I run `../bin/coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost:5684`
  - Then I get no error
  - And output contains `This is a test server made with libcoap`

### Scenario: put method with path
  - When I run `../bin/coap_client -m put -e "New resource" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`
  - Then I get no error

### Scenario: explicit get method with path
  - When I run `../bin/coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`
  - Then I get no error
  - And output contains `New resource`

### Scenario: explicit get method with IP and path
  - When I run `../bin/coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://127.0.0.1/`
  - Then I get no error
  - And output contains `This is a test server made with libcoap`

### Scenario: get method with port and path
  - When I run `../bin/coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost:5684/test`
  - Then I get no error
  - And output contains `New resource`

### Scenario: put method within previous resource
  - When I run `../bin/coap_client -m put -e "New resource inside another" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test/inner`
  - Then I get no error
  - And there is no output

### Scenario: get method with nested resource
  - When I run `../bin/coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test/inner`
  - Then I get no error
  - And output contains `New resource inside another`

### Scenario: get non-existent resource with path
  - When I run `../bin/coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/this/does/not/exist`
  - Then I get no error
  - And the output is
```
4.04 Not Found
```

### Scenario: put on long path
  - When I run `../bin/coap_client -m put -e "very long resource name" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/123412341234123412341234`
  - Then I get no error
  - And there is no output

### Scenario: get "123412341234123412341234" path
  - When I run `../bin/coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/123412341234123412341234`
  - Then I get no error
  - And the output contains
```
very long resource name
```

### Scenario: explicit get with error 4.04
  - When I run `../bin/coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/hola`
  - Then I get no error
  - And output is
```
4.04 Not found
```

### Scenario: post method error 4.05
  - When I run `../bin/coap_client -m post -e "This is a test" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`
  - Then I get no error
  - And output is
```
4.05 Method Not Allowed
```

## Feature: some miscelaneous error conditions
Tests for incorrect URIs or incompatibities with the server.

### Scenario: coaps not supported by server
  - When I run `../bin/coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://coap.me/test`
  - Then I get an error
  - And output contains `Communication problems.`
