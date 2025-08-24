# Black box tests for CoAP Server
These are the test scenarios for `coap_server` to be run using the `bbt` tool.
They test cases where the server processes the command line and terminates.

You can install `bbt` with `alr install bbt`.

Use this command to run the tests: `bbt coap_server_tests.md`

## Feature: CoAP Server command line

### Scenario: version message
  - When I run `../bin/coap_server --version`
  - Then I get no error
  - And output contains `coap_server v0.`

### Scenario: help message
  - When I run `../bin/coap_server --help`
  - Then I get no error
  - And output contains `Usage: coap_server`

### Scenario: usage message on unrecognized option
  - When I run `../bin/coap_server --unknown-option`
  - Then I get an error
  - And output contains `Usage: coap_server`

### Scenario: no value for -u
  - When I run `../bin/coap_server -v 4 -u`
  - Then I get an error
  - And output contains `Unexpected option: -u`

### Scenario: invalid verbosity level
  - When I run `../bin/coap_server -v fatal`
  - Then I get an error
  - And output contains `Invalid verbosity level`

### Scenario: Verbosity level too high
  - When I run `../bin/coap_server -v 10`
  - Then I get an error
  - And output contains `Verbosity level too high`

### Scenario: invalid argument
  - When I run `../bin/coap_server -z`
  - Then I get an error
  - And output contains `Invalid option: -z`

### Scenario: Invalid port
  - When I run `../bin/coap_server -p 100000`
  - Then I get an error
  - And output contains `Invalid specified port`

# Black box tests for CoAP Server
These are the test scenarios for `coap_server` to be run using the `bbt` tool.

You can install `bbt` with `alr install bbt`.

Use this command to run the tests: `bbt coap_server_tests.md`

These scenarios require connectivity to a server running locally in this way:
```
coap_server
```

`coap_client` has to be visible in the PATH, because each scenario invokes it
directly.

## Feature: CoAP secure communication with server

### Scenario: get method with test path and high verbosity I
This test has to be broken in two contains checks, because the ETAG
option will be determined by the server, so we have to exclude it,
and it is in the middle of the output.

  - When I run `coap_client -v 4 coap://localhost/`
  - Then I get no error
  - And the output contains 
```
Method: GET  
Scheme: coap  
Host: localhost  
Port: 5683  
Path: /  
Query:   
REQUEST:   
Option: URI_HOST  
  - Length:  9  
  - Value: localhost  
Option: URI_PORT  
  - Length:  2  
  - Value:  5683  

RESPONSE:   
Server answered with success.  
Content-Format: text/plain; charset=utf-8  
Payload: This is a test server made with CoAP-SPARK
```

### Scenario: get method without path
  - When I run `coap_client coap://localhost`
  - Then I get no error
  - And output contains `This is a test server made with`


### Scenario: get method with empty path
  - When I run `coap_client coap://localhost/`
  - Then I get no error
  - And output contains `This is a test server made with`

### Scenario: get method with port and no path
  - When I run `coap_client coap://localhost:5683`
  - Then I get no error
  - And output contains `This is a test server made with`

### Scenario: post method with path
  - When I run `coap_client -m post -e "New resource" coap://localhost/test`
  - Then I get no error

### Scenario: explicit get method with path
  - When I run `coap_client -m get coap://localhost/test`
  - Then I get no error
  - And output contains `New resource`

### Scenario: explicit get method with IP and path
  - When I run `coap_client -m get coap://127.0.0.1/`
  - Then I get no error
  - And output contains `This is a test server made with`

### Scenario: get method with port and path
  - When I run `coap_client coap://localhost:5683/test`
  - Then I get no error
  - And output contains `New resource`

### Scenario: post method within previous resource
  - When I run `coap_client -m post -e "New resource inside another" coap://localhost/test/inner`
  - Then I get no error

### Scenario: get method with nested resource
  - When I run `coap_client -m get coap://localhost/test/inner`
  - Then I get no error
  - And output contains `New resource inside another`

### Scenario: get non-existent resource with path
  - When I run `coap_client coap://localhost/this/does/not/exist`
  - Then I get no error
  - And the output is
```
4.04 Resource not found
```

### Scenario: post on long path
  - When I run `coap_client -m post -e "very long resource name" coap://localhost/123412341234123412341234`
  - Then I get no error

### Scenario: get "123412341234123412341234" path
  - When I run `coap_client coap://localhost/123412341234123412341234`
  - Then I get no error
  - And the output contains
```
very long resource name
```

### Scenario: explicit get with error 4.04
  - When I run `coap_client -m get coap://localhost/hola`
  - Then I get no error
  - And output is
```
4.04 Resource not found
```

### Scenario: post method error 4.05
  - When I run `coap_client -m patch -e "This is a test" coap://localhost/test`
  - Then I get no error
  - And output is
```
4.05 Method not supported
```

### Scenario: delete method with path
  - When I run `coap_client -m delete coap://localhost/test`
  - Then I get no error

### Scenario: explicit get method with path of just deleted resource
  - When I run `coap_client -m get coap://localhost/test`
  - Then I get no error
  - And the output is
```
4.04 Resource not found
```
