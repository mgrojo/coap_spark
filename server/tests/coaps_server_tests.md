# Black box tests for CoAP Server
These are the test scenarios for `coap_server` to be run using the `bbt` tool.

You can install `bbt` with `alr install bbt`.

Use this command to run the tests: `bbt coaps_server_tests.md`

These scenarios require connectivity to a server running locally in this way:
```
coap_server -k COAP_SPARK_KEY_5684 -u coap_spark
```

`coap_client` has to be visible in the PATH, because each scenario invokes it
directly.

## Feature: CoAP secure communication with server

### Scenario: get method with test path and high verbosity I
This test has to be broken in two contains checks, because the ETAG
option will be determined by the server, so we have to exclude it,
and it is in the middle of the output.

  - When I run `coap_client -v 4 -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/`
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
Hint:   
  
RESPONSE:   
Server answered with success.  
Content-Format: text/plain; charset=utf-8  
Payload: This is a test server made with CoAP-SPARK
```

### Scenario: get method without path
  - When I run `coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost`
  - Then I get no error
  - And output contains `This is a test server made with`


### Scenario: get method with empty path
  - When I run `coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/`
  - Then I get no error
  - And output contains `This is a test server made with`

### Scenario: get method with port and no path
  - When I run `coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost:5684`
  - Then I get no error
  - And output contains `This is a test server made with`

### Scenario: post method with path
  - When I run `coap_client -m post -e "New resource" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`
  - Then I get no error

### Scenario: explicit get method with path
  - When I run `coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`
  - Then I get no error
  - And output contains `New resource`

### Scenario: explicit get method with IP and path
  - When I run `coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://127.0.0.1/`
  - Then I get no error
  - And output contains `This is a test server made with`

### Scenario: get method with port and path
  - When I run `coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost:5684/test`
  - Then I get no error
  - And output contains `New resource`

### Scenario: post method within previous resource
  - When I run `coap_client -m post -e "New resource inside another" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test/inner`
  - Then I get no error

### Scenario: get method with nested resource
  - When I run `coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test/inner`
  - Then I get no error
  - And output contains `New resource inside another`

### Scenario: get non-existent resource with path
  - When I run `coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/this/does/not/exist`
  - Then I get no error
  - And the output is
```
4.04 Resource not found
```

### Scenario: post on long path
  - When I run `coap_client -m post -e "very long resource name" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/123412341234123412341234`
  - Then I get no error

### Scenario: get "123412341234123412341234" path
  - When I run `coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/123412341234123412341234`
  - Then I get no error
  - And the output contains
```
very long resource name
```

### Scenario: explicit get with error 4.04
  - When I run `coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/hola`
  - Then I get no error
  - And output is
```
4.04 Resource not found
```

### Scenario: post method error 4.05
  - When I run `coap_client -m patch -e "This is a test" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`
  - Then I get no error
  - And output is
```
4.05 Method not supported
```

### Scenario: delete method with path
  - When I run `coap_client -m delete -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`
  - Then I get no error

### Scenario: explicit get method with path of just deleted resource
  - When I run `coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`
  - Then I get no error
  - And the output is
```
4.04 Resource not found
```
