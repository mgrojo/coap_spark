
## [coaps_client_tests.md](coaps_client_tests.md)  

  ### Feature: CoAP secure communication with server  

    OK  : When I run `../bin/coap_client -v 4 -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/`
    OK  : Then I get no error
    OK  : And the output contains 
  - [X] scenario [get method with test path and high verbosity I](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost`
    OK  : Then I get no error
    OK  : And output contains `This is a test server made with libcoap`
  - [X] scenario [get method without path](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/`
    OK  : Then I get no error
    OK  : And output contains `This is a test server made with libcoap`
  - [X] scenario [get method with empty path](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost:5684`
    OK  : Then I get no error
    OK  : And output contains `This is a test server made with libcoap`
  - [X] scenario [get method with port and no path](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m put -e "New resource" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`
    OK  : Then I get no error
  - [X] scenario [put method with path](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`
    OK  : Then I get no error
    OK  : And output contains `New resource`
  - [X] scenario [explicit get method with path](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://127.0.0.1/`
    OK  : Then I get no error
    OK  : And output contains `This is a test server made with libcoap`
  - [X] scenario [explicit get method with IP and path](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost:5684/test`
    OK  : Then I get no error
    OK  : And output contains `New resource`
  - [X] scenario [get method with port and path](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m put -e "New resource inside another" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test/inner`
    OK  : Then I get no error
    OK  : And there is no output
  - [X] scenario [put method within previous resource](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test/inner`
    OK  : Then I get no error
    OK  : And output contains `New resource inside another`
  - [X] scenario [get method with nested resource](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/this/does/not/exist`
    OK  : Then I get no error
    OK  : And the output is
  - [X] scenario [get non-existent resource with path](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m put -e "very long resource name" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/123412341234123412341234`
    OK  : Then I get no error
    OK  : And there is no output
  - [X] scenario [put on long path](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/123412341234123412341234`
    OK  : Then I get no error
    OK  : And the output contains
  - [X] scenario [get "123412341234123412341234" path](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/hola`
    OK  : Then I get no error
    OK  : And output is
  - [X] scenario [explicit get with error 4.04](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m post -e "This is a test" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`
    OK  : Then I get no error
    OK  : And output is
  - [X] scenario [post method error 4.05](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m delete -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`
    OK  : Then I get no error
    OK  : And there is no output
  - [X] scenario [delete method with path](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`
    OK  : Then I get no error
    OK  : And the output is
  - [X] scenario [explicit get method with path of just deleted resource](coaps_client_tests.md) pass  

  ### Feature: some miscelaneous error conditions  

    OK  : When I run `../bin/coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://coap.me/test`
    OK  : Then I get an error
    OK  : And output contains `Communication problems.`
  - [X] scenario [coaps not supported by server](coaps_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m get -k COAP_SPARK_KEYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY -u coap_spark coaps://localhost/`
    OK  : Then I get an error
    OK  : And output contains `Key too long`
  - [X] scenario [Key too long](coaps_client_tests.md) pass  


------------------
- Failed     =  0
- Successful =  19
- Empty      =  0
