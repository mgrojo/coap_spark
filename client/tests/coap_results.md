
## [coap_client_tests.md](coap_client_tests.md)  

  ### Feature: CoAP Client command line  

    OK  : When I run `../bin/coap_client --version`
    OK  : Then I get no error
    OK  : And output contains `coap_client v0.`
  - [X] scenario [version message](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client --help`
    OK  : Then I get no error
    OK  : And output contains `Usage: coap_client`
  - [X] scenario [help message](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client --unknown-option`
    OK  : Then I get an error
    OK  : And output contains `Usage: coap_client`
  - [X] scenario [usage message on unrecognized option](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -e Payload`
    OK  : Then I get an error
    OK  : And output contains `URI is missing`
  - [X] scenario [no URI](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -k coap://coap.me`
    OK  : Then I get an error
    OK  : And output contains `Missing argument for -k`
  - [X] scenario [no value for -k](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -v fatal coap://coap.me`
    OK  : Then I get an error
    OK  : And output contains `Invalid verbosity level`
  - [X] scenario [invalid verbosity level](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -v 10 coap://coap.me`
    OK  : Then I get an error
    OK  : And output contains `Verbosity level too high`
  - [X] scenario [Verbosity level too high](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -z coap://coap.me`
    OK  : Then I get an error
    OK  : And output contains `Invalid option: -z`
  - [X] scenario [invalid argument](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coaps://localhost`
    OK  : Then I get an error
    OK  : And output contains `Communication problems`
  - [X] scenario [communication problems](coap_client_tests.md) pass  

  ### Feature: ETSI CoAP plugtest  

    OK  : When I run `../bin/coap_client -v 4 coap://coap.me/test`
    OK  : Then I get no error
    OK  : And the output contains 
  - [X] scenario [get method with test path and high verbosity I](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -v 4 coap://coap.me/test`
    OK  : Then I get no error
    OK  : And the output contains 
  - [X] scenario [get method with test path and high verbosity II](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me`
    OK  : Then I get no error
    OK  : And output contains `</test>;rt="test";ct=0,</validate>;rt="validate"`
  - [X] scenario [get method without path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/`
    OK  : Then I get no error
    OK  : And output contains `</test>;rt="test";ct=0,</validate>;rt="validate"`
  - [X] scenario [get method with empty path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me:5683`
    OK  : Then I get no error
    OK  : And output contains `</test>;rt="test";ct=0,</validate>;rt="validate"`
  - [X] scenario [get method with port and no path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m get coap://coap.me/test`
    OK  : Then I get no error
    OK  : And output contains `welcome to the ETSI plugtest! last change:`
  - [X] scenario [explicit get method with path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m get coap://134.102.218.18/test`
    OK  : Then I get no error
    OK  : And output contains `welcome to the ETSI plugtest! last change:`
  - [X] scenario [explicit get method with IP and path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me:5683/test`
    OK  : Then I get no error
    OK  : And output contains `welcome to the ETSI plugtest! last change:`
  - [X] scenario [get method with port and path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/validate`
    OK  : Then I get no error
  - [X] scenario [get "validate" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/Type1`
    OK  : Then I get no error
  - [X] scenario [get "Type1" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/blåbærsyltetøy`
    OK  : Then I get no error
  - [X] scenario [get "blåbærsyltetøy" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/sink`
    OK  : Then I get no error
  - [X] scenario [get "sink" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/separate`
    OK  : Then I get no error
  - [X] scenario [get "separate" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/secret`
    OK  : Then I get no error
    OK  : And the output is
  - [X] scenario [get "secret" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/weird33`
    OK  : Then I get an error
    OK  : And output does not contain `MALFORMED_MESSAGE`
    OK  : And output contains `UNKNOWN_CRITICAL_OPTION`
  - [X] scenario [get "weird33" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/weird44`
    OK  : Then I get no error
    OK  : And output does not contain `MALFORMED_MESSAGE`
    OK  : And output contains `resource with option 44`
  - [X] scenario [get "weird44" (unknown elective option I)](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -v 4 coap://coap.me/weird44`
    OK  : Then I get no error
    OK  : And output does not contain `MALFORMED_MESSAGE`
    OK  : And output contains `unknown elective option`
    OK  : And output contains `resource with option 44`
  - [X] scenario [get "weird44" (unknown elective option II)](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/weird55`
    OK  : Then I get an error
    OK  : And output does not contain `MALFORMED_MESSAGE`
    OK  : And output contains `UNKNOWN_CRITICAL_OPTION`
  - [X] scenario [get "weird55" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/weird333`
    OK  : Then I get an error
    OK  : And output does not contain `MALFORMED_MESSAGE`
    OK  : And output contains `UNKNOWN_CRITICAL_OPTION`
  - [X] scenario [get "weird333" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/weird3333`
    OK  : Then I get an error
    OK  : And output does not contain `MALFORMED_MESSAGE`
    OK  : And output contains `UNKNOWN_CRITICAL_OPTION`
  - [X] scenario [get "weird3333" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/weird33333`
    OK  : Then I get an error
    OK  : And output does not contain `MALFORMED_MESSAGE`
    OK  : And output contains `UNKNOWN_CRITICAL_OPTION`
  - [X] scenario [get "weird33333" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/123412341234123412341234`
    OK  : Then I get no error
    OK  : And the output contains
  - [X] scenario [get "123412341234123412341234" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/create1`
    OK  : Then I get no error
  - [X] scenario [get "create1" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/query`
    OK  : Then I get no error
    OK  : And the output contains
  - [X] scenario [get "query" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/seg1`
    OK  : Then I get no error
  - [X] scenario [get "seg1" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/path`
    OK  : Then I get no error
  - [X] scenario [get "path" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/location1`
    OK  : Then I get no error
  - [X] scenario [get "location1" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/3`
    OK  : Then I get no error
  - [X] scenario [get "3" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/4`
    OK  : Then I get no error
  - [X] scenario [get "4" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/5`
    OK  : Then I get no error
  - [X] scenario [get "5" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/location1/location2/location3`
    OK  : Then I get no error
  - [X] scenario [get "location1/location2/location3" path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/broken`
    OK  : Then I get no error
  - [X] scenario [get with server error](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client  coap://coap.me/query?n=v`
    OK  : Then I get no error
    OK  : And output is
  - [X] scenario [get with short query](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me/query?a=12345678901234567890abcdefghijklmnopqrstuvwxyz`
    OK  : Then I get no error
    OK  : And output is
  - [X] scenario [get with long query](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m get coap://coap.me/hola`
    OK  : Then I get no error
    OK  : And output is
  - [X] scenario [explicit get with error 4.04](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m put -e Hola coap://coap.me/test`
    OK  : Then I get no error
    OK  : And output is
  - [X] scenario [put method](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m post -e "This is a test" coap://coap.me/test`
    OK  : Then I get no error
    OK  : And output is
  - [X] scenario [post method](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m post -e "This is a test" coap://coap.me/forbidden`
    OK  : Then I get no error
    OK  : And output is
  - [X] scenario [post method error 4.05](coap_client_tests.md) pass  

  ### Feature: some miscelaneous error conditions  

    OK  : When I run `../bin/coap_client http://coap.me/`
    OK  : Then I get an error
    OK  : And output contains `invalid URI`
  - [X] scenario [invalid CoAP URI (invalid scheme and path)](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client http://coap.me`
    OK  : Then I get an error
    OK  : And output contains `invalid URI`
  - [X] scenario [invalid CoAP URI (invalid scheme and no path)](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap.me`
    OK  : Then I get an error
    OK  : And output contains `invalid URI`
  - [X] scenario [invalid CoAP URI (no scheme)](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m gato http://coap.me/`
    OK  : Then I get an error
    OK  : And output contains `invalid method`
  - [X] scenario [invalid CoAP method](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m get coaps://coap.me/`
    OK  : Then I get an error
  - [X] scenario [coaps not supported by server](coap_client_tests.md) pass  


------------------
- Failed     =  0
- Successful =  52
- Empty      =  0
