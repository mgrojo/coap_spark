
## [coap_client_tests.md](coap_client_tests.md)  

  ### Feature: CoAP Client to coap://coap.me  

    OK  : When I run `../bin/coap_client --version`
    OK  : Then I get no error
    OK  : And output contains `coap_client v0.1.`
  - [X] scenario [version message](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client --help`
    OK  : Then I get no error
    OK  : And output contains `Usage: coap_client`
  - [X] scenario [help message](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client --unknown-option`
*** NOK : Then I get an error (coap_client_tests.md:22:)  
Expected error code, got no error  
    OK  : And output contains `Usage: coap_client`
  - [ ] scenario [usage message on unrecognized option](coap_client_tests.md) fails  

    OK  : When I run `../bin/coap_client -m get coaps://coap.me/`
*** NOK : Then I get an error (coap_client_tests.md:27:)  
Expected error code, got no error  
  - [ ] scenario [coaps not supported by server](coap_client_tests.md) fails  

    OK  : When I run `../bin/coap_client -m get http://coap.me/`
*** NOK : Then I get an error (coap_client_tests.md:31:)  
Expected error code, got no error  
  - [ ] scenario [invalid method](coap_client_tests.md) fails  

    OK  : When I run `../bin/coap_client coap://coap.me/test`
    OK  : Then I get no error
*** NOK : And the output contains  (coap_client_tests.md:36:)  
Output:  
| welcome to the ETSI plugtest! last change: 2025-01-27 22:09:45 UTC  

does not contain expected:  
| Method: GET  
| Scheme: coap  
| Host: coap.me  
| Port: 5683  
| Path: /test  
| Query:   
| REQUEST:   
| Option: URI_HOST  
|   - Length:  7  
|   - Value: coap.me  
| Option: URI_PORT  
|   - Length:  2  
|   - Value:  5683  
| Option: URI_PATH  
|   - Length:  4  
|   - Value: test  
|   
| RESPONSE:   
| Server answered with success.  
| Option: ETAG  
|   - Length:  8  
|   - Value:  217 170  27 123 139 211 160 191  
| Option: CONTENT_FORMAT  
|   - Length:  0  
|   - Value:   
| Content-Format: text/plain; charset=utf-8  
| Payload: welcome to the ETSI plugtest! last change:   

  
  - [ ] scenario [get method with test path and high verbosity](coap_client_tests.md) fails  

    OK  : When I run `../bin/coap_client coap://coap.me`
    OK  : Then I get no error
    OK  : And output contains `</test>;rt="test";ct=0,</validate>;rt="validate"`
  - [X] scenario [get method without path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client coap://coap.me:5683`
    OK  : Then I get no error
    OK  : And output contains `</test>;rt="test";ct=0,</validate>;rt="validate"`
  - [X] scenario [get method with port and no path](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client -m get coap://coap.me/test`
    OK  : Then I get no error
    OK  : And output contains `welcome to the ETSI plugtest! last change:`
  - [X] scenario [explicit get method with path](coap_client_tests.md) pass  

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
    OK  : Then I get no error
*** NOK : And output does not contain `MALFORMED_MESSAGE` (coap_client_tests.md:118:)  
Output:  
| MALFORMED_MESSAGE  

contains unexpected:  
| MALFORMED_MESSAGE  

  
  - [ ] scenario [get "weird33" path](coap_client_tests.md) fails  

    OK  : When I run `../bin/coap_client coap://coap.me/weird44`
    OK  : Then I get no error
*** NOK : And output does not contain `MALFORMED_MESSAGE` (coap_client_tests.md:123:)  
Output:  
| MALFORMED_MESSAGE  

contains unexpected:  
| MALFORMED_MESSAGE  

  
  - [ ] scenario [get "weird44" path](coap_client_tests.md) fails  

    OK  : When I run `../bin/coap_client coap://coap.me/weird55`
    OK  : Then I get no error
*** NOK : And output does not contain `MALFORMED_MESSAGE` (coap_client_tests.md:128:)  
Output:  
| MALFORMED_MESSAGE  

contains unexpected:  
| MALFORMED_MESSAGE  

  
  - [ ] scenario [get "weird55" path](coap_client_tests.md) fails  

    OK  : When I run `../bin/coap_client coap://coap.me/weird333`
    OK  : Then I get no error
*** NOK : And output does not contain `MALFORMED_MESSAGE` (coap_client_tests.md:133:)  
Output:  
| MALFORMED_MESSAGE  

contains unexpected:  
| MALFORMED_MESSAGE  

  
  - [ ] scenario [get "weird333" path](coap_client_tests.md) fails  

    OK  : When I run `../bin/coap_client coap://coap.me/weird3333`
    OK  : Then I get no error
*** NOK : And output does not contain `MALFORMED_MESSAGE` (coap_client_tests.md:138:)  
Output:  
| MALFORMED_MESSAGE  

contains unexpected:  
| MALFORMED_MESSAGE  

  
  - [ ] scenario [get "weird3333" path](coap_client_tests.md) fails  

    OK  : When I run `../bin/coap_client coap://coap.me/weird33333`
    OK  : Then I get no error
*** NOK : And output does not contain `MALFORMED_MESSAGE` (coap_client_tests.md:143:)  
Output:  
| MALFORMED_MESSAGE  

contains unexpected:  
| MALFORMED_MESSAGE  

  
  - [ ] scenario [get "weird33333" path](coap_client_tests.md) fails  

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
*** NOK : And output is (coap_client_tests.md:236:)  
Output:  
| "This is a test"  
| POST OK  

not equal to expected:  
| This is a test  
| POST OK  

  
  - [ ] scenario [post method](coap_client_tests.md) fails  

    OK  : When I run `../bin/coap_client -m post -e "This is a test" coap://coap.me/forbidden`
    OK  : Then I get no error
*** NOK : And output is (coap_client_tests.md:245:)  
Output:  
| "This is a test"  
| 4.05 Method not supported here  

not equal to expected:  
| 4.05 Method not supported here  

  
  - [ ] scenario [post method error 4.05](coap_client_tests.md) fails  


------------------
- Failed     =  12
- Successful =  27
- Empty      =  0
- Not run    =  0
