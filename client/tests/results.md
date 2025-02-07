
## [coap_client_tests.md](coap_client_tests.md)  

  ### Feature: CoAP Client command line  

    OK  : When I run `../bin/coap_client --version`
    OK  : Then I get no error
    OK  : And output contains `coap_client v0.1.`
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
*** NOK : And output is (coap_client_tests.md:284:)  
Output:  
| "This is a test"  
| 4.05 Method not supported here  

not equal to expected:  
| 4.05 Method not supported here  

  
  - [ ] scenario [post method error 4.05](coap_client_tests.md) fails  

  ### Feature: some miscelaneous error conditions  

    OK  : When I run `../bin/coap_client http://coap.me/`
    OK  : Then I get an error
    OK  : And output contains `invalid URI`
  - [X] scenario [invalid CoAP URI (invalid scheme and path)](coap_client_tests.md) pass  

    OK  : When I run `../bin/coap_client http://coap.me`
    OK  : Then I get an error
*** NOK : And output contains `invalid URI` (coap_client_tests.md:300:)  
Output:  
|   
| raised GNAT.SOCKETS.SOCKET_ERROR : [111] Connection refused  
| [../bin/coap_client]  
| 0x5ebe49 Gnat.Sockets.Raise_Socket_Error at g-socket.adb:2117  
| 0x5ede67 Gnat.Sockets.Receive_Socket at g-socket.adb:2182  
| 0x40fab4 Coap_Spark.Channel.Receive at coap_spark-channel.adb:240  
| 0x4108cf Coap_Spark.Channel.Receive at coap_spark-channel.adb:210  
| 0x40d5a4 Coap_Client at coap_client.adb:78  
| 0x40ba24 Main at b__coap_client.adb:412  
| [/lib/x86_64-linux-gnu/libc.so.6]  
| 0x7653db029d8e  
| 0x7653db029e3e  
| [../bin/coap_client]  
| 0x40ba73 _start at ???  
| 0xfffffffffffffffe  

does not contain expected:  
| invalid URI  

  
  - [ ] scenario [invalid CoAP URI (invalid scheme and no path)](coap_client_tests.md) fails  

    OK  : When I run `../bin/coap_client coap.me`
    OK  : Then I get an error
*** NOK : And output contains `invalid URI` (coap_client_tests.md:305:)  
Output:  
|   
| raised GNAT.SOCKETS.HOST_ERROR : [1] Host not found: p.me  
| [../bin/coap_client]  
| 0x5e8966 Gnat.Sockets.Raise_Host_Error at g-socket.adb:2105  
| 0x5ed83c Gnat.Sockets.Get_Host_By_Name at g-socket.adb:1294  
| 0x4100d1 Coap_Spark.Channel.Connect at coap_spark-channel.adb:156  
| 0x40cb94 Coap_Client at coap_client.adb:278  
| 0x40ba24 Main at b__coap_client.adb:412  
| [/lib/x86_64-linux-gnu/libc.so.6]  
| 0x74b828829d8e  
| 0x74b828829e3e  
| [../bin/coap_client]  
| 0x40ba73 _start at ???  
| 0xfffffffffffffffe  

does not contain expected:  
| invalid URI  

  
  - [ ] scenario [invalid CoAP URI (no scheme)](coap_client_tests.md) fails  

    OK  : When I run `../bin/coap_client -m get coaps://coap.me/`
    OK  : Then I get an error
  - [X] scenario [coaps not supported by server](coap_client_tests.md) pass  


------------------
- Failed     =  3
- Successful =  41
- Empty      =  0
- Not run    =  0
