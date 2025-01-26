
## [coap_client_tests.md](coap_client_tests.md)  
*** NOK : Then I get no error (coap_client_tests.md:12:)  
No error expected, but got one ( 1)  
*** NOK : And output contains `coap_client v0.1.` (coap_client_tests.md:13:)  
Output:  
|   
| raised GNAT.SOCKETS.HOST_ERROR : [2] Try again: ersion  
| [../bin/coap_client]  
| 0x5e8b66 Gnat.Sockets.Raise_Host_Error at g-socket.adb:2105  
| 0x5eda3c Gnat.Sockets.Get_Host_By_Name at g-socket.adb:1294  
| 0x410351 Coap_Spark.Channel.Connect at coap_spark-channel.adb:156  
| 0x40ccd6 Coap_Client at coap_client.adb:241  
| 0x40ba04 Main at b__coap_client.adb:408  
| [/lib/x86_64-linux-gnu/libc.so.6]  
| 0x760109c29d8e  
| 0x760109c29e3e  
| [../bin/coap_client]  
| 0x40ba53 _start at ???  
| 0xfffffffffffffffe  

does not contain expected:  
| coap_client v0.1.  

  
  - [ ] scenario [version message](coap_client_tests.md) fails  

*** NOK : Then I get no error (coap_client_tests.md:17:)  
No error expected, but got one ( 1)  
*** NOK : And output contains `Usage: coap_client` (coap_client_tests.md:18:)  
Output:  
|   
| raised GNAT.SOCKETS.HOST_ERROR : [2] Try again: elp  
| [../bin/coap_client]  
| 0x5e8b66 Gnat.Sockets.Raise_Host_Error at g-socket.adb:2105  
| 0x5eda3c Gnat.Sockets.Get_Host_By_Name at g-socket.adb:1294  
| 0x410351 Coap_Spark.Channel.Connect at coap_spark-channel.adb:156  
| 0x40ccd6 Coap_Client at coap_client.adb:241  
| 0x40ba04 Main at b__coap_client.adb:408  
| [/lib/x86_64-linux-gnu/libc.so.6]  
| 0x7a8bb4629d8e  
| 0x7a8bb4629e3e  
| [../bin/coap_client]  
| 0x40ba53 _start at ???  
| 0xfffffffffffffffe  

does not contain expected:  
| Usage: coap_client  

  
  - [ ] scenario [help message](coap_client_tests.md) fails  

*** NOK : And output contains `Usage: coap_client` (coap_client_tests.md:23:)  
Output:  
|   
| raised GNAT.SOCKETS.HOST_ERROR : [2] Try again: nknown-option  
| [../bin/coap_client]  
| 0x5e8b66 Gnat.Sockets.Raise_Host_Error at g-socket.adb:2105  
| 0x5eda3c Gnat.Sockets.Get_Host_By_Name at g-socket.adb:1294  
| 0x410351 Coap_Spark.Channel.Connect at coap_spark-channel.adb:156  
| 0x40ccd6 Coap_Client at coap_client.adb:241  
| 0x40ba04 Main at b__coap_client.adb:408  
| [/lib/x86_64-linux-gnu/libc.so.6]  
| 0x758adc029d8e  
| 0x758adc029e3e  
| [../bin/coap_client]  
| 0x40ba53 _start at ???  
| 0xfffffffffffffffe  

does not contain expected:  
| Usage: coap_client  

  
  - [ ] scenario [usage message on unrecognized option](coap_client_tests.md) fails  

*** NOK : Then I get an error (coap_client_tests.md:27:)  
Expected error code, got no error  
  - [ ] scenario [coaps not supported by server](coap_client_tests.md) fails  

*** NOK : Then I get an error (coap_client_tests.md:31:)  
Expected error code, got no error  
  - [ ] scenario [invalid method](coap_client_tests.md) fails  

*** NOK : And the output contains  (coap_client_tests.md:36:)  
Output:  
| welcome to the ETSI plugtest! last change: 2025-01-26 22:52:40 UTC  

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

*** NOK : And the output is (coap_client_tests.md:110:)  
Output:  
| Not authorized  

not equal to expected:  
| 4.01 Not authorized  

  
  - [ ] scenario [get "secret" path](coap_client_tests.md) fails  

*** NOK : And output does not contain `MALFORMED_MESSAGE` (coap_client_tests.md:118:)  
Output:  
| MALFORMED_MESSAGE  

contains unexpected:  
| MALFORMED_MESSAGE  

  
  - [ ] scenario [get "weird33" path](coap_client_tests.md) fails  

*** NOK : And output does not contain `MALFORMED_MESSAGE` (coap_client_tests.md:123:)  
Output:  
| MALFORMED_MESSAGE  

contains unexpected:  
| MALFORMED_MESSAGE  

  
  - [ ] scenario [get "weird44" path](coap_client_tests.md) fails  

*** NOK : And output does not contain `MALFORMED_MESSAGE` (coap_client_tests.md:128:)  
Output:  
| MALFORMED_MESSAGE  

contains unexpected:  
| MALFORMED_MESSAGE  

  
  - [ ] scenario [get "weird55" path](coap_client_tests.md) fails  

*** NOK : And output does not contain `MALFORMED_MESSAGE` (coap_client_tests.md:133:)  
Output:  
| MALFORMED_MESSAGE  

contains unexpected:  
| MALFORMED_MESSAGE  

  
  - [ ] scenario [get "weird333" path](coap_client_tests.md) fails  

*** NOK : And output does not contain `MALFORMED_MESSAGE` (coap_client_tests.md:138:)  
Output:  
| MALFORMED_MESSAGE  

contains unexpected:  
| MALFORMED_MESSAGE  

  
  - [ ] scenario [get "weird3333" path](coap_client_tests.md) fails  

*** NOK : And output does not contain `MALFORMED_MESSAGE` (coap_client_tests.md:143:)  
Output:  
| MALFORMED_MESSAGE  

contains unexpected:  
| MALFORMED_MESSAGE  

  
  - [ ] scenario [get "weird33333" path](coap_client_tests.md) fails  

*** NOK : And output is (coap_client_tests.md:219:)  
Output:  
| Not found  

not equal to expected:  
| 4.04 Not found  

  
  - [ ] scenario [explicit get with error 4.04](coap_client_tests.md) fails  

*** NOK : And output is (coap_client_tests.md:236:)  
Output:  
| "This is a test"  
| POST OK  

not equal to expected:  
| This is a test  
| POST OK  

  
  - [ ] scenario [post method](coap_client_tests.md) fails  

*** NOK : And output is (coap_client_tests.md:245:)  
Output:  
| "This is a test"  
| Method not supported here  

not equal to expected:  
| 4.05 Method not supported here  

  
  - [ ] scenario [post method error 4.05](coap_client_tests.md) fails  


------------------
- Failed     =  16
- Successful =  23
- Empty      =  0
- Not run    =  0
