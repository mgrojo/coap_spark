
# Document: [coap_server_tests.md](coap_server_tests.md)  
  ## Feature: CoAP Server command line  
   ### Scenario: [version message](coap_server_tests.md): 
   - OK : When I run `../bin/coap_server --version`  
   - OK : Then I get no error  
   - OK : And output contains `coap_server v0.`  
   - [X] scenario   [version message](coap_server_tests.md) pass  

   ### Scenario: [help message](coap_server_tests.md): 
   - OK : When I run `../bin/coap_server --help`  
   - OK : Then I get no error  
   - OK : And output contains `Usage: coap_server`  
   - [X] scenario   [help message](coap_server_tests.md) pass  

   ### Scenario: [usage message on unrecognized option](coap_server_tests.md): 
   - OK : When I run `../bin/coap_server --unknown-option`  
   - OK : Then I get an error  
   - OK : And output contains `Usage: coap_server`  
   - [X] scenario   [usage message on unrecognized option](coap_server_tests.md) pass  

   ### Scenario: [no value for -u](coap_server_tests.md): 
   - OK : When I run `../bin/coap_server -v 4 -u`  
   - OK : Then I get an error  
   - OK : And output contains `Unexpected option: -u`  
   - [X] scenario   [no value for -u](coap_server_tests.md) pass  

   ### Scenario: [invalid verbosity level](coap_server_tests.md): 
   - OK : When I run `../bin/coap_server -v fatal`  
   - OK : Then I get an error  
   - OK : And output contains `Invalid verbosity level`  
   - [X] scenario   [invalid verbosity level](coap_server_tests.md) pass  

   ### Scenario: [Verbosity level too high](coap_server_tests.md): 
   - OK : When I run `../bin/coap_server -v 10`  
   - OK : Then I get an error  
   - OK : And output contains `Verbosity level too high`  
   - [X] scenario   [Verbosity level too high](coap_server_tests.md) pass  

   ### Scenario: [invalid argument](coap_server_tests.md): 
   - OK : When I run `../bin/coap_server -z`  
   - OK : Then I get an error  
   - OK : And output contains `Invalid option: -z`  
   - [X] scenario   [invalid argument](coap_server_tests.md) pass  

   ### Scenario: [Invalid port](coap_server_tests.md): 
   - OK : When I run `../bin/coap_server -p 100000`  
   - OK : Then I get an error  
   - OK : And output contains `Invalid specified port`  
   - [X] scenario   [Invalid port](coap_server_tests.md) pass  

   ### Scenario: [specified port and invalid option](coap_server_tests.md): 
   - OK : When I run `../bin/coap_server -p 1234 -? 0`  
   - OK : Then I get an error  
   - OK : And output contains `Invalid option: -?`  
   - [X] scenario   [specified port and invalid option](coap_server_tests.md) pass  

  ## Feature: CoAP communication with server  
   ### Scenario: [get method with test path and high verbosity I](coap_server_tests.md): 
   - OK : When I run `coap_client -v 4 coap://localhost/`  
   - OK : Then I get no error  
   - OK : And the output contains   
   - [X] scenario   [get method with test path and high verbosity I](coap_server_tests.md) pass  

   ### Scenario: [get method without path](coap_server_tests.md): 
   - OK : When I run `coap_client coap://localhost`  
   - OK : Then I get no error  
   - OK : And output contains `This is a test server made with`  
   - [X] scenario   [get method without path](coap_server_tests.md) pass  

   ### Scenario: [get method with empty path](coap_server_tests.md): 
   - OK : When I run `coap_client coap://localhost/`  
   - OK : Then I get no error  
   - OK : And output contains `This is a test server made with`  
   - [X] scenario   [get method with empty path](coap_server_tests.md) pass  

   ### Scenario: [get method with port and no path](coap_server_tests.md): 
   - OK : When I run `coap_client coap://localhost:5683`  
   - OK : Then I get no error  
   - OK : And output contains `This is a test server made with`  
   - [X] scenario   [get method with port and no path](coap_server_tests.md) pass  

   ### Scenario: [post method with path](coap_server_tests.md): 
   - OK : When I run `coap_client -m post -e "New resource" coap://localhost/test`  
   - OK : Then I get no error  
   - [X] scenario   [post method with path](coap_server_tests.md) pass  

   ### Scenario: [explicit get method with path](coap_server_tests.md): 
   - OK : When I run `coap_client -m get coap://localhost/test`  
   - OK : Then I get no error  
   - OK : And output contains `New resource`  
   - [X] scenario   [explicit get method with path](coap_server_tests.md) pass  

   ### Scenario: [post method with already created path](coap_server_tests.md): 
   - OK : When I run `coap_client -m post -e "New resource" coap://localhost/test`  
   - OK : Then I get no error  
   - OK : And output contains `Resource already exists`  
   - [X] scenario   [post method with already created path](coap_server_tests.md) pass  

   ### Scenario: [explicit get method with IP and path](coap_server_tests.md): 
   - OK : When I run `coap_client -m get coap://127.0.0.1/`  
   - OK : Then I get no error  
   - OK : And output contains `This is a test server made with`  
   - [X] scenario   [explicit get method with IP and path](coap_server_tests.md) pass  

   ### Scenario: [get method with port and path](coap_server_tests.md): 
   - OK : When I run `coap_client coap://localhost:5683/test`  
   - OK : Then I get no error  
   - OK : And output contains `New resource`  
   - [X] scenario   [get method with port and path](coap_server_tests.md) pass  

   ### Scenario: [post method within previous resource](coap_server_tests.md): 
   - OK : When I run `coap_client -m post -e "New resource inside another" coap://localhost/test/inner`  
   - OK : Then I get no error  
   - [X] scenario   [post method within previous resource](coap_server_tests.md) pass  

   ### Scenario: [get method with nested resource](coap_server_tests.md): 
   - OK : When I run `coap_client -m get coap://localhost/test/inner`  
   - OK : Then I get no error  
   - OK : And output contains `New resource inside another`  
   - [X] scenario   [get method with nested resource](coap_server_tests.md) pass  

   ### Scenario: [get non-existent resource with path](coap_server_tests.md): 
   - OK : When I run `coap_client coap://localhost/this/does/not/exist`  
   - OK : Then I get no error  
   - OK : And the output is  
   - [X] scenario   [get non-existent resource with path](coap_server_tests.md) pass  

   ### Scenario: [post on long path](coap_server_tests.md): 
   - OK : When I run `coap_client -m post -e "very long resource name" coap://localhost/123412341234123412341234`  
   - OK : Then I get no error  
   - [X] scenario   [post on long path](coap_server_tests.md) pass  

   ### Scenario: [get "123412341234123412341234" path](coap_server_tests.md): 
   - OK : When I run `coap_client coap://localhost/123412341234123412341234`  
   - OK : Then I get no error  
   - OK : And the output contains  
   - [X] scenario   [get "123412341234123412341234" path](coap_server_tests.md) pass  

   ### Scenario: [explicit get with error 4.04](coap_server_tests.md): 
   - OK : When I run `coap_client -m get coap://localhost/hola`  
   - OK : Then I get no error  
   - OK : And output is  
   - [X] scenario   [explicit get with error 4.04](coap_server_tests.md) pass  

   ### Scenario: [post method error 4.05](coap_server_tests.md): 
   - OK : When I run `coap_client -m patch -e "This is a test" coap://localhost/test`  
   - OK : Then I get no error  
   - OK : And output is  
   - [X] scenario   [post method error 4.05](coap_server_tests.md) pass  

   ### Scenario: [delete method with path](coap_server_tests.md): 
   - OK : When I run `coap_client -m delete coap://localhost/test`  
   - OK : Then I get no error  
   - [X] scenario   [delete method with path](coap_server_tests.md) pass  

   ### Scenario: [delete method with already deleted path](coap_server_tests.md): 
   - OK : When I run `coap_client -m delete coap://localhost/test`  
   - OK : Then I get no error  
   - OK : And the output is  
   - [X] scenario   [delete method with already deleted path](coap_server_tests.md) pass  

   ### Scenario: [explicit get method with path of just deleted resource](coap_server_tests.md): 
   - OK : When I run `coap_client -m get coap://localhost/test`  
   - OK : Then I get no error  
   - OK : And the output is  
   - [X] scenario   [explicit get method with path of just deleted resource](coap_server_tests.md) pass  

   ### Scenario: [nc request to the server](coap_server_tests.md): 
   - OK : When I run `nc -w 2 -v -u localhost 5683`  
   - OK : Then I get no error  
   - OK : And the output is  
   - [X] scenario   [nc request to the server](coap_server_tests.md) pass  


## Summary : **Success**, 29 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 29    |
| Empty      | 0     |
| Not Run    | 0     |

