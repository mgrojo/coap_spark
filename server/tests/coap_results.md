
# Document: [coap_server_tests.md](coap_server_tests.md)  
  ## Feature: CoAP Client command line  
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

   ### Scenario: [priviliged port](coap_server_tests.md): 
   - OK : When I run `../bin/coap_server -p 100000`  
   - OK : Then I get an error  
   - OK : And output contains `Invalid specified port`  
   - [X] scenario   [priviliged port](coap_server_tests.md) pass  


## Summary : **Success**, 8 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 8     |
| Empty      | 0     |
| Not Run    | 0     |

