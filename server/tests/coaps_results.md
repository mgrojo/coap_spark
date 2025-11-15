
# Document: [coaps_server_tests.md](coaps_server_tests.md)  
  ## Feature: CoAP secure communication with server  
   ### Scenario: [get method with test path and high verbosity I](coaps_server_tests.md): 
   - OK : When I run `coap_client -v 4 -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/`  
   - OK : Then I get no error  
   - OK : And the output contains   
   - [X] scenario   [get method with test path and high verbosity I](coaps_server_tests.md) pass  

   ### Scenario: [get method without path](coaps_server_tests.md): 
   - OK : When I run `coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost`  
   - OK : Then I get no error  
   - OK : And output contains `This is a test server made with`  
   - [X] scenario   [get method without path](coaps_server_tests.md) pass  

   ### Scenario: [get method with empty path](coaps_server_tests.md): 
   - OK : When I run `coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/`  
   - OK : Then I get no error  
   - OK : And output contains `This is a test server made with`  
   - [X] scenario   [get method with empty path](coaps_server_tests.md) pass  

   ### Scenario: [get method with port and no path](coaps_server_tests.md): 
   - OK : When I run `coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost:5684`  
   - OK : Then I get no error  
   - OK : And output contains `This is a test server made with`  
   - [X] scenario   [get method with port and no path](coaps_server_tests.md) pass  

   ### Scenario: [post method with path](coaps_server_tests.md): 
   - OK : When I run `coap_client -m post -e "New resource" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`  
   - OK : Then I get no error  
   - [X] scenario   [post method with path](coaps_server_tests.md) pass  

   ### Scenario: [explicit get method with path](coaps_server_tests.md): 
   - OK : When I run `coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`  
   - OK : Then I get no error  
   - OK : And output contains `New resource`  
   - [X] scenario   [explicit get method with path](coaps_server_tests.md) pass  

   ### Scenario: [explicit get method with IP and path](coaps_server_tests.md): 
   - OK : When I run `coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://127.0.0.1/`  
   - OK : Then I get no error  
   - OK : And output contains `This is a test server made with`  
   - [X] scenario   [explicit get method with IP and path](coaps_server_tests.md) pass  

   ### Scenario: [get method with port and path](coaps_server_tests.md): 
   - OK : When I run `coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost:5684/test`  
   - OK : Then I get no error  
   - OK : And output contains `New resource`  
   - [X] scenario   [get method with port and path](coaps_server_tests.md) pass  

   ### Scenario: [post method within previous resource](coaps_server_tests.md): 
   - OK : When I run `coap_client -m post -e "New resource inside another" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test/inner`  
   - OK : Then I get no error  
   - [X] scenario   [post method within previous resource](coaps_server_tests.md) pass  

   ### Scenario: [get method with nested resource](coaps_server_tests.md): 
   - OK : When I run `coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test/inner`  
   - OK : Then I get no error  
   - OK : And output contains `New resource inside another`  
   - [X] scenario   [get method with nested resource](coaps_server_tests.md) pass  

   ### Scenario: [get non-existent resource with path](coaps_server_tests.md): 
   - OK : When I run `coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/this/does/not/exist`  
   - OK : Then I get no error  
   - OK : And the output is  
   - [X] scenario   [get non-existent resource with path](coaps_server_tests.md) pass  

   ### Scenario: [post on long path](coaps_server_tests.md): 
   - OK : When I run `coap_client -m post -e "very long resource name" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/123412341234123412341234`  
   - OK : Then I get no error  
   - [X] scenario   [post on long path](coaps_server_tests.md) pass  

   ### Scenario: [get "123412341234123412341234" path](coaps_server_tests.md): 
   - OK : When I run `coap_client -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/123412341234123412341234`  
   - OK : Then I get no error  
   - OK : And the output contains  
   - [X] scenario   [get "123412341234123412341234" path](coaps_server_tests.md) pass  

   ### Scenario: [explicit get with error 4.04](coaps_server_tests.md): 
   - OK : When I run `coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/hola`  
   - OK : Then I get no error  
   - OK : And output is  
   - [X] scenario   [explicit get with error 4.04](coaps_server_tests.md) pass  

   ### Scenario: [post method error 4.05](coaps_server_tests.md): 
   - OK : When I run `coap_client -m patch -e "This is a test" -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`  
   - OK : Then I get no error  
   - OK : And output is  
   - [X] scenario   [post method error 4.05](coaps_server_tests.md) pass  

   ### Scenario: [delete method with path](coaps_server_tests.md): 
   - OK : When I run `coap_client -m delete -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`  
   - OK : Then I get no error  
   - [X] scenario   [delete method with path](coaps_server_tests.md) pass  

   ### Scenario: [explicit get method with path of just deleted resource](coaps_server_tests.md): 
   - OK : When I run `coap_client -m get -k COAP_SPARK_KEY_5684 -u coap_spark coaps://localhost/test`  
   - OK : Then I get no error  
   - OK : And the output is  
   - [X] scenario   [explicit get method with path of just deleted resource](coaps_server_tests.md) pass  


## Summary : **Success**, 17 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 17    |
| Empty      | 0     |
| Not Run    | 0     |

