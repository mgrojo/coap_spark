# Black box tests for CoAP Client
These are the test scenarios for `coap_server` to be run using the `bbt` tool. They test cases where the server processes the command line and terminates.

You can install `bbt` with `alr install bbt`.

Use this command to run the tests: `bbt coap_server_tests.md`

## Feature: CoAP Client command line

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
