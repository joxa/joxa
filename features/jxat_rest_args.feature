Feature: &rest Args
  In order to allow a developer have &rest args
  As a Joxa Developer
  I want to write a function that has &rest args,
   write a function that calls that function with variable arguments
   and have the callee recieve the 'list' of arguments as expected

  Scenario: Support varargs in joxa
    Given a module that has rest arguments
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
