Feature: Throwing Exceptions
  In order to allow a developer to raise exceptions for exceptional conditions
  And interact with normal exception throwing of erlang systems
  As a Joxa Developer
  I want to throw an exception that successfully propogates
    up the stack to the test handling code

  Scenario: Support exceptions
    Given a module that catches an exception
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
