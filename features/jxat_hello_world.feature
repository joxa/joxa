Feature: Function Calling
  In order to allow a developer to build a module
    that allows one function to call another
  As a Joxa Developer
  I want to write a function that calls another function

  Scenario: Build a hello world module
    Given a module that has a function that calls io:format on an argument
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
