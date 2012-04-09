Feature: Try exception handling expression
  In order to allow a developer to handle exceptions
  As a Joxa Developer
  I want to support to throw an exception that is
    successfully caught with a try expression

  Scenario: Build a try expression
    Given a module that has a function that contains 'try'
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
