Feature: Joxa builds supports normal 'try' expressions
  In order to allow a developer to handle exceptions easily
  As an Joxa Developer
  I want to Joxa to support something like more normal trys

  Scenario: Build a try expression
    Given a module that has a function that contains 'try'
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
