Feature: Joxa should support contains anonymous functions
  In order to allow a developer to do functional composition
  As an Joxa Developer
  I want to Joxa to be able to compile anonymous functions

  Scenario: Support anonymous functions
    Given a module that has an anonymous function
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
