Feature: Joxa should support anonymous functions
  In order to allow a developer to do functional composition
  As a Joxa Developer
  I want to be write a function that declares and uses an
   unnamed anonymous function

  Scenario: Support anonymous functions
    Given a module that has an anonymous function
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
