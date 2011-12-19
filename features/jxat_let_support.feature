Feature: Joxa builds a module containing let bindings
  In order to allow a developer to create variables
  As an Joxa Developer
  I want to Joxa to support let bindings

  Scenario: Build a binding module
    Given a module that has a function that contains 'let'
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
