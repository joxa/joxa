Feature: Support Let Bindings
  In order to allow the creation of scope bound variables
  As a Joxa Developer
  I want to create a function that contains 'let' bound variables
   and have that function run correctly.

  Scenario: Build a binding module
    Given a module that has a function that contains 'let'
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
