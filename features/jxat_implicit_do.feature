Feature: Multiple expressions in the body of let, defn and case (implicit 'do')
  In order to allow a developer to more succinctly write side effect
    using code easily
  As a Joxa Developer
  I want to write a function that has multiple expressions in the body of
     a let, defn or case statement.

  Scenario: Support implicit do
    Given a module that has an anonymous function
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
