Feature: Joxa should support mulitple expressions in the body of let, defn and case
  In order to allow a developer to do side effect code easily
  As an Joxa Developer
  I want to Joxa to be able have mulitple expressions in bodies without having a do

  Scenario: Support implicit do
    Given a module that has an anonymous function
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
