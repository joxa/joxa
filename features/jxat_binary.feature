Feature: Joxa should support binary patterns in case clauses
  In order to allow a developer to support interesting binary parsing
  As an Joxa Developer
  I want to Joxa to be able to have binary representations in a pattern

  Scenario: Support binary representations
    Given a module that has a binary representatino
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
