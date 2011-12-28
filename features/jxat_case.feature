Feature: Joxa should support complex pattern matching in case clauses
  In order to allow a developer to do reasonable development in Joxa
  As an Joxa Developer
  I want to Joxa to be able to compile pattern matching in case clauses

  Scenario: Support pattern matching with case
    Given a module that has a case statement
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
