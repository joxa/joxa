Feature: Complex pattern matching in case clauses
  In order to allow a developer to make use of Erlang style pattern matching
  As a Joxa Developer
  I want to write a function that includes a case statement that destructures a tuple

  Scenario: Support pattern matching with case
    Given a module that has a case statement
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
