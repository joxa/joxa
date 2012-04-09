Feature: Binary patterns in case clauses
  In order to allow a developer binary parsing and Erlang style bit syntax
  As a Joxa Developer
  I want to write a function that contains a case, that does binary destructuring

  Scenario: Support binary representations
    Given a module that has a binary representatino
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
