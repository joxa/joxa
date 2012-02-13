Feature: Joxa should support predicate operations
  In order to allow a developer reasonable predicates aside from case
  As an Joxa Developer
  I want to Joxa to support if, when, and unless

  Scenario: Support predicate operations
    Given a module that has a predicates
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
