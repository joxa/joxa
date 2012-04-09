Feature: Conditionals
  In order to allow more expressive conditional syntax
  As a Joxa Developer
  I want to write a function that has an if, a when, and an 'unless'
  And have that function correctly execute based on the conditional
    defined

  Scenario: Support predicate operations
    Given a module that has a predicates
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
