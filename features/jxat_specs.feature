Feature: Type declarations
  In order to be able to document and define types
  and allow functions to be pre-declared
  As a Joxa Developer
  I want to specify a type in a function and have the compiler
   correctly compile the code into a form usable by ERTS

  Scenario: Support type specs
    Given a module that declares types
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
