Feature: Joxa should support type declarations
  In order to allow a developer to specify types
  and allow functions to be predeclared.
  As an Joxa Developer
  I want to Joxa to support specifying types and type specs

  Scenario: Support type specs
    Given a module that declares types
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
