Feature: Compile Time Information
  In order to allow a developer to have access to context
    information at compile time
  As a Joxa Developer
  I want to be able to call the functions (module) (function) (line) and have
    them evaluate to the correct result

  Scenario: Write a function that evaluates to the module name
    Given a module that has a function that calls module
    When joxa is called on this module
    Then a beam binary is produced
    And the described function returns the name of the module
