Feature: Macro Support
  In order to allow a developer make use of semantic macros
  As a Joxa Developer
  I want to create a Macro and a function that calls that macro and
    have the function be correctly built and run using the contents
    generated from the macro

  Scenario: Support macros in joxa with defmacro
    Given a module that contains macros
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly

  Scenario: Support reporting macro errors in defmacro
    Given a module that contains a macro that errors
    When joxa is called on this module
    Then an error is produced
    And that error is in the error list
