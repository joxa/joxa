Feature: Joxa should support macros
  In order to allow a developer make use of semantic macros
  As an Joxa Developer
  I want to Joxa to be able to compile and use macros

  Scenario: Support macros in joxa with defmacro
    Given a module that contains macros
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
