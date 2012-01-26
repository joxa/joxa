Feature: Joxa should support variable arguments
  In order to allow a developer have varargs
  As an Joxa Developer
  I want to Joxa to be able to make use of var args

  Scenario: Support varargs in joxa
    Given a module that has rest arguments
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
