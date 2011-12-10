Feature: Joxa should be able to compile a module
  In order to allow a developer to compile a minimal module
  As an Joxa Developer
  I want to Joxa to be able to compile a form consisting of a single module

  Scenario: Allow a bare module
    Given a bare module
    When joxa is called on this module
    Then a beam binary is produced
    And the joxa context for a bare module is correctly formed
