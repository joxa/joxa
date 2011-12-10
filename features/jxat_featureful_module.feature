Feature: Joxa should be able to compile a module with use,require and attr clauses
  In order to allow a developer to compile a featureful module
  As an Joxa Developer
  I want to Joxa to be able to compile a form consisting of a single
  module with various clauses

  Scenario: Allow a bare module
    Given a featureful module
    When joxa is called on this module
    Then a beam binary is produced
    And the joxa context for a featureful module is correctly formed
