Feature: Module 'Visibility' Management
  In order to allow a developer to build a module
    that correctly manages internal visibilty
  As a Joxa Developer
  I want to Joxa to be able to compile a form consisting of a single
  module that uses at least one 'use', one 'require' and one 'attr' clause

  Scenario: Allow a bare module
    Given a featureful module
    When joxa is called on this module
    Then a beam binary is produced
    And the joxa context for a featureful module is correctly formed
