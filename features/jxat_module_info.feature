Feature: Module Introspection
  In order to allow Joxa modules to advertise thier features
  As a Joxa Developer
  I want to create a module, and then query that module about what
   macros, and &rest arguments it support

  Scenario: Support getting context
    Given a module that has a require and use
    When joxa info is called on this module
    Then context is produced
    And context contains the required information
