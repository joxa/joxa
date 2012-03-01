Feature: Joxa should support gathering information from a module without compilation
  In order to allow a developer get information from a module
  As an Joxa Developer
  I want to Joxa to be able to parse a module and return context without compiling

  Scenario: Support getting context
    Given a module that has a require and use
    When joxa info is called on this module
    Then context is produced
    And context contains the required information

  Scenario: Support getting context in broken module
    Given a module that with a valid module but broken body
    When joxa info is called on this module
    Then context is produced
    And context contains the required information
