Feature: Joxa should the receive clause
  In order to allow a developer write code that receves messages
  As an Joxa Developer
  I want to Joxa to be able to receive messages

  Scenario: Support the receive expression
    Given a module that has a receive clause
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
