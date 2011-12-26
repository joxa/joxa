Feature: Joxa should support exceptions
  In order to allow a developer to use exceptions
  And interact with normal erlang systems
  As an Joxa Developer
  I want to Joxa to support catching exceptions

  Scenario: Support exceptions
    Given a module that catches an exception
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
