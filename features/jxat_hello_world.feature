Feature: Joxa should be able to build a module that does some minimal thing
  In order to allow a developer to build a module that does anything at all
  As an Joxa Developer
  I want to Joxa to be able to a function that calls another function

  Scenario: Build a hello world module
    Given a module that has a function that calls io:format on an argument
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
