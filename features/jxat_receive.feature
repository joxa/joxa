Feature: Message Receive Expression
  In order to support receiving messages sent to a process (a critical
    feature in Erlang or Joxa)
  As a Joxa Developer
  I want Joxa to create to create two processes, one of which
     sends a message to the other, which succesfully receives that message

  Scenario: Support the receive expression
    Given a module that has a receive clause
    When joxa is called on this module
    Then a beam binary is produced
    And the described function can be called and works correctly
