Feature: Joxa should support records in the style of lisp
  In order to allow a developer to have records and interact easily with erlang records
  As an Joxa Developer
  I want to Joxa to support record definition and manipulation

  Scenario: Support records
    Given a module that has defined records
    And another module uses those records
    When joxa is called on these modules
    Then a beam binary is produced for both
    And the described function can be called and works correctly
