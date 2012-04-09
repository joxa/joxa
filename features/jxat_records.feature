Feature: Lisp Style Records
  In order to allow a developer to define records of his own
  And interact easily with existing native erlang records
  As a Joxa Developer
  I want to Joxa to want to define records with a more intuitive syntax,
    be able to access those records in patterns and accessors and consume
    records defined in erlang.

  Scenario: Support records
    Given a module that has defined records
    And another module uses those records
    When joxa is called on these modules
    Then a beam binary is produced for both
    And the described function can be called and works correctly
