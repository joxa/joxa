Feature: Concurrent Compiler
  In order to have short compile times
  As a Joxa developer
  I want to have a concurrent compiler

  Scenario: Use the concurrent compiler
    Given a bunch of joxa source files
    When the concurrent compiler is called with these files
    Then all files are compiled successfully
