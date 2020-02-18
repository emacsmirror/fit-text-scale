Feature: See whole buffer in window

  Background:
    Given I am in empty buffer "*test"
    And buffer "*test" is current and empty

  Scenario: Scale text down to see all lines
    And I insert
    """
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    line
    """
    When I fit scale vertically
    Then I should see almost all lines
