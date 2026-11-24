import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";

import { GovernanceActionCardStatePill } from "./GovernanceActionCardStatePill";

describe("GovernanceActionCardStatePill", () => {
  it("shows a submitted vote in green", () => {
    render(<GovernanceActionCardStatePill variant="voteSubmitted" />);
    expect(screen.getByTestId("action-state-pill")).toHaveStyle({
      backgroundColor: "#E3F8EA",
      borderRadius: "6px",
    });
  });

  it("shows an in-progress vote in amber", () => {
    render(<GovernanceActionCardStatePill variant="inProgress" />);
    expect(screen.getByTestId("action-state-pill")).toHaveStyle({
      backgroundColor: "#FFFAEB",
    });
  });
});
