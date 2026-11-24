import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";

import { GovernanceActionStatusChip } from "./GovernanceActionStatusChip";

const chipOf = (label: string) =>
  screen.getByText(label).closest(".MuiChip-root") as HTMLElement;

describe("GovernanceActionStatusChip", () => {
  it("shows an expired action in red", () => {
    render(<GovernanceActionStatusChip label="Expired" />);
    expect(chipOf("Expired")).toHaveStyle({
      backgroundColor: "#FEF3F2",
      borderRadius: "6px",
    });
    expect(screen.getByText("Expired")).toHaveStyle({ color: "#B42318" });
  });

  it("shows an enacted action in blue", () => {
    render(<GovernanceActionStatusChip label="Enacted" />);
    expect(chipOf("Enacted")).toHaveStyle({ backgroundColor: "#EEF3FF" });
  });

  it("shows an in-progress action in green", () => {
    render(<GovernanceActionStatusChip label="In Progress" />);
    expect(chipOf("In Progress")).toHaveStyle({ backgroundColor: "#E3F8EA" });
  });

  it("shows other labels in gray", () => {
    render(<GovernanceActionStatusChip label="Treasury Withdrawal" />);
    expect(chipOf("Treasury Withdrawal")).toHaveStyle({
      backgroundColor: "#F1F4F9",
    });
  });
});
