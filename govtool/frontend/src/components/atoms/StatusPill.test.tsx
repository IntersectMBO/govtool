import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";

import { DRepStatus } from "@/models";

import { StatusPill } from "./StatusPill";

describe("StatusPill", () => {
  it("shows an active DRep in brand blue", () => {
    render(<StatusPill status={DRepStatus.Active} />);
    expect(screen.getByTestId("Active-pill")).toHaveStyle({
      backgroundColor: "#EEF3FF",
      color: "#1447E6",
      borderRadius: "4px",
      height: "18px",
    });
  });

  it("shows an inactive DRep in red", () => {
    render(<StatusPill status={DRepStatus.Inactive} />);
    expect(screen.getByTestId("Inactive-pill")).toHaveStyle({
      backgroundColor: "#FEF3F2",
      color: "#D92D20",
    });
  });

  it("shows a retired DRep in gray", () => {
    render(<StatusPill status={DRepStatus.Retired} />);
    expect(screen.getByTestId("Retired-pill")).toHaveStyle({
      backgroundColor: "#F1F4F9",
      color: "#475467",
    });
  });
});
