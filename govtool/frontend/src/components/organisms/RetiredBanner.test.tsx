import { describe, expect, it } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
import { MemoryRouter, useLocation } from "react-router";

import "@/i18n";

import { RetiredBanner } from "./RetiredBanner";

const StateProbe = () => <div data-testid="route-state">{JSON.stringify(useLocation().state)}</div>;

describe("RetiredBanner", () => {
  it("confirms a direct voter retirement until dismissed", () => {
    render(
      <MemoryRouter initialEntries={[{ pathname: "/dashboard", state: { retired: "directVoter" } }]}>
        <RetiredBanner />
        <StateProbe />
      </MemoryRouter>,
    );
    expect(screen.getByRole("status")).toHaveTextContent("You have successfully retired as a Direct Voter");
    fireEvent.click(screen.getByTestId("retired-banner-dismiss"));
    expect(screen.queryByRole("status")).toBeNull();
    // Cleared from the history entry, so a reload or Back does not bring it back.
    expect(screen.getByTestId("route-state")).toHaveTextContent("null");
  });

  it("shows nothing otherwise", () => {
    render(<MemoryRouter initialEntries={["/dashboard"]}><RetiredBanner /></MemoryRouter>);
    expect(screen.queryByRole("status")).toBeNull();
  });
});
