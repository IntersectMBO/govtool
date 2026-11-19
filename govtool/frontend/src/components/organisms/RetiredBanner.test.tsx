import { describe, expect, it } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import "@/i18n";

import { RetiredBanner } from "./RetiredBanner";

describe("RetiredBanner", () => {
  it("confirms a direct voter retirement until dismissed", () => {
    render(
      <MemoryRouter initialEntries={[{ pathname: "/dashboard", state: { retired: "directVoter" } }]}>
        <RetiredBanner />
      </MemoryRouter>,
    );
    expect(screen.getByRole("status")).toHaveTextContent("You have successfully retired as a Direct Voter");
    fireEvent.click(screen.getByTestId("retired-banner-dismiss"));
    expect(screen.queryByRole("status")).toBeNull();
  });

  it("shows nothing otherwise", () => {
    render(<MemoryRouter initialEntries={["/dashboard"]}><RetiredBanner /></MemoryRouter>);
    expect(screen.queryByRole("status")).toBeNull();
  });
});
