import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import "@/i18n";

import { GovernanceActionHistorySearchFiltersSortBar } from "./GovernanceActionHistorySearchFiltersSortBar";

const record = (type: string) => ({ type });
vi.mock("@/hooks/queries/useDRepDashboard", () => ({
  useDRepDashboardActions: () => ({
    actions: [record("InfoAction"), record("InfoAction"), record("TreasuryWithdrawals")],
  }),
}));

const renderBar = () =>
  render(
    <MemoryRouter initialEntries={["/governance_actions/history"]}>
      <GovernanceActionHistorySearchFiltersSortBar>
        <p>list</p>
      </GovernanceActionHistorySearchFiltersSortBar>
    </MemoryRouter>,
  );

const resize = (width: number) => {
  window.innerWidth = width;
  window.dispatchEvent(new Event("resize"));
};

describe("GovernanceActionHistorySearchFiltersSortBar", () => {
  beforeEach(() => resize(1440));

  it("shows one set of filters beside the list on wide screens", () => {
    renderBar();
    expect(screen.getAllByTestId("history-status-filter")).toHaveLength(1);
  });

  it("keeps a single set of filters on phones once Filters is open", () => {
    resize(375);
    renderBar();
    expect(screen.queryAllByTestId("history-status-filter")).toHaveLength(0);
    fireEvent.click(screen.getByTestId("filters-button"));
    expect(screen.getAllByTestId("history-status-filter")).toHaveLength(1);
  });

  it("puts the status checkboxes first and counts each action type", () => {
    renderBar();
    const types = screen.getByTestId("history-type-filter");
    // Status comes before the type card in the page.
    const order = [...document.querySelectorAll("[data-testid$='-filter']")].map(
      (element) => element.getAttribute("data-testid"),
    );
    expect(order).toEqual(["history-status-filter", "history-type-filter"]);
    expect(types).toBeInTheDocument();
    expect(screen.getByTestId("Expired-checkbox")).toHaveAttribute("role", "checkbox");
    expect(screen.queryByTestId("Live-checkbox")).toBeNull();
    expect(screen.getByTestId("AllGovernanceActions-checkbox")).toHaveTextContent("3");
    // Short labels on screen, the long ones in the ids the E2E specs click.
    expect(screen.getByTestId("InfoAction-checkbox")).toHaveTextContent("Info2");
    expect(screen.getByTestId("UpdateCommittee-checkbox")).toBeInTheDocument();
    expect(screen.getByTestId("Hard-ForkInitiation-checkbox")).toBeInTheDocument();
  });
});
