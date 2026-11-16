import { beforeEach, describe, expect, it } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import "@/i18n";

import { GovernanceActionHistorySearchFiltersSortBar } from "./GovernanceActionHistorySearchFiltersSortBar";

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
});
