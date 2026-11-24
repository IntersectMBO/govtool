import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";

import "@/i18n";

import { GovernanceActionsListLayout } from "./GovernanceActionsListLayout";

vi.mock("@context", () => ({
  useDataActionsBar: () => ({
    searchText: "",
    setSearchText: vi.fn(),
    chosenFilters: [],
    setChosenFilters: vi.fn(),
    chosenSorting: "",
    setChosenSorting: vi.fn(),
  }),
  useFeatureFlag: () => ({
    governanceActionsSort: { isAvailable: false, options: [] },
  }),
}));

vi.mock("@hooks", async (importOriginal) => ({
  ...(await importOriginal<typeof import("@hooks")>()),
  useGetLiveProposalsQuery: () => ({ liveProposals: [], liveProposalsTotal: 0 }),
}));

const renderLayout = () =>
  render(
    <GovernanceActionsListLayout>
      <p>list</p>
    </GovernanceActionsListLayout>,
  );

describe("GovernanceActionsListLayout", () => {
  beforeEach(() => {
    window.innerWidth = 1440;
    window.dispatchEvent(new Event("resize"));
  });

  it("shows one type filter beside the list on wide screens", () => {
    renderLayout();
    expect(screen.getAllByTestId("InfoAction-checkbox")).toHaveLength(1);
  });

  it("keeps a single type filter on phones once Filters is open", () => {
    window.innerWidth = 375;
    window.dispatchEvent(new Event("resize"));
    renderLayout();
    expect(screen.queryAllByTestId("InfoAction-checkbox")).toHaveLength(0);
    fireEvent.click(screen.getByTestId("filters-button"));
    expect(screen.getAllByTestId("InfoAction-checkbox")).toHaveLength(1);
  });
});
