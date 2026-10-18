import { render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";
import { describe, expect, it, vi } from "vitest";

import { GovernanceActionHistoryList } from "./GovernanceActionHistoryList";

const history = vi.fn();

vi.mock("@consts", () => ({ GOV_ACTION_HISTORY_ITEMS_PER_PAGE: 12 }));
vi.mock("@atoms", () => ({
  Button: ({ children }: { children?: React.ReactNode }) => (
    <button type="button">{children}</button>
  ),
}));

vi.mock("@hooks", () => ({
  useGetGovernanceActionHistoryQuery: () => history(),
  useTranslation: () => ({ t: (key: string) => key }),
}));

vi.mock("@molecules", () => ({
  GovernanceActionHistoryCard: () => <p>card</p>,
  GovernanceActionHistoryEmptyState: ({ title }: { title: string }) => (
    <p>{title}</p>
  ),
  SearchNotReady: () => <p>search not ready</p>,
}));

const query = (overrides: Record<string, unknown>) => ({
  govActions: undefined,
  isGovActionsLoading: false,
  isSearchNotReady: false,
  fetchNextPage: vi.fn(),
  hasNextPage: false,
  isFetchingNextPage: false,
  ...overrides,
});

const renderList = () =>
  render(
    <MemoryRouter initialEntries={["/?q=treasury"]}>
      <GovernanceActionHistoryList />
    </MemoryRouter>,
  );

describe("GovernanceActionHistoryList", () => {
  it("says search is not ready rather than that nothing matched", () => {
    history.mockReturnValue(query({ isSearchNotReady: true }));
    renderList();

    expect(screen.getByText("search not ready")).toBeVisible();
    expect(
      screen.queryByText("governanceActionHistoryList.noResults.title"),
    ).toBeNull();
  });

  it("shows the empty state when the search ran and matched nothing", () => {
    history.mockReturnValue(query({ govActions: { pages: [[]] } }));
    renderList();

    expect(
      screen.getByText("governanceActionHistoryList.noResults.title"),
    ).toBeVisible();
    expect(screen.queryByText("search not ready")).toBeNull();
  });
});
