import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import { MemoryRouter } from "react-router";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";

import "@/i18n";
import { getGovActionCIP129Id } from "@/utils/getGovActionId";

import { GlobalSearchButton } from "./GlobalSearchButton";

const mocks = vi.hoisted(() => ({
  navigate: vi.fn(),
  addSuccessAlert: vi.fn(),
  forum: true,
  search: {} as Record<string, unknown>,
}));

vi.mock("react-router", async (importOriginal) => ({
  ...(await importOriginal<typeof import("react-router")>()),
  useNavigate: () => mocks.navigate,
}));

vi.mock("@context", () => ({
  useCardano: () => ({ isEnabled: false, stakeKey: undefined }),
  useFeatureFlag: () => ({ isProposalDiscussionForumEnabled: mocks.forum }),
  useSnackbar: () => ({ addSuccessAlert: mocks.addSuccessAlert }),
}));

vi.mock("@hooks", async (importOriginal) => ({
  ...(await importOriginal<typeof import("@hooks")>()),
  useDebounce: (value: unknown) => value,
}));

vi.mock("@/hooks/queries/useGetGovernanceActionMetadata", () => ({
  useGetGovernanceActionMetadata: () => ({ metadata: undefined }),
}));

const category = (over: Record<string, unknown> = {}) => ({
  items: [],
  total: 0,
  hasMore: false,
  isEnabled: true,
  isLoading: false,
  isError: false,
  isSearchNotReady: false,
  isFetchingMore: false,
  loadMore: vi.fn(),
  retry: vi.fn(),
  ...over,
});

const action = (n: number) => ({
  txHash: `${n}`.padStart(64, "a"),
  index: 0,
  type: "InfoAction",
  title: `Treasury action ${n}`,
  abstract: "An abstract",
  createdDate: "2026-09-28T10:00:00Z",
});

const dRep = (n: number) => ({
  view: `drep1${"x".repeat(50)}${n}`,
  givenName: `DRep ${n}`,
  objectives: "Objectives",
  votingPower: 224_600_000_000_000,
  status: "Active",
});

const searchFor = (query: string) => {
  const text = query.trim();
  const isSearching = text.length >= 2;
  const off = category({ isEnabled: false });
  return {
    classified: { text, kind: "keyword" },
    isSearching,
    actions: isSearching ? category() : off,
    dreps: isSearching ? category() : off,
    proposals: isSearching ? category() : off,
    outcomes: isSearching ? category() : off,
    addresses: off,
    handles: off,
    ...(isSearching ? mocks.search : {}),
  };
};

vi.mock("@/hooks/queries/useGlobalSearch", () => ({
  useGlobalSearch: (query: string) => searchFor(query),
}));

const renderButton = () =>
  render(
    <QueryClientProvider client={new QueryClient()}>
      <MemoryRouter>
        <GlobalSearchButton />
      </MemoryRouter>
    </QueryClientProvider>,
  );

const openDialog = () => {
  renderButton();
  fireEvent.click(screen.getByTestId("global-search-button"));
  return screen.getByTestId("global-search-input");
};

describe("GlobalSearch", () => {
  beforeEach(() => {
    localStorage.clear();
    mocks.navigate.mockReset();
    mocks.addSuccessAlert.mockReset();
    mocks.forum = true;
    mocks.search = {};
  });

  it("opens from the header button and from Ctrl+K", () => {
    renderButton();
    expect(screen.queryByTestId("global-search-dialog")).toBeNull();
    fireEvent.keyDown(window, { key: "k", ctrlKey: true });
    expect(screen.getByTestId("global-search-dialog")).toBeInTheDocument();
    expect(screen.getByRole("dialog", { name: "Find anything in GovTool" })).toBeInTheDocument();
  });

  it("starts on browse by type and opens a type's page", () => {
    openDialog();
    expect(screen.getByText("Browse by type")).toBeInTheDocument();
    fireEvent.click(screen.getByTestId("global-search-browse-dreps"));
    expect(mocks.navigate).toHaveBeenCalledWith("/dreps");
  });

  it("leaves Proposals out while the forum is off", () => {
    mocks.forum = false;
    openDialog();
    expect(screen.queryByTestId("global-search-browse-proposals")).toBeNull();
  });

  it("shows the first category with results and switches with the chips", () => {
    mocks.search = {
      dreps: category({ items: [dRep(1), dRep(2)], total: 2 }),
      outcomes: category({ items: [], total: undefined }),
    };
    const input = openDialog();
    fireEvent.change(input, { target: { value: "treasury" } });
    expect(screen.getByTestId("global-search-category-dreps")).toHaveAttribute(
      "aria-pressed",
      "true",
    );
    expect(screen.getAllByTestId("global-search-result")).toHaveLength(2);
    expect(screen.getByTestId("global-search-count")).toHaveTextContent("2 matches");
    fireEvent.click(screen.getByTestId("global-search-category-actions"));
    expect(screen.getByTestId("global-search-no-matches")).toHaveTextContent(
      "No matches for “treasury”",
    );
  });

  it("moves the highlight with the arrow keys and opens it with Enter", async () => {
    mocks.search = {
      actions: category({ items: [action(1), action(2)], total: 7, hasMore: true }),
    };
    const input = openDialog();
    fireEvent.change(input, { target: { value: "treasury" } });
    expect(input).toHaveAttribute("aria-activedescendant", "global-search-option-0");
    fireEvent.keyDown(input, { key: "ArrowDown" });
    expect(input).toHaveAttribute("aria-activedescendant", "global-search-option-1");
    expect(screen.getAllByRole("option")[1]).toHaveAttribute("aria-selected", "true");
    fireEvent.keyDown(input, { key: "Enter" });
    expect(mocks.navigate).toHaveBeenCalledWith(
      `/actions/${getGovActionCIP129Id("2".padStart(64, "a"), 0)}`,
      expect.objectContaining({ state: expect.objectContaining({ proposal: action(2) }) }),
    );
    // The dialog closes, and the query is remembered for next time.
    await waitFor(() => expect(screen.queryByTestId("global-search-dialog")).toBeNull());
    fireEvent.click(screen.getByTestId("global-search-button"));
    expect(screen.getByTestId("global-search-recent")).toHaveTextContent("treasury");
  });

  it("loads more of a category", () => {
    const loadMore = vi.fn();
    mocks.search = { actions: category({ items: [action(1)], total: 7, hasMore: true, loadMore }) };
    fireEvent.change(openDialog(), { target: { value: "treasury" } });
    fireEvent.click(screen.getByTestId("global-search-load-more"));
    expect(loadMore).toHaveBeenCalled();
  });

  it("says when every category failed and retries them", () => {
    const retry = vi.fn();
    const failed = category({ isError: true, retry });
    mocks.search = { actions: failed, dreps: failed, proposals: failed, outcomes: failed };
    fireEvent.change(openDialog(), { target: { value: "treasury" } });
    const panel = screen.getByTestId("global-search-error");
    expect(panel).toHaveTextContent("Search is temporarily unavailable");
    fireEvent.click(within(panel).getByTestId("global-search-try-again"));
    expect(retry).toHaveBeenCalledTimes(4);
  });

  it("shows a loading banner and skeletons while the category loads", () => {
    mocks.search = { actions: category({ isLoading: true, total: undefined }) };
    fireEvent.change(openDialog(), { target: { value: "treasury" } });
    expect(screen.getByText("Searching across 4 categories for “treasury”…")).toBeInTheDocument();
  });

  it("keeps loading rather than saying no matches while another category loads", () => {
    mocks.search = { handles: category({ isLoading: true, total: undefined }) };
    fireEvent.change(openDialog(), { target: { value: "treasury" } });
    expect(screen.queryByTestId("global-search-no-matches")).toBeNull();
    expect(screen.getByText("Searching across 5 categories for “treasury”…")).toBeInTheDocument();
  });

  it("keeps the tab it showed first when a slower type brings results", () => {
    mocks.search = { dreps: category({ isLoading: true }) };
    mocks.search.outcomes = category({
      items: [{
        id: "o1",
        tx_hash: "aa",
        index: 0,
        type: "InfoAction",
        title: "Old outcome",
        status: {
          ratified_epoch: 1,
          enacted_epoch: null,
          dropped_epoch: null,
          expired_epoch: null,
        },
        status_times: {
          ratified_time: "2026-09-01T00:00:00Z",
          enacted_time: null,
          dropped_time: null,
          expired_time: null,
        },
      }],
      total: undefined,
    });
    const input = openDialog();
    fireEvent.change(input, { target: { value: "treasury" } });
    expect(screen.getByTestId("global-search-category-outcomes")).toHaveAttribute(
      "aria-pressed",
      "true",
    );
    mocks.search.dreps = category({ items: [dRep(1)], total: 1 });
    // Same search text, new render: the slower type has arrived.
    fireEvent.change(input, { target: { value: "treasury " } });
    expect(screen.getByTestId("global-search-category-outcomes")).toHaveAttribute(
      "aria-pressed",
      "true",
    );
  });

  it("says search is unavailable when every type that can fail has failed", () => {
    const failed = category({ isError: true });
    mocks.search = {
      actions: failed,
      dreps: failed,
      proposals: failed,
      outcomes: failed,
      handles: category({ items: [], total: 0 }),
    };
    fireEvent.change(openDialog(), { target: { value: "treasury" } });
    expect(screen.getByTestId("global-search-error")).toBeInTheDocument();
  });

  it("points the field at the list only while the list is shown", () => {
    mocks.search = { actions: category({ isLoading: true, total: undefined }) };
    const input = openDialog();
    expect(input).not.toHaveAttribute("aria-controls");
    fireEvent.change(input, { target: { value: "treasury" } });
    expect(input).not.toHaveAttribute("aria-activedescendant");
    expect(input).not.toHaveAttribute("aria-controls");
  });

  it("ignores Ctrl+Shift+K and Ctrl+K while another dialog is open", () => {
    renderButton();
    fireEvent.keyDown(window, { key: "K", ctrlKey: true, shiftKey: true });
    expect(screen.queryByTestId("global-search-dialog")).toBeNull();
    const other = document.createElement("div");
    other.setAttribute("role", "dialog");
    document.body.appendChild(other);
    fireEvent.keyDown(window, { key: "k", ctrlKey: true });
    expect(screen.queryByTestId("global-search-dialog")).toBeNull();
    other.remove();
  });

  it("explains the address and handle tabs for a keyword", () => {
    fireEvent.change(openDialog(), { target: { value: "treasury committee" } });
    fireEvent.click(screen.getByTestId("global-search-category-addresses"));
    expect(screen.getByTestId("global-search-category-hint")).toHaveTextContent(
      "Paste a full stake1… or addr1… address.",
    );
  });

  it("copies an address that delegates to no DRep", () => {
    const original = navigator.clipboard;
    Object.assign(navigator, { clipboard: { writeText: vi.fn() } });
    mocks.search = {
      classified: { text: "stake1uabc", kind: "address", stakeAddress: "stake1uabc" },
      addresses: category({
        items: [{ stakeAddress: "stake1uabc", isRegistered: true, delegation: null, votingPower: 0 }],
        total: 1,
      }),
      actions: category({ isEnabled: false }),
      dreps: category({ isEnabled: false }),
      proposals: category({ isEnabled: false }),
      outcomes: category({ isEnabled: false }),
    };
    fireEvent.change(openDialog(), { target: { value: "stake1uabc" } });
    fireEvent.click(screen.getByTestId("global-search-result"));
    expect(navigator.clipboard.writeText).toHaveBeenCalledWith("stake1uabc");
    expect(mocks.addSuccessAlert).toHaveBeenCalled();
    Object.assign(navigator, { clipboard: original });
  });
});
