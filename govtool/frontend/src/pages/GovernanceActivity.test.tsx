import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen, within } from "@testing-library/react";
import { MemoryRouter } from "react-router";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";

import "@/i18n";

import { GovernanceActivity } from "./GovernanceActivity";

const DAY = 24 * 60 * 60 * 1000;
const NOW = Date.now();
const daysAgo = (days: number) => new Date(NOW - days * DAY).toISOString();
const daysAhead = (days: number) => new Date(NOW + days * DAY).toISOString();

const mocks = vi.hoisted(() => ({
  liveProposals: [] as unknown[],
  liveTotal: undefined as number | undefined,
  decidedActions: [] as unknown[],
  pageArgs: [] as Record<string, unknown>[],
  pageTotal: 0,
}));

vi.mock("@context", () => ({
  useCardano: () => ({ isEnabled: false, stakeKey: undefined, pendingTransaction: {} }),
  useModal: () => ({ openModal: vi.fn() }),
  useFeatureFlag: () => ({ isProposalDiscussionForumEnabled: true }),
  useChatwoot: () => ({ openFeedbackWindow: vi.fn() }),
  useAppContext: () => ({ epochParams: { epoch_no: 660 } }),
}));

vi.mock("@hooks", async (importOriginal) => ({
  ...(await importOriginal<typeof import("@hooks")>()),
  useDebounce: (value: unknown) => value,
  useGetLiveProposalsQuery: () => ({
    liveProposals: mocks.liveProposals,
    liveProposalsTotal: mocks.liveTotal ?? mocks.liveProposals.length,
    areLiveProposalsLoading: false,
    liveProposalsUpdatedAt: NOW,
  }),
  useGetDecidedActionsQuery: () => ({
    decidedActions: mocks.decidedActions,
    areDecidedActionsLoading: false,
  }),
  useGetProposalsPageQuery: (args: Record<string, unknown>) => {
    mocks.pageArgs.push(args);
    const page = args.page as number;
    return {
      proposals: mocks.liveProposals.slice((page - 1) * 5, page * 5),
      proposalsTotal: mocks.pageTotal,
      isProposalsLoading: false,
      isSearchNotReady: false,
    };
  },
  useGetGovernanceActionMetadata: () => ({ metadata: undefined }),
}));

vi.mock("@services", async (importOriginal) => ({
  ...(await importOriginal<typeof import("@services")>()),
  getGovernanceActionRecord: vi.fn(async () => ({
    vote_aggregates: [
      {
        role: "drep",
        representation: "stake",
        yes: "720",
        no: "280",
        abstain: "0",
        notVoted: "0",
        totalEligible: "1000",
        threshold: { numerator: 67, denominator: 100 },
      },
    ],
  })),
}));

const proposal = (n: number, over: Record<string, unknown> = {}) => ({
  id: `p${n}`,
  txHash: `${n}`.padStart(64, "a"),
  index: 0,
  type: "InfoAction",
  title: `Live action ${n}`,
  createdDate: daysAgo(n),
  expiryDate: daysAhead(10),
  ...over,
});

const decided = (
  n: number,
  times: Record<string, string | null>,
  over: Record<string, unknown> = {},
) => ({
  id: `d${n}`,
  tx_hash: `${n}`.padStart(64, "b"),
  index: 0,
  type: "ParameterChange",
  title: `Decided action ${n}`,
  url: "",
  data_hash: "",
  status: {
    ratified_epoch: times.ratified_time ? 650 : null,
    enacted_epoch: times.enacted_time ? 651 : null,
    dropped_epoch: null,
    expired_epoch: times.expired_time ? 652 : null,
  },
  status_times: {
    ratified_time: null,
    enacted_time: null,
    dropped_time: null,
    expired_time: null,
    ...times,
  },
  ...over,
});

const renderPage = () =>
  render(
    <QueryClientProvider client={new QueryClient()}>
      <MemoryRouter>
        <GovernanceActivity />
      </MemoryRouter>
    </QueryClientProvider>,
  );

describe("GovernanceActivity", () => {
  beforeEach(() => {
    window.innerWidth = 1440;
    mocks.liveTotal = undefined;
    mocks.pageArgs = [];
    mocks.liveProposals = [
      proposal(1, { expiryDate: daysAhead(2.5), type: "TreasuryWithdrawals" }),
      proposal(2, { expiryDate: daysAhead(5.5) }),
      proposal(3),
    ];
    mocks.pageTotal = 3;
    mocks.decidedActions = [
      decided(1, { ratified_time: daysAgo(3.5), enacted_time: daysAgo(1.5) }),
      decided(2, { ratified_time: daysAgo(2.5) }),
      decided(3, { expired_time: daysAgo(4.5) }),
    ];
  });

  it("titles the page and shows each section", () => {
    renderPage();
    expect(screen.getByRole("heading", { level: 1 })).toHaveTextContent(
      "Governance Activity",
    );
    [
      "Governance overview",
      "What’s happening now",
      "Recently ratified",
      "Active Governance Actions",
    ].forEach((name) =>
      expect(screen.getByRole("heading", { level: 2, name })).toBeInTheDocument(),
    );
    expect(screen.getByTestId("activity-updated")).toHaveTextContent(
      "Updated just now",
    );
  });

  it("shows the overview figures from the live and decided lists", () => {
    renderPage();
    expect(screen.getByTestId("activity-stat-open-value")).toHaveTextContent("3");
    expect(screen.getByTestId("activity-stat-deadlines-value")).toHaveTextContent("2");
    expect(screen.getByTestId("activity-stat-ratified-value")).toHaveTextContent("2");
    expect(screen.getByTestId("activity-stat-enacted-value")).toHaveTextContent("1");
  });

  it("does not guess the deadline count when more actions are open than fetched", () => {
    mocks.liveTotal = 60;
    renderPage();
    const value = screen.getByTestId("activity-stat-deadlines-value");
    expect(value).toHaveTextContent("—");
    expect(value).toHaveAccessibleName(/more than 50 actions/);
  });

  it("does not claim exact ratification counts from a full, recent decided list", () => {
    mocks.decidedActions = Array.from({ length: 20 }, (_, n) =>
      decided(n + 10, { ratified_time: daysAgo(1), enacted_time: daysAgo(0.5) }),
    );
    renderPage();
    const value = screen.getByTestId("activity-stat-ratified-value");
    expect(value).toHaveTextContent("—");
    expect(value).toHaveAccessibleName(/more decisions/);
    expect(screen.getByTestId("activity-stat-enacted-value")).toHaveTextContent("—");
  });

  it("lists openings and decisions in the feed, newest first", () => {
    renderPage();
    const events = screen.getAllByTestId("activity-event");
    expect(events.map((event) => within(event).getByRole("heading").textContent)).toEqual([
      "Live action 1",
      "Decided action 1",
      "Live action 2",
      "Decided action 2",
      "Live action 3",
      "Decided action 1",
      "Decided action 3",
    ]);
    expect(within(events[1]).getByText("Enacted")).toBeInTheDocument();
  });

  it("reveals earlier activity ten rows at a time", () => {
    mocks.liveProposals = Array.from({ length: 14 }, (_, n) => proposal(n + 1));
    mocks.decidedActions = [];
    renderPage();
    expect(screen.getAllByTestId("activity-event")).toHaveLength(10);
    fireEvent.click(screen.getByTestId("load-earlier-activity"));
    expect(screen.getAllByTestId("activity-event")).toHaveLength(14);
    expect(screen.queryByTestId("load-earlier-activity")).toBeNull();
    // Focus moves to the first revealed row, not to the page.
    expect(
      within(screen.getAllByTestId("activity-event")[10]).getByRole("link"),
    ).toHaveFocus();
  });

  it("shows the latest ratifications with their decision date", () => {
    renderPage();
    const rows = screen.getAllByTestId("ratified-row");
    expect(rows).toHaveLength(2);
    expect(within(rows[0]).getByRole("heading")).toHaveTextContent("Decided action 2");
    expect(within(rows[1]).getAllByText("Enacted")).toHaveLength(1);
  });

  it("filters the active list by type and counts each type", () => {
    renderPage();
    expect(screen.getByTestId("activity-type-all")).toHaveTextContent("All3");
    const treasury = screen.getByTestId("activity-type-TreasuryWithdrawals");
    expect(treasury).toHaveTextContent("1");
    fireEvent.click(treasury);
    expect(treasury).toHaveAttribute("aria-pressed", "true");
    expect(mocks.pageArgs.at(-1)).toMatchObject({
      filters: ["TreasuryWithdrawals"],
      page: 1,
    });
  });

  it("never asks for a later page of a filter it just changed", () => {
    mocks.liveProposals = Array.from({ length: 12 }, (_, n) =>
      proposal(n + 1, { type: n % 2 ? "InfoAction" : "TreasuryWithdrawals" }),
    );
    mocks.pageTotal = 12;
    renderPage();
    fireEvent.click(screen.getByTestId("activity-next-page"));
    mocks.pageArgs = [];
    fireEvent.click(screen.getByTestId("activity-type-InfoAction"));
    expect(mocks.pageArgs.length).toBeGreaterThan(0);
    mocks.pageArgs.forEach((args) =>
      expect(args).toMatchObject({ filters: ["InfoAction"], page: 1 }),
    );
  });

  it("hides type counts when the live list is incomplete", () => {
    mocks.liveTotal = 60;
    renderPage();
    expect(screen.getByTestId("activity-type-all")).toHaveTextContent(/^All$/);
  });

  it("shows deadlines with how many days are left", () => {
    renderPage();
    const rows = screen.getAllByTestId("active-action-row");
    expect(within(rows[0]).getByText("2 days left")).toBeInTheDocument();
    expect(within(rows[1]).getByText("5 days left")).toBeInTheDocument();
  });

  it("pages through the active list and returns to page 1 on a new search", () => {
    mocks.liveProposals = Array.from({ length: 12 }, (_, n) => proposal(n + 1));
    mocks.pageTotal = 12;
    renderPage();
    expect(screen.getByText("Showing 1–5 of 12")).toBeInTheDocument();
    expect(screen.getByTestId("activity-previous-page")).toBeDisabled();
    fireEvent.click(screen.getByTestId("activity-next-page"));
    expect(screen.getByText("Showing 6–10 of 12")).toBeInTheDocument();
    fireEvent.change(screen.getByPlaceholderText("Search title or Governance Action ID"), {
      target: { value: "budget" },
    });
    expect(mocks.pageArgs.at(-1)).toMatchObject({ page: 1, searchPhrase: "budget" });
  });
});
