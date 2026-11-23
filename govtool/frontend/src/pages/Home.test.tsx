import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen, within } from "@testing-library/react";
import { MemoryRouter } from "react-router";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";

import "@/i18n";

import { Home } from "./Home";

const NOW = new Date();
const daysAgo = (days: number) =>
  new Date(NOW.getTime() - days * 24 * 60 * 60 * 1000).toISOString();

const mocks = vi.hoisted(() => ({
  liveProposals: [] as unknown[],
  decidedActions: [] as unknown[],
  dReps: [] as unknown[],
}));

vi.mock("@context", () => ({
  useCardano: () => ({ isEnabled: false, stakeKey: undefined }),
  useModal: () => ({ openModal: vi.fn() }),
  useFeatureFlag: () => ({ isProposalDiscussionForumEnabled: true }),
  useChatwoot: () => ({ openFeedbackWindow: vi.fn() }),
  useAppContext: () => ({ epochParams: { epoch_no: 660 } }),
}));

vi.mock("@hooks", async (importOriginal) => ({
  ...(await importOriginal<typeof import("@hooks")>()),
  useGetLiveProposalsQuery: () => ({
    liveProposals: mocks.liveProposals,
    liveProposalsTotal: mocks.liveProposals.length,
    areLiveProposalsLoading: false,
  }),
  useGetDecidedActionsQuery: () => ({
    decidedActions: mocks.decidedActions,
    areDecidedActionsLoading: false,
  }),
  useGetDRepListPaginatedQuery: () => ({ dRepData: mocks.dReps }),
  useGetGovernanceActionMetadata: () => ({ metadata: undefined }),
}));

// The newsletter's posts come from another site; keep the test offline.
vi.mock("@/hooks/queries/useNewsletterPosts", () => ({
  useNewsletterPosts: () => ({
    items: [],
    isLoading: false,
    isError: false,
  }),
}));

vi.mock("@services", async (importOriginal) => ({
  ...(await importOriginal<typeof import("@services")>()),
  getGovernanceActionRecord: vi.fn(async () => ({
    vote_aggregates: [
      {
        role: "drep",
        representation: "stake",
        yes: "600",
        no: "200",
        abstain: "100",
        notVoted: "200",
        totalEligible: "1100",
        threshold: { numerator: 67, denominator: 100 },
      },
    ],
  })),
}));

const proposal = (n: number, over: Record<string, unknown> = {}) => ({
  id: `gov_action${n}`,
  txHash: `${n}`.padStart(64, "a"),
  index: 0,
  type: "InfoAction",
  title: `Live action ${n}`,
  createdDate: daysAgo(3),
  createdEpochNo: 658,
  expiryDate: new Date(NOW.getTime() + 2 * 86400000).toISOString(),
  expiryEpochNo: 664,
  details: { data: { tag: "InfoAction" } },
  dRepYesVotes: 2,
  dRepNoVotes: 1,
  ...over,
});

const decided = (n: number, status: Record<string, number | null>, time: string) => ({
  id: `gov_actiond${n}`,
  tx_hash: `${n}`.padStart(64, "b"),
  index: 0,
  type: "NewCommittee",
  title: `Decided action ${n}`,
  url: "",
  data_hash: "",
  status: {
    ratified_epoch: null,
    enacted_epoch: null,
    dropped_epoch: null,
    expired_epoch: null,
    ...status,
  },
  status_times: {
    ratified_time: status.ratified_epoch ? time : null,
    enacted_time: status.enacted_epoch ? time : null,
    dropped_time: null,
    expired_time: status.expired_epoch ? time : null,
  },
});

const renderHome = () =>
  render(
    <QueryClientProvider client={new QueryClient()}>
      <MemoryRouter>
        <Home />
      </MemoryRouter>
    </QueryClientProvider>,
  );

describe("Home", () => {
  beforeEach(() => {
    window.innerWidth = 1440;
    mocks.liveProposals = [
      proposal(1, { expiryEpochNo: 660 }),
      proposal(2, {
        type: "TreasuryWithdrawals",
        details: [{ receivingAddress: "stake1", amount: 103_000_000_000 }],
      }),
      proposal(3),
      proposal(4),
    ];
    mocks.decidedActions = [
      decided(1, { ratified_epoch: 655, enacted_epoch: 656 }, daysAgo(2)),
      decided(2, { expired_epoch: 650 }, daysAgo(10)),
      decided(3, { ratified_epoch: 640 }, daysAgo(90)),
    ];
    mocks.dReps = [
      { view: "drep1a", givenName: "Sarah Jenkins", status: "Active", votingPower: 12_400_000_000_000, objectives: "Scaling", doNotList: false },
      { view: "drep1b", givenName: "Hidden", status: "Active", votingPower: 9_000_000_000_000, doNotList: true },
      { view: "drep1c", givenName: "Alex Rivera", status: "Active", votingPower: 8_100_000_000_000, doNotList: false },
    ];
  });

  it("shows the hero with the governance actions entry point", () => {
    renderHome();
    expect(screen.getByRole("heading", { level: 1 })).toHaveTextContent(
      "Good governanceStarts with all of us.",
    );
    expect(
      screen.getByTestId("move-to-governance-actions-button"),
    ).toBeInTheDocument();
  });

  it("links the three participation cards to their guides, keeping the E2E ids", () => {
    renderHome();
    expect(screen.getByTestId("delegate-learn-more-button")).toHaveAttribute(
      "href",
      "https://docs.gov.tools/cardano-govtool/using-govtool/delegating",
    );
    expect(screen.getByTestId("d-rep-learn-more-button")).toHaveAttribute(
      "href",
      "https://docs.gov.tools/cardano-govtool/using-govtool/dreps/register-as-a-drep",
    );
    expect(screen.getByTestId("direct-voter-learn-more-button")).toHaveAttribute(
      "href",
      "https://docs.gov.tools/cardano-govtool/using-govtool/direct-voting",
    );
  });

  it("fills the snapshot from live and decided actions", () => {
    renderHome();
    const snapshot = within(screen.getByTestId("governance-snapshot"));
    expect(snapshot.getByText("Epoch 660")).toBeInTheDocument();
    expect(snapshot.getByText("Across 2 types")).toBeInTheDocument();
    // One live action expires in the current epoch; one action was ratified
    // within 30 days (the 90-day-old one is outside the window).
    expect(snapshot.getByText("Of 4 in voting")).toBeInTheDocument();
    expect(snapshot.getAllByText("1")).toHaveLength(2);
    expect(snapshot.getByText("Decided action 1")).toBeInTheDocument();
    expect(snapshot.getByText("Live action 1")).toBeInTheDocument();
  });

  it("lists the three newest live actions with their withdrawal amount", () => {
    renderHome();
    expect(screen.getAllByTestId("pulse-row")).toHaveLength(3);
    expect(screen.getByText("₳ 103.00k")).toBeInTheDocument();
    expect(screen.getAllByText("3 votes")).toHaveLength(3);
  });

  it("leaves DReps who asked not to be listed off the leaderboard", () => {
    renderHome();
    const cards = screen.getAllByTestId("leaderboard-drep-card");
    expect(cards).toHaveLength(2);
    expect(screen.queryByText("Hidden")).toBeNull();
    expect(within(cards[0]).getByText("₳ 12.4M")).toBeInTheDocument();
  });

  it("summarises recent outcomes", () => {
    renderHome();
    expect(screen.getAllByTestId("outcome-row")).toHaveLength(3);
    // Two decided in 30 days, one of them ratified.
    expect(screen.getByText("Actions decided · 30 days").previousSibling).toHaveTextContent("2");
    expect(screen.getByText("Ratification rate").previousSibling).toHaveTextContent("50%");
  });

  it("opens one FAQ answer at a time", () => {
    renderHome();
    expect(screen.getByText(/brings together tools for Cardano governance/)).toBeInTheDocument();
    fireEvent.click(screen.getByTestId("faq-needWallet"));
    expect(screen.getByTestId("faq-needWallet")).toHaveAttribute("aria-expanded", "true");
    expect(screen.queryByText(/brings together tools for Cardano governance/)).toBeNull();
  });

  it("ends with the DRep banner linking to the DRep guide", () => {
    renderHome();
    expect(screen.getByTestId("drep-banner-learn-more")).toHaveAttribute(
      "href",
      "https://docs.gov.tools/cardano-govtool/using-govtool/dreps",
    );
  });
});
