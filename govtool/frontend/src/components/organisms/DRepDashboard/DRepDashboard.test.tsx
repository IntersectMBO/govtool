import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen, within } from "@testing-library/react";
import { MemoryRouter, Route, Routes, useLocation } from "react-router";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";

import "@/i18n";
import type { GovernanceActionRecord, VotedProposal, VoterInfo } from "@/models";

import { DRepDashboard } from "./DRepDashboard";

const NO_STATUS = {
  ratified_epoch: null,
  enacted_epoch: null,
  dropped_epoch: null,
  expired_epoch: null,
};
const NO_TIMES = {
  ratified_time: null,
  enacted_time: null,
  dropped_time: null,
  expired_time: null,
};

const query = (over: Record<string, unknown> = {}) => ({
  isServed: false,
  data: undefined,
  isLoading: false,
  isError: false,
  refetch: vi.fn(),
  ...over,
});

const mocks = vi.hoisted(() => ({
  app: {} as Record<string, unknown>,
  forum: false,
  dRep: undefined as Record<string, unknown> | undefined,
  votes: [] as unknown[] | undefined,
  actions: [] as unknown[] | undefined,
  delegators: {} as Record<string, unknown>,
  history: {} as Record<string, unknown>,
  delegationHistory: {} as Record<string, unknown>,
  growth: {} as Record<string, unknown>,
  discussions: {} as Record<string, unknown>,
  rationale: { voteContextText: "Because it funds tooling.", isLoading: false, valid: true },
}));

vi.mock("@/context/appContext", () => ({ useAppContext: () => mocks.app }));
vi.mock("@/context/featureFlag", () => ({
  useFeatureFlag: () => ({ isProposalDiscussionForumEnabled: mocks.forum }),
}));
vi.mock("@/context/snackbar", () => ({ useSnackbar: () => ({ addSuccessAlert: vi.fn() }) }));
vi.mock("@/components/molecules/Share", () => ({ Share: () => null }));
vi.mock("@/hooks/queries/useGetDrepDetailsQuery", () => ({
  useGetDRepDetailsQuery: () => ({ dRep: mocks.dRep, isLoading: mocks.dRep === undefined }),
}));
vi.mock("@/hooks/queries/useGetAdaHolderCurrentDelegationQuery", () => ({
  useGetAdaHolderCurrentDelegationQuery: () => ({ currentDelegation: null }),
}));
vi.mock("@/hooks/queries/useGetAdaHolderVotingPowerQuery", () => ({
  useGetAdaHolderVotingPowerQuery: () => ({ votingPower: 5_000_000 }),
}));
vi.mock("@/hooks/queries/useGetVoteContextTextFromFile", () => ({
  useGetVoteContextTextFromFile: () => mocks.rationale,
}));
vi.mock("@/hooks/queries/useDRepDashboard", () => ({
  useDRepDashboardVotes: () => ({
    votes: mocks.votes,
    isLoading: false,
    isError: false,
    refetch: vi.fn(),
    dataUpdatedAt: Date.now(),
  }),
  useDRepDashboardActions: () => ({
    actions: mocks.actions,
    isLoading: false,
    isError: false,
    refetch: vi.fn(),
    dataUpdatedAt: Date.now(),
  }),
  useDRepDelegators: () => mocks.delegators,
  useDRepHistory: () => mocks.history,
  useDelegationHistory: () => mocks.delegationHistory,
  useDRepVotingPowerHistory: () => mocks.growth,
  useDRepDiscussions: () => mocks.discussions,
}));

const STAKE_HEX = `e1${"ab".repeat(28)}`;

const voter = {
  givenName: "Jordan",
  isRegisteredAsDRep: true,
  votingPower: 2_000_000_000,
  url: "https://example.com/drep.jsonld",
  paymentAddress: null,
  dRepRegisterTxHash: "regtx",
  objectives: "Transparency",
  motivations: null,
  qualifications: null,
} as unknown as VoterInfo;

const record = (id: string, over: Partial<GovernanceActionRecord> = {}) =>
  ({
    id: `${id}#0`,
    tx_hash: id,
    index: 0,
    type: "InfoAction",
    title: `Action ${id}`,
    epoch_no: 9,
    time: "2026-11-01T00:00:00Z",
    status: NO_STATUS,
    status_times: NO_TIMES,
    ...over,
  }) as GovernanceActionRecord;

const voted = (id: string, vote: "yes" | "no" | "abstain", epoch = 10, url = "https://r.example/x.json") =>
  ({
    vote: { vote, epochNo: epoch, date: "2026-11-10T00:00:00Z", url, metadataHash: "hash", txHash: `v${id}`, drepId: "d", proposalId: id },
    proposal: { txHash: id, index: 0, type: "InfoAction", title: `Action ${id}`, createdDate: "2026-11-08T00:00:00Z" },
  }) as unknown as VotedProposal;

const ProfileStub = () => {
  const { state } = useLocation();
  return <div data-testid="profile-stub">{JSON.stringify(state)}</div>;
};

const dashboard = (pending: Record<string, unknown> = {}) => (
  <DRepDashboard
    actionCards={<div>action cards</div>}
    dRepID={"cd".repeat(28)}
    pendingTransaction={pending as never}
    stakeKey={STAKE_HEX}
    voter={voter}
    walletCard={<div>wallet card</div>}
  />
);

const tree = (path: string, client: QueryClient, pending: Record<string, unknown> = {}) => (
  <QueryClientProvider client={client}>
    <MemoryRouter initialEntries={[path]}>
      <Routes>
        <Route element={dashboard(pending)} path="/dashboard" />
        <Route element={<ProfileStub />} path="/connected/drep_directory/:dRepId" />
      </Routes>
    </MemoryRouter>
  </QueryClientProvider>
);

const renderAt = (path = "/dashboard", client = new QueryClient()) => render(tree(path, client));

beforeEach(() => {
  window.localStorage.clear();
  mocks.app = {
    epochParams: { epoch_no: 10 },
    cExplorerBaseUrl: "https://cexplorer.io",
    capabilitiesStatus: "ready",
    featureSet: { unavailable: {} },
  };
  mocks.forum = false;
  mocks.dRep = {
    view: "drep1example",
    status: "Active",
    latestRegistrationDate: "2025-03-15T00:00:00Z",
    metadataHash: "meta",
    identityReferences: [],
    linkReferences: [],
  };
  mocks.votes = [voted("a", "yes"), voted("b", "no", 9)];
  mocks.actions = [
    record("a", { status: { ...NO_STATUS, ratified_epoch: 11, enacted_epoch: 12 } }),
    record("b"),
  ];
  mocks.delegators = query();
  mocks.history = query();
  mocks.delegationHistory = query();
  mocks.growth = query();
  mocks.discussions = query({ isForumEnabled: false });
});

describe("DRepDashboard", () => {
  it("welcomes a DRep with no votes, and remembers a dismissal", () => {
    mocks.votes = [];
    mocks.dRep = { ...mocks.dRep, latestRegistrationDate: new Date().toISOString() };
    const { unmount } = renderAt();
    expect(screen.getByTestId("dashboard-role-pill")).toHaveTextContent("New DRep");
    fireEvent.click(screen.getByTestId("drep-dashboard-welcome-dismiss"));
    expect(screen.queryByTestId("drep-dashboard-welcome")).not.toBeInTheDocument();
    unmount();
    renderAt();
    expect(screen.queryByTestId("drep-dashboard-welcome")).not.toBeInTheDocument();
    expect(screen.getByTestId("drep-recent-votes-empty")).toBeInTheDocument();
  });

  it("does not call a long-registered DRep new for not having voted", () => {
    mocks.votes = [];
    renderAt();
    expect(screen.getByTestId("dashboard-role-pill")).toHaveTextContent("DRep");
    expect(screen.getByTestId("dashboard-role-pill")).not.toHaveTextContent("New");
    expect(screen.queryByTestId("drep-dashboard-welcome")).not.toBeInTheDocument();
  });

  it("opens the public profile with its back button", () => {
    renderAt();
    fireEvent.click(screen.getByTestId("view-drep-details-button"));
    expect(screen.getByTestId("profile-stub")).toHaveTextContent("enteredFromWithinApp");
  });

  it("refreshes the profile once a metadata update confirms", () => {
    const client = new QueryClient();
    const invalidate = vi.spyOn(client, "invalidateQueries");
    const { rerender } = render(tree("/dashboard", client, { updateMetaData: { transactionHash: "u1" } }));
    expect(invalidate).not.toHaveBeenCalled();
    rerender(tree("/dashboard", client, {}));
    expect(invalidate).toHaveBeenCalledWith({ queryKey: ["useGetDRepInfoKey"] });
    expect(invalidate).toHaveBeenCalledWith({ queryKey: ["useGetDRepListInfiniteKey"] });
  });

  it("holds the activity figures until the DRep record has loaded", () => {
    mocks.dRep = undefined;
    renderAt("/dashboard?tab=activity");
    expect(screen.queryByTestId("drep-activity-participation")).not.toBeInTheDocument();
  });

  it("shows recent votes with their outcome, and links to the full history", () => {
    renderAt();
    expect(screen.getByTestId("dashboard-role-pill")).toHaveTextContent("DRep");
    expect(screen.queryByTestId("drep-dashboard-welcome")).not.toBeInTheDocument();
    const rows = screen.getAllByTestId("drep-recent-vote-row");
    expect(rows).toHaveLength(2);
    expect(within(rows[0]).getByText("Enacted")).toBeInTheDocument();
    expect(screen.getByTestId("drep-recent-votes-view-all")).toHaveAttribute("href", "/dashboard?tab=governance");
    expect(screen.getByTestId("dRep-id-display-card-dashboard")).toHaveTextContent("drep1example");
    expect(screen.getByTestId("retire-button")).toHaveAttribute("href", "/retire_drep");
  });

  it("hides the discussions tab while the forum is off", () => {
    renderAt("/dashboard?tab=discussions");
    expect(screen.queryByTestId("drep-dashboard-tab-discussions")).not.toBeInTheDocument();
    expect(screen.getByTestId("drep-dashboard-panel-overview")).toBeInTheDocument();
  });

  it("totals the votes, filters by outcome and opens a rationale", () => {
    renderAt("/dashboard?tab=governance");
    expect(screen.getByTestId("drep-voting-history-totals")).toHaveTextContent(
      "2 Total Votes · Yes: 1 (50%) · No: 1 (50%) · Abstain: 0 (0%)",
    );
    expect(screen.getAllByTestId("drep-vote-row")).toHaveLength(2);

    fireEvent.click(screen.getByTestId("drep-votes-status"));
    fireEvent.click(screen.getByTestId("enacted-radio"));
    expect(screen.getAllByTestId("drep-vote-row")).toHaveLength(1);

    fireEvent.click(screen.getByTestId("drep-vote-view-rationale"));
    expect(screen.getByTestId("drep-rationale-text")).toHaveTextContent("Because it funds tooling.");
  });

  it("says delegators are unavailable until the backend serves them", () => {
    renderAt("/dashboard?tab=governance");
    fireEvent.click(screen.getByTestId("drep-activity-tab-delegators"));
    expect(screen.getByTestId("drep-delegators-unavailable")).toBeInTheDocument();
    expect(screen.queryByTestId("drep-delegation-distribution")).not.toBeInTheDocument();
  });

  it("lists delegators and their share once served", () => {
    mocks.delegators = query({
      isServed: true,
      data: {
        elements: [
          { stakeAddress: "stake1uxone", votingPower: "500000000", delegatedAt: { epoch: 9, time: "2026-03-15T00:00:00Z" } },
          { stakeAddress: "stake1uxtwo", votingPower: "300000000", delegatedAt: null },
        ],
        total: 2,
        page: 1,
        pageSize: 12,
      },
    });
    renderAt("/dashboard?tab=governance");
    expect(screen.getByTestId("drep-delegation-distribution")).toHaveTextContent(
      "Top 2 delegators hold 40.0% of your total voting power.",
    );
    fireEvent.click(screen.getByTestId("drep-activity-tab-delegators"));
    expect(screen.getAllByTestId("drep-delegator-row")).toHaveLength(2);
  });

  it("lists only the wallet's own submissions, once records carry a return address", async () => {
    const { bech32 } = await import("bech32");
    const own = bech32.encode("stake", bech32.toWords((STAKE_HEX.match(/../g) ?? []).map((h) => parseInt(h, 16))));
    renderAt("/dashboard?tab=governance");
    fireEvent.click(screen.getByTestId("drep-activity-tab-proposals"));
    expect(screen.getByTestId("drep-proposals-unknown")).toBeInTheDocument();

    mocks.actions = [
      record("a", { deposit_return_address: own }),
      record("b", { deposit_return_address: "stake1someoneelse" }),
    ];
    renderAt("/dashboard?tab=governance");
    fireEvent.click(screen.getAllByTestId("drep-activity-tab-proposals")[1]);
    expect(screen.getAllByTestId("drep-proposal-row")).toHaveLength(1);
  });

  it("exports the votes as CSV", async () => {
    const created: Blob[] = [];
    URL.createObjectURL = vi.fn((blob: Blob) => {
      created.push(blob);
      return "blob:x";
    });
    URL.revokeObjectURL = vi.fn();
    const click = vi.spyOn(HTMLAnchorElement.prototype, "click").mockImplementation(() => {});
    renderAt("/dashboard?tab=governance");
    fireEvent.click(screen.getByTestId("drep-dashboard-export-csv"));
    expect(click).toHaveBeenCalled();
    const text = await new Promise<string>((resolve) => {
      const reader = new FileReader();
      reader.onload = () => resolve(String(reader.result));
      reader.readAsText(created[0]);
    });
    expect(text.split("\r\n")[0]).toBe("Governance action,Action ID,Type,Vote,Epoch,Voted at,Outcome,Rationale URL");
    expect(text).toContain("Action a,");
    click.mockRestore();
  });

  it("shows the latest registration, with a note, before history is served", () => {
    renderAt("/dashboard?tab=history");
    const rows = screen.getAllByTestId("drep-history-row");
    expect(rows).toHaveLength(1);
    expect(rows[0]).toHaveTextContent("Registered");
    expect(screen.getByTestId("drep-history-partial")).toBeInTheDocument();
    fireEvent.click(screen.getByTestId("drep-history-open"));
    expect(screen.getByTestId("drep-history-dialog-hash")).toHaveTextContent("regtx");
  });

  it("reads the served history, and offers a retry when it fails", () => {
    const refetch = vi.fn();
    mocks.history = query({ isServed: true, isError: true, refetch });
    const { unmount } = renderAt("/dashboard?tab=history");
    fireEvent.click(within(screen.getByTestId("drep-history-error")).getByRole("button"));
    expect(refetch).toHaveBeenCalled();
    unmount();

    mocks.history = query({
      isServed: true,
      data: {
        elements: [
          { kind: "retired", status: "retired", at: { epoch: 9, time: "2026-09-12T09:08:00Z" }, txHash: "t2", anchor: null },
          { kind: "statusChanged", status: "inactive", at: { epoch: 8, time: "2026-09-01T18:45:00Z" }, txHash: null, anchor: null },
        ],
        total: 2,
        page: 1,
        pageSize: 100,
      },
    });
    renderAt("/dashboard?tab=history");
    const rows = screen.getAllByTestId("drep-history-row");
    expect(rows[0]).toHaveTextContent("Retired");
    expect(rows[1]).toHaveTextContent("Participation threshold not met");
    expect(screen.queryByTestId("drep-history-partial")).not.toBeInTheDocument();
  });

  it("reports participation and the voting streak", () => {
    renderAt("/dashboard?tab=activity");
    expect(screen.getByTestId("drep-activity-participation")).toHaveTextContent("100%");
    expect(screen.getByTestId("drep-activity-streak")).toHaveTextContent("2 Epochs");
    expect(screen.getAllByTestId("drep-epoch-row")[0]).toHaveTextContent("10 (Current)");
  });
});
