import type { Meta, StoryObj } from "@storybook/react-vite";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { expect, userEvent, within } from "storybook/test";

import { DRepDashboard } from "@/components/organisms/DRepDashboard";
import { QUERY_KEYS } from "@/consts/queryKeys";
import type { VoterInfo } from "@/models";

// The dashboard reads its data through queries; these stories seed the cache
// so it renders without a backend or a wallet.

const D_REP_ID = "cd".repeat(28);
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

const statusOf = (index: number) => {
  if (index < 2) return { ...NO_STATUS, ratified_epoch: 523 + index, enacted_epoch: 524 + index };
  if (index === 3) return { ...NO_STATUS, expired_epoch: 524 };
  return NO_STATUS;
};
const DAY = 24 * 60 * 60 * 1000;
const ago = (days: number) => new Date(Date.now() - days * DAY).toISOString();

const TITLES = [
  ["Reimburse Ikigai Info Governance Action Deposit", "TreasuryWithdrawals", "yes"],
  ["Reduce minPoolCost to 75 ada and increase Plutus Memory Limits", "ParameterChange", "yes"],
  ["Update Constitutional Committee 2026", "NewCommittee", "yes"],
  ["Global Order Book: connect Cardano DeFi", "TreasuryWithdrawals", "no"],
  ["Bifrost: Unlocking Bitcoin DeFi on Cardano", "TreasuryWithdrawals", "abstain"],
  ["Net Change Limit: Cardano Treasury", "InfoAction", "yes"],
] as const;

const actions = TITLES.map(([title, type], index) => ({
  id: `tx${index}#0`,
  tx_hash: `tx${index}`,
  index: 0,
  type,
  title,
  epoch_no: 520 + index,
  time: ago(40 - index * 5),
  status: statusOf(index),
  status_times: NO_TIMES,
}));

const votes = TITLES.map(([title, type, vote], index) => ({
  vote: { vote, epochNo: 521 + index, date: ago(38 - index * 5), url: "https://example.com/rationale.jsonld", metadataHash: "hash", txHash: `v${index}`, drepId: D_REP_ID, proposalId: `tx${index}#0` },
  proposal: { txHash: `tx${index}`, index: 0, type, title, abstract: "A short abstract of the governance action.", createdDate: ago(40 - index * 5) },
}));

const dRep = {
  drepId: D_REP_ID,
  view: "drep1yfhj8e0n8l9xq3wtzpfe9k0d5j2r0k3m4h6z7g8f9e0d1c2b3a4qx2f",
  status: "Active",
  givenName: "Jordan Dyer",
  latestRegistrationDate: "2025-03-15T10:00:00Z",
  latestTxHash: "8f31c6e279a3b014b3d764fce4867d6dc2f1aa75c12be4f18291bb85f207d90a",
  metadataHash: "81ca64e9e2b4dd1704f",
  url: "https://raw.githubusercontent.com/example/drep.jsonld",
  votingPower: 2_847_392_000_000,
  identityReferences: [{ "@type": "Identity", label: "GitHub", uri: "https://github.com/example" }],
  linkReferences: [{ "@type": "Links", label: "My Governance Blog", uri: "https://example.medium.com" }],
  type: "DRep",
  deposit: 500_000_000,
  isScriptBased: false,
  imageUrl: null,
  image: null,
};

const voter = {
  givenName: "Jordan",
  isRegisteredAsDRep: true,
  wasRegisteredAsDRep: true,
  isRegisteredAsSoleVoter: false,
  votingPower: 2_847_392_000_000,
  url: dRep.url,
  dataHash: dRep.metadataHash,
  paymentAddress: "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x",
  dRepRegisterTxHash: dRep.latestTxHash,
  objectives: "To champion transparent and inclusive governance practices within the Cardano ecosystem.",
  motivations: "Having been an active Cardano community member since 2019, I want to formalize my contributions by serving as a DRep.",
  qualifications: "Masters in Public Policy. Five years of experience in DAO governance frameworks.",
} as unknown as VoterInfo;

const clientWith = (withVotes: boolean) => {
  const client = new QueryClient({
    defaultOptions: { queries: { retry: false, staleTime: Infinity, refetchOnMount: false } },
  });
  client.setQueryData(
    [QUERY_KEYS.drepDashboardVotesKey, undefined, D_REP_ID],
    withVotes ? votes : [],
  );
  client.setQueryData([QUERY_KEYS.drepDashboardActionsKey], actions);
  client.setQueryData(
    [QUERY_KEYS.useGetDRepListInfiniteKey, "noPendingTransaction", "", D_REP_ID, "", ""],
    {
      // A DRep counts as new for 30 days after registering.
      pages: [{
        elements: [withVotes ? dRep : { ...dRep, latestRegistrationDate: ago(3) }],
        page: 0,
        pageSize: 10,
        total: 1,
      }],
      pageParams: [0],
    },
  );
  return client;
};

const meta = {
  title: "Example/DRepDashboard",
  component: DRepDashboard,
  parameters: { layout: "padded" },
  args: {
    voter,
    dRepID: D_REP_ID,
    stakeKey: `e1${"ab".repeat(28)}`,
    pendingTransaction: {} as never,
    walletCard: <div data-testid="wallet-card-placeholder" />,
    actionCards: <div data-testid="action-cards-placeholder" />,
  },
} satisfies Meta<typeof DRepDashboard>;

export default meta;
type Story = StoryObj<typeof meta>;

export const AfterSomeActivity: Story = {
  decorators: [
    (Story) => (
      <QueryClientProvider client={clientWith(true)}>
        <Story />
      </QueryClientProvider>
    ),
  ],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByRole("heading", { name: "Welcome, Jordan" })).toBeVisible();
    await expect(canvas.getAllByTestId("drep-recent-vote-row")).toHaveLength(3);
    await userEvent.click(canvas.getByTestId("drep-dashboard-tab-governance"));
    await expect(await canvas.findAllByTestId("drep-vote-row")).toHaveLength(6);
  },
};

export const Empty: Story = {
  decorators: [
    (Story) => (
      <QueryClientProvider client={clientWith(false)}>
        <Story />
      </QueryClientProvider>
    ),
  ],
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("dashboard-role-pill")).toHaveTextContent("New DRep");
    await expect(canvas.getByTestId("drep-recent-votes-empty")).toBeVisible();
  },
};
