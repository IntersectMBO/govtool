import type { Meta, StoryObj } from "@storybook/react-vite";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { expect, userEvent, waitFor, within } from "storybook/test";

import { RetiredDRepDashboard } from "@/components/organisms/DRepDashboard/RetiredDRepDashboard";
import { ShareDRepDialog } from "@/components/organisms/DRepDashboard/ShareDRepDialog";
import { ReRegisterIntro } from "@/components/organisms/RegisterAsDRepSteps/ReRegisterIntro";
import { WhatRetirementMeans } from "@/components/organisms/RegisterAsDRepSteps/WhatRetirementMeans";
import { QUERY_KEYS } from "@/consts/queryKeys";
import { SnackbarProvider } from "@/context/snackbar";
import type { VoterInfo } from "@/models";

// The retired view reads its data through queries; this story seeds the cache.

const D_REP_ID = "ef".repeat(28);

const dRep = {
  drepId: D_REP_ID,
  view: "drep1yfhj8e0n8l9xq3wtzpfe9k0d5j2r0k3m4h6z7g8f9e0d1c2b3a4qx2f",
  status: "Retired",
  givenName: "Jordan Dyer",
  latestRegistrationDate: "2025-03-15T10:00:00Z",
  metadataHash: "81ca64e9e2b4dd1704f",
  url: "https://raw.githubusercontent.com/example/drep.jsonld",
  votingPower: 2_847_392_000_000,
  identityReferences: [{ "@type": "Identity", label: "GitHub", uri: "https://github.com/example" }],
  linkReferences: [{ "@type": "Links", label: "My Governance Blog", uri: "https://example.medium.com" }],
};

const votes = ["Kenya Retirement Edition Proposal", "Stablecoin DeFi Liquidity Budget", "Primer Party parameter change"]
  .map((title, index) => ({
    vote: { vote: ["yes", "no", "abstain"][index], epochNo: 590 - index, date: `2026-09-${19 - index * 4}T00:00:00Z` },
    proposal: { txHash: `r${index}`, index: 0, type: "InfoAction", title },
  }));

const voter = {
  givenName: "Jordan",
  isRegisteredAsDRep: false,
  wasRegisteredAsDRep: true,
  isRegisteredAsSoleVoter: false,
  votingPower: null,
  deposit: 0,
  url: dRep.url,
  paymentAddress: "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x",
  objectives: "To champion transparent and inclusive governance practices within the Cardano ecosystem.",
  motivations: "Having been an active Cardano community member since 2019.",
  qualifications: "Masters in Public Policy.",
} as unknown as VoterInfo;

const client = () => {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false, staleTime: Infinity, refetchOnMount: false } },
  });
  queryClient.setQueryData([QUERY_KEYS.drepDashboardVotesKey, undefined, D_REP_ID], votes);
  queryClient.setQueryData(
    [QUERY_KEYS.useGetDRepListInfiniteKey, "noPendingTransaction", "", D_REP_ID, "", ""],
    { pages: [{ elements: [dRep], page: 0, pageSize: 10, total: 1 }], pageParams: [0] },
  );
  return queryClient;
};

const meta = {
  title: "Example/RetiredDRepDashboard",
  component: RetiredDRepDashboard,
  parameters: { layout: "padded" },
  args: { voter, dRepID: D_REP_ID, actionCards: <div /> },
  decorators: [
    (Story) => (
      <QueryClientProvider client={client()}>
        <Story />
      </QueryClientProvider>
    ),
  ],
} satisfies Meta<typeof RetiredDRepDashboard>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Retired: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("dashboard-role-pill")).toHaveTextContent("Retired");
    await expect(canvas.getByTestId("register-button")).toBeVisible();
    await expect(canvas.getAllByTestId("retired-archived-vote")).toHaveLength(3);
  },
};

export const ShareDialog: StoryObj<typeof ShareDRepDialog> = {
  render: () => (
    <SnackbarProvider>
      <ShareDRepDialog link="https://gov.tools/drep_directory/drep1yfhj8e0n8l9xq3wtzpfe9k0d5j2r0k3m4h6z7g8f9e0d1c2b3a4qx2f" />
    </SnackbarProvider>
  ),
  play: async ({ canvasElement }) => {
    await userEvent.click(within(canvasElement).getByTestId("share-drep-button"));
    const dialog = within(document.body);
    const shown = await dialog.findByTestId("share-drep-dialog");
    await waitFor(() => expect(shown).toBeVisible());
    await expect(dialog.getByTestId("share-drep-x")).toHaveAttribute("target", "_blank");
  },
};

const card = { background: "#FFFFFF", borderRadius: 16, margin: "40px auto", maxWidth: 600, padding: 32 };

export const RetireConfirmation: StoryObj = {
  render: () => (
    <div style={card}>
      <WhatRetirementMeans onClickCancel={() => {}} onSubmitted={() => {}} />
    </div>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("continue-retirement-button")).toBeDisabled();
    await userEvent.click(canvas.getByTestId("retirement-ack-voting"));
    await userEvent.click(canvas.getByTestId("retirement-ack-deposit"));
    await expect(canvas.getByTestId("continue-retirement-button")).toBeEnabled();
  },
};

export const RegisterAgain: StoryObj = {
  render: () => (
    <div style={card}>
      <ReRegisterIntro onCancel={() => {}} onContinue={() => {}} />
    </div>
  ),
  play: async ({ canvasElement }) => {
    await expect(within(canvasElement).getByTestId("continue-register-again-button")).toBeVisible();
  },
};
