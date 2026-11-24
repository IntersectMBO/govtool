import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, within } from "storybook/test";

import { GovernanceActionsHeader } from "@organisms";
import { useProposeHeaderControls } from "@/components/organisms/ProposeHeaderControls";

/** The Proposed tab's header, as a visitor without a wallet sees it. */
const ProposedTabHeader = () => {
  const { action, notice } = useProposeHeaderControls(null);
  return <GovernanceActionsHeader action={action} notice={notice} />;
};

const meta = {
  title: "Example/GovernanceActionsHeader",
  component: GovernanceActionsHeader,
  parameters: { layout: "padded" },
} satisfies Meta<typeof GovernanceActionsHeader>;

export default meta;
type Story = StoryObj<typeof meta>;

export const LiveVotingAndOutcomes: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("create-governance-action-button")).toBeVisible();
    await Promise.all(
      ["total", "votingNow", "expiringSoon", "enacted"].map((id) =>
        expect(canvas.getByTestId(`governance-actions-stat-${id}`)).toBeVisible()),
    );
  },
};

export const ProposedDisconnected: Story = {
  render: () => <ProposedTabHeader />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("propose-a-governance-action-button")).toBeEnabled();
    await expect(canvas.getByRole("status")).toHaveTextContent(
      "To submit a proposal, you need to connect a Cardano Wallet.",
    );
    await expect(canvas.queryByTestId("create-governance-action-button")).toBeNull();
  },
};
