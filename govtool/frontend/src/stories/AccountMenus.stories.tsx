import type { Meta, StoryObj } from "@storybook/react-vite";
import { Box } from "@mui/material";
import { expect, userEvent, waitFor, within } from "storybook/test";

import { ConnectedWalletCard } from "@/components/organisms/ConnectedWalletCard";
import { ProfileMenu } from "@/components/organisms/ProfileMenu";
import { ProposalDiscussionProvider } from "@/context/proposalDiscussion";

// Storybook has no wallet, so both show their no-data states: the card's
// figures are dashes and the profile has no governance roles.
const meta = {
  title: "Example/AccountMenus",
  component: ConnectedWalletCard,
  parameters: { layout: "centered" },
} satisfies Meta<typeof ConnectedWalletCard>;

export default meta;
type Story = StoryObj<typeof meta>;

export const WalletPopup: Story = {
  args: { onViewProfile: () => {} },
  render: (args) => (
    <Box sx={{ border: "1px solid #E2E6EE", borderRadius: "12px", p: 2, width: 360 }}>
      <ConnectedWalletCard {...args} />
    </Box>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("view-profile-button")).toBeVisible();
    await expect(canvas.getByTestId("disconnect-button")).toBeVisible();
    await expect(canvas.getByRole("button", { name: "Switch wallet" })).toBeVisible();
  },
};

export const ProfilePopup: Story = {
  args: { onViewProfile: () => {} },
  // The app mounts this provider at its root (context/contextProviders.tsx).
  render: () => (
    <ProposalDiscussionProvider>
      <Box sx={{ display: "flex", justifyContent: "flex-end", minHeight: 420, width: 420 }}>
        <ProfileMenu />
      </Box>
    </ProposalDiscussionProvider>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByTestId("profile-avatar"));
    const menu = within(canvasElement.ownerDocument.body).getByTestId("profile-menu");
    // The popover fades in.
    await waitFor(() => expect(within(menu).getByTestId("profile-menu-overview")).toBeVisible());
    await expect(within(menu).queryByText("Governance roles")).toBeNull();
    await expect(within(menu).getByTestId("profile-menu-logout")).toBeVisible();
  },
};
