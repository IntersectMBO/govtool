import { expect, userEvent, within } from "storybook/test";
import type { Meta, StoryObj } from "@storybook/react-vite";

import { TopNav } from "@organisms";

const meta = {
  title: "Example/TopNav",
  component: TopNav,
  tags: ["autodocs"],
  // The actions addon would otherwise hand onNavigateAway a spy, which turns
  // every link into the flow's "leave?" prompt.
  args: { onNavigateAway: undefined },
} satisfies Meta<typeof TopNav>;

export default meta;
type Story = StoryObj<typeof meta>;

const performCommonActions = async (canvas: ReturnType<typeof within>) => {
  const logoButton = canvas.getByTestId("logo-button");
  await userEvent.click(logoButton);

  const governanceActions = canvas.getByTestId("governance-actions");
  await expect(governanceActions).not.toHaveAttribute("aria-current");
  await userEvent.click(governanceActions);
  await expect(governanceActions).toHaveAttribute("aria-current", "page");

  const drepDirectoryLink = canvas.getByTestId("drep-directory-link");
  await expect(drepDirectoryLink).not.toHaveAttribute("aria-current");
  await userEvent.click(drepDirectoryLink);
  await expect(drepDirectoryLink).toHaveAttribute("aria-current", "page");

  const guidesLink = canvas.getByTestId("guides-link");
  await expect(guidesLink).toHaveAttribute("target", "_blank");
  await expect(guidesLink).toHaveAttribute("rel", "noopener noreferrer");
};

export const TopNavComponent: Story = {
  args: { isConnectButton: true },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await performCommonActions(canvas);
    await expect(
      canvas.getByTestId("connect-wallet-button"),
    ).toBeInTheDocument();
  },
};

export const TopNavWithoutButton: Story = {
  args: { isConnectButton: false },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await performCommonActions(canvas);
    await expect(
      canvas.queryByTestId("connect-wallet-button"),
    ).not.toBeInTheDocument();
  },
};
