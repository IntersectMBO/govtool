import { expect, userEvent, within, fn } from "@storybook/test";
import type { Meta, StoryObj } from "@storybook/react";

import { TopNav } from "@organisms";

const meta = {
  title: "Example/TopNav",
  component: TopNav,
  tags: ["autodocs"],
} satisfies Meta<typeof TopNav>;

export default meta;
type Story = StoryObj<typeof meta>;

const performCommonActions = async (canvas: ReturnType<typeof within>) => {
  window.open = fn();

  const logoButton = canvas.getByTestId("logo-button");
  await userEvent.click(logoButton);

  const governanceActions = canvas.getByTestId("governance-actions");
  await userEvent.click(governanceActions);

  // governance actions link is injected outside the TopNav component, so we use querySelector
  const governanceActionsLink = document.querySelector(
    '[data-testid="governance-actions-link"]',
  );
  if (!governanceActionsLink)
    throw new Error("governance-actions-link not found");
  await expect(governanceActionsLink).not.toHaveClass("active");
  await userEvent.click(governanceActionsLink);
  await expect(governanceActions).not.toHaveClass("active");
  await expect(governanceActionsLink).toHaveClass("active");

  const drepDirectoryLink = canvas.getByTestId("drep-directory-link");
  await expect(drepDirectoryLink).not.toHaveClass("active");
  await userEvent.click(drepDirectoryLink);
  await expect(drepDirectoryLink).toHaveClass("active");

  const guidesLink = canvas.getByTestId("guides-link");
  await userEvent.click(guidesLink);

  const faqsLink = canvas.getByTestId("faqs-link");
  await userEvent.click(faqsLink);

  await expect(window.open).toHaveBeenCalledTimes(2);
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
