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

  await userEvent.click(canvas.getByTestId("logo-button"));
  const governanceActionsLink = canvas.getByTestId("governance-actions-link");
  await userEvent.click(canvas.getByTestId("governance-actions-link"));
  await expect(governanceActionsLink).toHaveClass("active");
  await userEvent.click(canvas.getByTestId("guides-link"));
  await userEvent.click(canvas.getByTestId("faqs-link"));
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
