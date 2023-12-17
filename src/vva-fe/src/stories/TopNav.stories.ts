import type { Meta, StoryObj } from "@storybook/react";
import { userEvent, within } from "@storybook/testing-library";
import { TopNav } from "@organisms";
import { expect, jest } from "@storybook/jest";

const meta = {
  title: "Example/TopNav",
  component: TopNav,
  tags: ["autodocs"],
} satisfies Meta<typeof TopNav>;

export default meta;
type Story = StoryObj<typeof meta>;

const performCommonActions = async (canvas: any) => {
  window.open = jest.fn();

  await userEvent.click(canvas.getByTestId("logo-button"));
  const homeLink = canvas.getByTestId("home-link");
  const governanceActionsLink = canvas.getByTestId("governance-actions-link");
  await userEvent.click(homeLink);
  await expect(homeLink).toHaveClass("active");
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
      canvas.getByTestId("connect-wallet-button")
    ).toBeInTheDocument();
  },
};

export const TopNavWithoutButton: Story = {
  args: { isConnectButton: false },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await performCommonActions(canvas);
    await expect(
      canvas.queryByTestId("connect-wallet-button")
    ).not.toBeInTheDocument();
  },
};
