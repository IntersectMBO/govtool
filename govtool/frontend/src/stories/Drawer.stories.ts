import { Drawer } from "@organisms";
import { expect, userEvent, within, fn } from "@storybook/test";
import type { Meta, StoryObj } from "@storybook/react";

const meta = {
  title: "Example/Drawer",
  component: Drawer,
  tags: ["autodocs"],
} satisfies Meta<typeof Drawer>;

export default meta;
type Story = StoryObj<typeof meta>;

export const DrawerComponent: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    window.open = fn();

    await userEvent.click(canvas.getByTestId("logo-button"));
    const dashboardLink = canvas.getByTestId("dashboard-link");
    const governanceActionsLink = canvas.getByTestId("governance-actions-link");
    await userEvent.click(dashboardLink);
    await expect(dashboardLink).toHaveClass("active");
    await userEvent.click(canvas.getByTestId("governance-actions-link"));
    await expect(governanceActionsLink).toHaveClass("active");
    await userEvent.click(canvas.getByTestId("guides-link"));
    await userEvent.click(canvas.getByTestId("faqs-link"));
    await expect(window.open).toHaveBeenCalledTimes(2);
  },
};
