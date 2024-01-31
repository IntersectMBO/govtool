import type { Meta, StoryObj } from "@storybook/react";
import { userEvent, within } from "@storybook/testing-library";
import { expect, jest } from "@storybook/jest";
import { Drawer } from "@organisms";

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
    window.open = jest.fn();

    await userEvent.click(canvas.getByTestId("logo-button"));
    const dashboardLink = canvas.getByTestId("dashboard-link");
    const governanceActionsLink = canvas.getByTestId("governance-actions-link");
    await userEvent.click(dashboardLink);
    await expect(dashboardLink).toHaveClass("active");
    await userEvent.click(canvas.getByTestId("governance-actions-link"));
    await expect(governanceActionsLink).toHaveClass("active");
    await userEvent.click(canvas.getByTestId("guides-link"));
    await userEvent.click(canvas.getByTestId("faqs-link"));
    await userEvent.click(canvas.getByTestId("helps-link"));
    await expect(window.open).toHaveBeenCalledTimes(3);
  },
};
