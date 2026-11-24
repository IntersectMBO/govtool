import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, within } from "storybook/test";

import { NotificationBell } from "@organisms";

const meta = {
  title: "Example/NotificationBell",
  component: NotificationBell,
  parameters: { layout: "centered" },
  tags: ["autodocs"],
} satisfies Meta<typeof NotificationBell>;

export default meta;
type Story = StoryObj<typeof meta>;

// Without a connected wallet there is nothing to count: the bell shows plain.
export const NoUnread: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByRole("button", { name: "Notifications" })).toBeVisible();
  },
};
