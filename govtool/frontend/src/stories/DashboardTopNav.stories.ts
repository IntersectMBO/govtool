import type { Meta, StoryObj } from "@storybook/react";
import { expect, within } from "@storybook/test";

import { DashboardTopNav } from "@organisms";

const meta = {
  title: "Example/DashboardTopNav",
  component: DashboardTopNav,
  tags: ["autodocs"],
} satisfies Meta<typeof DashboardTopNav>;

export default meta;
type Story = StoryObj<typeof meta>;

export const DashboardTopNavComponent: Story = {
  args: { title: "Example title" },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText("Example title")).toBeInTheDocument();
  },
};
