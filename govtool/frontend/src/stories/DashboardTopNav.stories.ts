import type { Meta, StoryObj } from "@storybook/react";

import { DashboardTopNav } from "@organisms";
import { expect } from "@storybook/jest";
import { within } from "@storybook/testing-library";

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
