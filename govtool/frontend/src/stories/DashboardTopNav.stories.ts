import type { Meta, StoryObj } from "@storybook/react";

import { DashboardTopNav } from "@organisms";
import {
  within, userEvent, waitFor, screen,
} from "@storybook/testing-library";
import { expect } from "@storybook/jest";

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
    await expect(canvas.getByText("Voting power:")).toBeInTheDocument();

    await expect(canvas.getByTestId("InfoOutlinedIcon")).toBeInTheDocument();
    await userEvent.hover(canvas.getByTestId("InfoOutlinedIcon"));
    await waitFor(async () => {
      await expect(screen.getByRole("tooltip")).toBeInTheDocument();
      await expect(screen.getByRole("tooltip")).toHaveTextContent(
        /DRep Voting Power/i,
      );
    });
  },
};
