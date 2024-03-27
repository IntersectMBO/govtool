import type { Meta, StoryObj } from "@storybook/react";

import { DashboardActionCard } from "@molecules";
import { within } from "@storybook/testing-library";
import { expect } from "@storybook/jest";
import { IMAGES } from "@/consts";

const meta = {
  title: "Example/DashboardCard",
  component: DashboardActionCard,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof DashboardActionCard>;

export default meta;
type Story = StoryObj<typeof meta>;

export const DashboardCardComponent: Story = {
  args: {
    buttons: [
      { children: "first button" },
      { children: "second button" },
    ],
    description: "Lorem ipsum dolor sit amet, consectetur adipisicing elit.",
    imageURL: IMAGES.govActionDelegateImage,
    title: "Action card",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByText("Action card")).toBeInTheDocument();
    expect(canvas.getByText(/lorem/i)).toBeInTheDocument();
    const buttons = canvas.getAllByRole("button");
    await expect(buttons[0].textContent).toBe("first button");
    await expect(buttons[1].textContent).toBe("second button");
    expect(canvas.getByRole("img")).toBeInTheDocument();
  },
};

export const WithDRepIdDashboardCardComponent: Story = {
  args: {
    buttons: [
      { children: "first button" },
      { children: "second button" },
    ],
    description: "Lorem ipsum dolor sit amet, consectetur adipisicing elit.",
    imageURL: IMAGES.govActionDelegateImage,
    title: "Action card",
  },
};

export const LoadingDashboardCard: Story = {
  args: {
    buttons: [
      { children: "first button" },
      { children: "second button" },
    ],
    description: "Lorem ipsum dolor sit amet, consectetur adipisicing elit.",
    imageURL: IMAGES.govActionDelegateImage,
    title: "Action card",
    isLoading: true,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.queryByText("Action card")).not.toBeInTheDocument();
    expect(canvas.queryByText(/lorem/i)).not.toBeInTheDocument();
    await expect(canvas.queryAllByRole("button")).toHaveLength(0);
    expect(canvas.queryByRole("img")).not.toBeInTheDocument();
  },
};

export const InProgressDashboardCard: Story = {
  args: {
    buttons: [
      { children: "first button" },
      { children: "second button" },
    ],
    description: "Lorem ipsum dolor sit amet, consectetur adipisicing elit.",
    imageURL: IMAGES.govActionDelegateImage,
    title: "Action card",
    state: "inProgress",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getAllByText(/in progress/i)).toHaveLength(2);
  },
};
