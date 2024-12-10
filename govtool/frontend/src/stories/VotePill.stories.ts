import type { Meta, StoryObj } from "@storybook/react";
import { expect, within } from "@storybook/test";

import { VotePill } from "@atoms";

const meta = {
  title: "Example/VotePill",
  component: VotePill,
  parameters: {
    layout: "centered",
  },
  args: {
    vote: "abstain",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof VotePill>;

export default meta;
type Story = StoryObj<typeof meta>;

export const VotePillYes: Story = {
  args: {
    vote: "yes",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText(/Yes/i)).toBeInTheDocument();
  },
};

export const VotePillNo: Story = {
  args: {
    vote: "no",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText(/No/i)).toBeInTheDocument();
  },
};

export const VotePillAbstain: Story = {
  args: {
    vote: "abstain",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText(/Abstain/i)).toBeInTheDocument();
  },
};
