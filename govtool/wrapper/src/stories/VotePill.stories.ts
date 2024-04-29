import type { Meta, StoryObj } from "@storybook/react";

import { VotePill } from "@atoms";
import { within } from "@storybook/testing-library";
import { expect } from "@storybook/jest";

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
    await expect(canvas.getByText(/yes/i)).toBeInTheDocument();
  },
};

export const VotePillNo: Story = {
  args: {
    vote: "no",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText(/no/i)).toBeInTheDocument();
  },
};

export const VotePillAbstain: Story = {
  args: {
    vote: "abstain",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText(/abstain/i)).toBeInTheDocument();
  },
};
