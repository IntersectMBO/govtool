import type { Meta, StoryObj } from "@storybook/react";

import { StatusPill } from "@atoms";

const meta = {
  title: "Example/StatusPill",
  component: StatusPill,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof StatusPill>;

export default meta;
type Story = StoryObj<typeof meta>;

export const StatusPillActive: Story = {
  args: {
    status: "active",
  },
};

export const StatusPillInactive: Story = {
  args: {
    status: "inactive",
  },
};

export const StatusPillRetired: Story = {
  args: {
    status: "retired",
  },
};
