import type { Meta, StoryObj } from "@storybook/react";

import { StatusPill } from "@atoms";
import { DRepStatus } from "@/models";

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
    status: DRepStatus.Active,
  },
};

export const StatusPillInactive: Story = {
  args: {
    status: DRepStatus.Inactive,
  },
};

export const StatusPillRetired: Story = {
  args: {
    status: DRepStatus.Retired,
  },
};
