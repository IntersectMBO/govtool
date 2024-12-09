import type { Meta, StoryObj } from "@storybook/react";
import { expect, within } from "@storybook/test";

import { StatusPill } from "@atoms";
import { DRepStatus } from "@/models";

const meta = {
  title: "Example/StatusPill",
  component: StatusPill,
  parameters: {
    layout: "centered",
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);

    await expect(canvas.getByText(args.status)).toBeVisible();
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
