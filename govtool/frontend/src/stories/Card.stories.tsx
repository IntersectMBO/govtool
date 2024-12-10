import { expect, within } from "@storybook/test";
import type { Meta, StoryObj } from "@storybook/react";

import { Card } from "@molecules";

const meta = {
  title: "Example/Card",
  component: Card,
  args: {
    variant: "default",
    children: "Put here whatever you want to display in the card.",
  },
  parameters: {
    layout: "centered",
    backgrounds: {
      default: "gradient",
      values: [
        {
          name: "white",
          value: "#fff",
        },
        {
          name: "gradient",
          value:
            "linear-gradient(to bottom right, #0033AD70, #0033AD70 20%, transparent)",
        },
      ],
    },
  },
} satisfies Meta<typeof Card>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Default: Story = {
  args: {},
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(
      canvas.getByText("Put here whatever you want to display in the card."),
    ).toBeVisible();
  },
};

export const WithLabel: Story = {
  args: {
    label: "Label goes here",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(
      canvas.getByText("Put here whatever you want to display in the card."),
    ).toBeVisible();
    await expect(canvas.getByText("Label goes here")).toBeVisible();
  },
};
