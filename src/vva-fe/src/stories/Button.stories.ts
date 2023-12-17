import type { Meta, StoryObj } from "@storybook/react";
import { userEvent, within } from "@storybook/testing-library";
import { expect, jest } from "@storybook/jest";

import { Button } from "@atoms";

const meta = {
  title: "Example/Button",
  component: Button,
  args: {
    onClick: jest.fn(),
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByRole("button")).toHaveTextContent("Button");
    await userEvent.click(canvas.getByRole("button"));
    await expect(args.onClick).toHaveBeenCalled();
  },
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof Button>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Contained: Story = {
  args: {
    children: "Button",
    size: "medium",
    variant: "contained",
  },
};

export const Outlined: Story = {
  args: { size: "small", variant: "outlined", children: "Button" },
};

export const Text: Story = {
  args: {
    size: "large",
    variant: "text",
    children: "Button",
  },
};
