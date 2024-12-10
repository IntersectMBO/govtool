import { expect, userEvent, within, fn } from "@storybook/test";
import type { Meta, StoryObj } from "@storybook/react";

import { Button } from "@atoms";

const meta = {
  title: "Example/Button",
  component: Button,
  args: {
    onClick: fn(),
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
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByRole("button")).toHaveTextContent("Button");
    await userEvent.click(canvas.getByRole("button"));
    await expect(args.onClick).toHaveBeenCalled();
  },
};

export const Outlined: Story = {
  args: { size: "small", variant: "outlined", children: "Button" },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByRole("button")).toHaveTextContent("Button");
    await userEvent.click(canvas.getByRole("button"));
    await expect(args.onClick).toHaveBeenCalled();
  },
};

export const Text: Story = {
  args: {
    size: "large",
    variant: "text",
    children: "Button",
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByRole("button")).toHaveTextContent("Button");
    await userEvent.click(canvas.getByRole("button"));
    await expect(args.onClick).toHaveBeenCalled();
  },
};

export const LoadingButton: Story = {
  args: {
    children: "Button",
    size: "large",
    variant: "contained",
    isLoading: true,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByRole("button")).toHaveTextContent("Button");
    await expect(canvas.getByRole("button")).toBeDisabled();
  },
};
