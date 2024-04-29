import type { Meta, StoryObj } from "@storybook/react";

import { LoadingButton } from "@atoms";

const meta = {
  title: "Example/LoadingButton",
  component: LoadingButton,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof LoadingButton>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Primary: Story = {
  args: {
    children: "Button",
    variant: "contained",
    isLoading: false,
  },
};

export const Loading: Story = {
  args: {
    children: "Button",
    variant: "contained",
    isLoading: true,
  },
};
