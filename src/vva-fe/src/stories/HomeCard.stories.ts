import type { Meta, StoryObj } from "@storybook/react";

import { ActionCard } from "@molecules";
import { IMAGES } from "@/consts";
import { within } from "@storybook/testing-library";
import { expect } from "@storybook/jest";

const meta = {
  title: "Example/HomeCard",
  component: ActionCard,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof ActionCard>;

export default meta;
type Story = StoryObj<typeof meta>;

export const HomeCard: Story = {
  args: {
    description: "Lorem ipsum dolor sit amet, consectetur adipisicing elit.",
    firstButtonLabel: "first button",
    imageHeight: 80,
    imageURL: IMAGES.govActionDelegateImage,
    imageWidth: 115,
    secondButtonLabel: "second button",
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
