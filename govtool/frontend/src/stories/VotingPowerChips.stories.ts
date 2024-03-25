import type { Meta, StoryObj } from "@storybook/react";

import { VotingPowerChips } from "@atoms";
import { userEvent, waitFor, within, screen } from "@storybook/testing-library";
import { expect } from "@storybook/jest";

const meta = {
  title: "Example/VotingPowerChips",
  component: VotingPowerChips,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof VotingPowerChips>;

export default meta;
type Story = StoryObj<typeof meta>;

export const VotingPowerChipsComponent: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText("Voting power:")).toBeInTheDocument();

    await expect(canvas.getByTestId("InfoOutlinedIcon")).toBeInTheDocument();
    await userEvent.hover(canvas.getByTestId("InfoOutlinedIcon"));
    await waitFor(async () => {
      await expect(screen.getByRole("tooltip")).toBeInTheDocument();
      await expect(screen.getByRole("tooltip")).toHaveTextContent(
        "DRep Voting PowerThis is the voting power delegated to you as a DRep and it is calculated at the end of every epoch for the epoch that just ended. IMPORTANT: When voting, the voting power provides an indication and not the exact number.",
      );
    });
  },
};
