import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, within } from "storybook/test";

import { ProposeActionBanner } from "@organisms";

const meta = {
  title: "Example/ProposeActionBanner",
  component: ProposeActionBanner,
  parameters: { layout: "padded" },
  tags: ["autodocs"],
} satisfies Meta<typeof ProposeActionBanner>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Default: Story = {
  args: { to: "/proposal_discussion/propose" },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(
      canvas.getByRole("heading", { name: "Have an idea for Cardano governance?" }),
    ).toBeVisible();
    await expect(
      canvas.getByTestId("propose-action-banner-button"),
    ).toHaveAttribute("href", "/proposal_discussion/propose");
  },
};
