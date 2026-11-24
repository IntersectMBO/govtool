import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, fn, userEvent, within } from "storybook/test";

import { FilterSidebar } from "@molecules";

const meta = {
  title: "Example/FilterSidebar",
  component: FilterSidebar,
  parameters: { layout: "padded" },
  tags: ["autodocs"],
} satisfies Meta<typeof FilterSidebar>;

export default meta;
type Story = StoryObj<typeof meta>;

export const ActionTypes: Story = {
  args: {
    title: "Action type",
    allLabel: "All Governance Actions",
    allCount: 3,
    options: [
      { key: "InfoAction", label: "Info Action", count: 1 },
      { key: "TreasuryWithdrawals", label: "Treasury Withdrawal", count: 2 },
      { key: "HardForkInitiation", label: "Hard Fork", count: 0 },
    ],
    selected: [],
    onChange: fn(),
  },
  play: async ({ args, canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(
      canvas.getByTestId("AllGovernanceActions-checkbox"),
    ).toHaveAttribute("aria-pressed", "true");
    await userEvent.click(canvas.getByTestId("HardFork-checkbox"));
    await expect(args.onChange).toHaveBeenCalledWith(["HardForkInitiation"]);
  },
};
