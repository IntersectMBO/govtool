import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, within } from "storybook/test";

import { PageTabs } from "@molecules";

const meta = {
  title: "Example/PageTabs",
  component: PageTabs,
  parameters: {
    layout: "padded",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof PageTabs>;

export default meta;
type Story = StoryObj<typeof meta>;

export const GovernanceActions: Story = {
  args: {
    tabs: [
      {
        label: "Proposed Governance Actions",
        to: "/proposals",
        dataTestId: "proposed-governance-actions-link",
        isActive: false,
      },
      {
        label: "Live Voting",
        to: "/actions",
        dataTestId: "governance-actions-link",
        isActive: true,
      },
      {
        label: "Outcomes",
        to: "/actions/history",
        dataTestId: "governance-actions-governance-actions-link",
        isActive: false,
      },
    ],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("governance-actions-link")).toHaveAttribute(
      "aria-current",
      "page",
    );
  },
};
