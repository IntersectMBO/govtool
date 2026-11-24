import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, fn, screen, userEvent, waitFor } from "storybook/test";

import { DelegationReviewDialog } from "@organisms";

const meta = {
  title: "Example/DelegationReviewDialog",
  component: DelegationReviewDialog,
  parameters: { layout: "centered" },
  tags: ["autodocs"],
} satisfies Meta<typeof DelegationReviewDialog>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Review: Story = {
  args: {
    dRep: {
      drepId: "abc123",
      view: "drep1examplexyz000000",
      givenName: "Jordan Dyer",
      isScriptBased: false,
    },
    onClose: fn(),
    onConfirm: fn(async () => {}),
  },
  play: async ({ args }) => {
    // The dialog fades in.
    await waitFor(() =>
      expect(
        screen.getByRole("heading", { name: "Review voting delegation" }),
      ).toBeVisible(),
    );
    await expect(screen.getByText("Jordan Dyer")).toBeVisible();
    await userEvent.click(screen.getByTestId("continue-to-wallet-button"));
    await expect(args.onConfirm).toHaveBeenCalled();
    // Nothing became pending, so the dialog hands back to the page.
    await waitFor(() => expect(args.onClose).toHaveBeenCalled());
  },
};
