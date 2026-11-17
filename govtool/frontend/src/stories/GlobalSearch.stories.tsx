import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, screen, userEvent, waitFor, within } from "storybook/test";

import { GlobalSearchButton } from "@organisms";
import { SnackbarProvider } from "@/context/snackbar";

const meta = {
  title: "Example/GlobalSearch",
  component: GlobalSearchButton,
  parameters: { layout: "centered" },
  // The dialog's copy action raises a snackbar; the app provides it globally.
  decorators: [(Story) => <SnackbarProvider><Story /></SnackbarProvider>],
  tags: ["autodocs"],
} satisfies Meta<typeof GlobalSearchButton>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Closed: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByRole("button", { name: "Global Search" })).toBeVisible();
  },
};

export const OpenOnBrowse: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByTestId("global-search-button"));
    // The dialog renders in a portal outside the story's canvas.
    await waitFor(() =>
      expect(screen.getByRole("dialog", { name: "Find anything in GovTool" })).toBeVisible(),
    );
    await expect(screen.getByTestId("global-search-input")).toHaveFocus();
    await expect(screen.getByText("Browse by type")).toBeVisible();
  },
};
