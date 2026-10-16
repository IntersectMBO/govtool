import { expect, within } from "storybook/test";
import type { Meta, StoryObj } from "@storybook/react-vite";

import { MaintenanceBanner } from "@/components/organisms/MaintenanceBanner";

const meta = {
  title: "Example/MaintenanceBanner",
  component: MaintenanceBanner,
  args: {
    startsAt: Date.now(),
    endsAt: Date.now() + 60 * 60_000,
  },
  parameters: {
    layout: "fullscreen",
  },
} satisfies Meta<typeof MaintenanceBanner>;

export default meta;
type Story = StoryObj<typeof meta>;

export const OneHour: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(
      canvas.getByText(/GovTool is under maintenance/),
    ).toBeVisible();
    await expect(canvas.getByTestId("system-maintenance-timer")).toBeVisible();
  },
};

export const LastFiveMinutes: Story = {
  args: {
    startsAt: Date.now() - 56 * 60_000,
    endsAt: Date.now() + 4 * 60_000,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(
      canvas.getByText("Almost there, we'll be back soon"),
    ).toBeVisible();
  },
};

export const NoEndTime: Story = {
  args: {
    startsAt: undefined,
    endsAt: undefined,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText(/We'll be back soon\./)).toBeVisible();
  },
};
