import type { Meta, StoryObj } from "@storybook/react";

import { userEvent, within } from "@storybook/testing-library";
import { expect, jest } from "@storybook/jest";
import { Footer } from "@/components/organisms";

const meta = {
  title: "Example/Footer",
  component: Footer,
  tags: ["autodocs"],
} satisfies Meta<typeof Footer>;

export default meta;
type Story = StoryObj<typeof meta>;

export const FooterComponent: Story = {
  args: {},
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    window.open = jest.fn();

    await expect(
      canvas.getByText(/ 2023 Voltaire Gov Tool/i),
    ).toBeInTheDocument();
    await userEvent.click(canvas.getByTestId("privacy-policy-link"));
    await expect(window.open).toHaveBeenCalledTimes(1);
  },
};
