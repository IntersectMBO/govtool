import type { Meta, StoryObj } from "@storybook/react";

import { expect, jest } from "@storybook/jest";
import { userEvent, within } from "@storybook/testing-library";
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

    const nowDate = new Date();
    await expect(
      canvas.getByText(`© ${nowDate.getFullYear()} Intersect MBO`),
    ).toBeInTheDocument();
    await userEvent.click(canvas.getByTestId("privacy-policy-footer-link"));
    await expect(window.open).toHaveBeenCalledTimes(1);

    await userEvent.click(
      canvas.getByTestId("terms-and-conditions-footer-link"),
    );
    await expect(window.open).toHaveBeenCalledTimes(2);

    await userEvent.click(canvas.getByTestId("help-footer-button"));
    await expect(window.open).toHaveBeenCalledTimes(3);

    await expect(canvas.getByTestId("feedback-footer-button")).toBeEnabled();
  },
};
