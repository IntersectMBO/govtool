import type { Meta, StoryObj } from "@storybook/react-vite";

import { expect, within } from "storybook/test";

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

    const nowDate = new Date();
    await expect(
      canvas.getByText(`© ${nowDate.getFullYear()} GovTool`),
    ).toBeInTheDocument();

    [
      "faqs-link",
      "privacy-policy-footer-link",
      "terms-of-use-footer-link",
      "help-footer-button",
    ].forEach((id) =>
      expect(canvas.getByTestId(id)).toHaveAttribute("target", "_blank"),
    );

    await expect(canvas.getByTestId("feedback-footer-button")).toBeEnabled();
  },
};
