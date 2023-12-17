import type { Meta, StoryObj } from "@storybook/react";

import { DashboardTopNav } from "@organisms";
import { IMAGES } from "@/consts";
import { within, userEvent, waitFor, screen } from "@storybook/testing-library";
import { expect } from "@storybook/jest";

const meta = {
  title: "Example/DashboardTopNav",
  component: DashboardTopNav,
  tags: ["autodocs"],
} satisfies Meta<typeof DashboardTopNav>;

export default meta;
type Story = StoryObj<typeof meta>;

export const DashboardTopNavComponent: Story = {
  args: { title: "Example title", isDrawer: true },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByText("Example title")).toBeInTheDocument();
    expect(canvas.getByText("Voting power:")).toBeInTheDocument();

    expect(canvas.getByTestId("InfoOutlinedIcon")).toBeInTheDocument();
    await userEvent.hover(canvas.getByTestId("InfoOutlinedIcon"));
    await waitFor(() => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveTextContent(
        /DRep Voting Power/i
      );
    });
  },
};

export const DashboardTopNavWithIcon: Story = {
  args: {
    title: "Example title",
    isDrawer: true,
    imageSRC: IMAGES.appLogoWithoutText,
    imageHeight: 24,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByRole("img")).toBeInTheDocument();
  },
};
