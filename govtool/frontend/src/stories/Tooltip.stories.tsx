import type { Meta, StoryObj } from "@storybook/react";
import { Box, Typography } from "@mui/material";
import {
  screen, userEvent, waitFor, within,
} from "@storybook/testing-library";
import { expect } from "@storybook/jest";
import { Tooltip } from "@/components/atoms";

const defaultChildren = (
  <Box>
    <Typography>Hover to show tooltip</Typography>
  </Box>
);

const meta = {
  title: "Example/Tooltip",
  component: Tooltip,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
  args: {
    children: defaultChildren,
    placement: "top",
  },
} satisfies Meta<typeof Tooltip>;

export default meta;

type Story = StoryObj<typeof meta>;

const hoverTooltip = async (canvas: any) => {
  const tooltip = canvas.getByText(/hover to show tooltip/i);
  await userEvent.hover(tooltip);
};

export const BasicTooltip: Story = {
  args: {
    heading: "Tooltip Heading",
    paragraphOne: "This is the first paragraph.",
    paragraphTwo: "This is the second paragraph.",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await hoverTooltip(canvas);
    await waitFor(() => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveTextContent("Tooltip Heading");
      expect(screen.getByRole("tooltip")).toHaveTextContent(
        "This is the first paragraph.",
      );
      expect(screen.getByRole("tooltip")).toHaveTextContent(
        "This is the second paragraph.",
      );
    });
  },
};

export const HeadingOnly: Story = {
  args: {
    heading: "Only Heading",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await hoverTooltip(canvas);
    await waitFor(() => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveTextContent("Only Heading");
    });
  },
};

export const OneParagraphOnly: Story = {
  args: {
    paragraphOne: "Only one paragraph without a heading.",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await hoverTooltip(canvas);
    await waitFor(() => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveTextContent(
        "Only one paragraph without a heading.",
      );
    });
  },
};

export const TwoParagraphs: Story = {
  args: {
    paragraphOne: "First paragraph.",
    paragraphTwo: "Second paragraph.",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await hoverTooltip(canvas);
    await waitFor(() => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveTextContent("First paragraph.");
      expect(screen.getByRole("tooltip")).toHaveTextContent(
        "Second paragraph.",
      );
    });
  },
};

export const RightPlacement: Story = {
  args: {
    ...BasicTooltip.args,
    placement: "right",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await hoverTooltip(canvas);
    await waitFor(() => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveAttribute(
        "data-popper-placement",
        "right",
      );
    });
  },
};

export const BottomPlacement: Story = {
  args: {
    ...BasicTooltip.args,
    placement: "bottom",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await hoverTooltip(canvas);
    await waitFor(() => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveAttribute(
        "data-popper-placement",
        "bottom",
      );
    });
  },
};

export const LeftPlacement: Story = {
  args: {
    ...BasicTooltip.args,
    placement: "left",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await hoverTooltip(canvas);
    await waitFor(() => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveAttribute(
        "data-popper-placement",
        "left",
      );
    });
  },
};
