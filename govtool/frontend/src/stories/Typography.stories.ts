import type { Meta, StoryObj } from "@storybook/react";

import { Typography } from "@atoms";
import { within } from "@storybook/testing-library";
import { expect } from "@storybook/jest";

const meta = {
  title: "Example/Typography",
  component: Typography,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
  args: {
    children: "Lorem ipsum dolor sit amet. 123 !@#",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(
      canvas.getByText("Lorem ipsum dolor sit amet. 123 !@#")
    ).toBeInTheDocument();
  },
} satisfies Meta<typeof Typography>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Headline1: Story = {
  args: {
    variant: "headline1",
  },
};

export const Headline2: Story = {
  args: {
    variant: "headline2",
  },
};

export const Headline3: Story = {
  args: {
    variant: "headline3",
  },
};

export const Headline4: Story = {
  args: {
    variant: "headline4",
  },
};

export const Headline5: Story = {
  args: {
    variant: "headline5",
  },
};

export const Title1: Story = {
  args: {
    variant: "title1",
  },
};

export const Title2: Story = {
  args: {
    variant: "title2",
  },
};

export const Body1: Story = {
  args: {
    variant: "body1",
  },
};

export const Body2: Story = {
  args: {
    variant: "body2",
  },
};

export const Caption: Story = {
  args: {
    variant: "caption",
  },
};
