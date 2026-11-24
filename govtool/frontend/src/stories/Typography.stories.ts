import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, within } from "storybook/test";

import { Typography } from "@atoms";

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
    await expect(
      canvas.getByText("Lorem ipsum dolor sit amet. 123 !@#"),
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

export const Heading1: Story = { args: { variant: "heading1" } };

export const Heading3: Story = { args: { variant: "heading3" } };

export const Heading4: Story = { args: { variant: "heading4" } };

export const Heading5: Story = { args: { variant: "heading5" } };

export const Heading6: Story = { args: { variant: "heading6" } };

export const ParagraphXL: Story = { args: { variant: "paragraphXL" } };

export const ParagraphLG: Story = { args: { variant: "paragraphLG" } };

export const ParagraphMD: Story = { args: { variant: "paragraphMD" } };

export const ParagraphNormal: Story = { args: { variant: "paragraphNormal" } };

export const ParagraphBase: Story = { args: { variant: "paragraphBase" } };

export const ParagraphSM: Story = { args: { variant: "paragraphSM" } };

export const ParagraphXS: Story = { args: { variant: "paragraphXS" } };

export const Tagline: Story = { args: { variant: "tagline" } };
