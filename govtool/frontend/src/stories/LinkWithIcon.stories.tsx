import { ICONS } from "@consts";
import { LinkWithIcon } from "@molecules";
import { expect, jest } from "@storybook/jest";
import { Meta, StoryObj } from "@storybook/react";
import { userEvent, within } from "@storybook/testing-library";

const meta: Meta<typeof LinkWithIcon> = {
  title: "Example/LinkWithIcon",
  component: LinkWithIcon,
  args: {
    onClick: jest.fn(),
  },

  parameters: {
    layout: "centered",
  },
};

export default meta;

export const Default: StoryObj<typeof LinkWithIcon> = {
  args: {
    label: "Default Link",
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);

    await expect(canvas.getByTestId("ArrowBackIosIcon")).toBeVisible();
    await userEvent.click(
      canvas.getByTestId(`${args.label.replace(/\s/g, "-")}-link`),
    );
    await expect(args.onClick).toHaveBeenCalled();
  },
};

export const WithCustomIcon: StoryObj<typeof LinkWithIcon> = {
  args: {
    label: "Custom Icon Link",
    icon: <img alt="custom icon" src={ICONS.link} />,
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);

    await expect(canvas.getByRole("img")).toHaveAttribute("alt", "custom icon");
    await userEvent.click(
      canvas.getByTestId(`${args.label.replace(/\s/g, "-")}-link`),
    );
    await expect(args.onClick).toHaveBeenCalled();
  },
};
