import type { Meta, StoryObj } from "@storybook/react";

import {
  expect,
  screen,
  userEvent,
  waitFor,
  within,
  fn,
} from "@storybook/test";

import { ActionRadio } from "@atoms";

const meta = {
  title: "Example/ActionRadio",
  component: ActionRadio,
  args: {
    dataTestId: "radio",
    onChange: fn(),
  },
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof ActionRadio>;

export default meta;
type Story = StoryObj<typeof meta>;

export const ActionRadioComponent: Story = {
  args: {
    subtitle: "Example subtitle",
    title: "Title",
    isChecked: false,
    tooltipText: "Example tooltip text",
    tooltipTitle: "Tooltip title",
    value: "",
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByText("Title")).toBeInTheDocument();
    expect(canvas.getByText("Example subtitle")).toBeInTheDocument();
    await expect(canvas.getByTestId("radio")).toHaveAttribute(
      "aria-checked",
      "false",
    );
    await userEvent.hover(canvas.getByTestId("InfoOutlinedIcon"));
    await waitFor(() => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveTextContent("Tooltip title");
      expect(screen.getByRole("tooltip")).toHaveTextContent(
        "Example tooltip text",
      );
    });
    await userEvent.click(canvas.getByTestId("radio"));
    await expect(args.onChange).toHaveBeenCalled();
  },
};

export const ActionRadioActive: Story = {
  args: {
    subtitle: "Example subtitle",
    title: "Title",
    isChecked: true,
    tooltipText: "Example tooltip text",
    tooltipTitle: "Tooltip title",
    value: "",
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByText("Title")).toBeInTheDocument();
    expect(canvas.getByText("Example subtitle")).toBeInTheDocument();

    await userEvent.hover(canvas.getByTestId("InfoOutlinedIcon"));
    await waitFor(() => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveTextContent("Tooltip title");
      expect(screen.getByRole("tooltip")).toHaveTextContent(
        "Example tooltip text",
      );
    });

    await userEvent.click(canvas.getByTestId("radio"));
    await expect(args.onChange).toHaveBeenCalled();

    await expect(canvas.getByTestId("radio")).toHaveAttribute(
      "aria-checked",
      "true",
    );
  },
};

export const ActionRadioOnlyTitle: Story = {
  args: {
    title: "Title",
    value: "",
    isChecked: false,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("radio")).toHaveAttribute(
      "aria-checked",
      "false",
    );
  },
};

export const ActionRadioOnlyTitleChecked: Story = {
  args: {
    title: "Title",
    value: "",
    isChecked: true,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("radio")).toHaveAttribute(
      "aria-checked",
      "true",
    );
  },
};
