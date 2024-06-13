import { expect } from "@storybook/jest";
import type { Meta, StoryFn } from "@storybook/react";

import { Field } from "@molecules";
import { within } from "@storybook/testing-library";
import { ComponentProps } from "react";

const meta = {
  title: "Example/Checkbox",
  component: Field.Checkbox,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof Field.Checkbox>;

export default meta;

const Template: StoryFn<ComponentProps<typeof Field.Checkbox>> = (args) => (
  <Field.Checkbox {...args} />
);

export const Default = Template.bind({});
Default.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  expect(canvas.getByRole("checkbox")).not.toBeChecked();
};

export const WithLabel = Template.bind({});
WithLabel.args = {
  label: "Label",
};
WithLabel.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  expect(canvas.getByRole("checkbox")).not.toBeChecked();
  expect(canvas.getByText("Label")).toBeVisible();
};

export const Error = Template.bind({});
Error.args = {
  errorMessage: "Error message",
};
Error.play = async ({ canvasElement, args }) => {
  const canvas = within(canvasElement);
  const errorId = args.errorMessage!.toLowerCase().split(" ").join("-");

  expect(canvas.getByRole("checkbox")).not.toBeChecked();
  expect(canvas.getByTestId(`${errorId}-error`)).toBeVisible();
};

export const ErrorAndLabel = Template.bind({});
ErrorAndLabel.args = {
  errorMessage: "Error message",
  label: "Label",
};
ErrorAndLabel.play = async ({ canvasElement, args }) => {
  const canvas = within(canvasElement);
  const errorId = args.errorMessage!.toLowerCase().split(" ").join("-");

  expect(canvas.getByRole("checkbox")).not.toBeChecked();
  expect(canvas.getByText("Label")).toBeVisible();
  expect(canvas.getByTestId(`${errorId}-error`)).toBeVisible();
};
