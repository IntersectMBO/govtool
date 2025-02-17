import { ComponentProps } from "react";
import { expect, within } from "@storybook/test";
import type { Meta, StoryFn } from "@storybook/react";

import { Field } from "@molecules";

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

export const ErrorBase = Template.bind({});
ErrorBase.args = {
  errorMessage: "Error message",
};
ErrorBase.play = async ({ canvasElement, args }) => {
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
