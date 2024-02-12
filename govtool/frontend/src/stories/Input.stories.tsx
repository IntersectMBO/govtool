import type { Meta, StoryFn } from "@storybook/react";

import { Field } from "@molecules";
import { ComponentProps } from "react";
import { userEvent, within } from "@storybook/testing-library";
import { expect } from "@storybook/jest";

const meta = {
  title: "Example/Input",
  component: Field.Input,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof Field.Input>;

export default meta;

const Template: StoryFn<ComponentProps<typeof Field.Input>> = (args) => {
  return <Field.Input placeholder="Placeholder-auto" {...args} />;
};

export const Default = Template.bind({});

Default.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  const inputElement = canvas.getByPlaceholderText("Placeholder");
  await userEvent.type(inputElement, "test");
  expect(inputElement).toHaveValue("test");
};

export const WithLabel = Template.bind({});
WithLabel.args = {
  label: "Label",
};

WithLabel.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  expect(canvas.getByText("Label")).toBeInTheDocument();
};

export const Error = Template.bind({});
Error.args = {
  errorMessage: "Error message",
};

Error.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  expect(canvas.getByTestId("error-message-error")).toBeInTheDocument();
};

export const ErrorAndLabel = Template.bind({});
ErrorAndLabel.args = {
  errorMessage: "Error message",
  label: "Label",
};

ErrorAndLabel.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  expect(canvas.getByText("Label")).toBeInTheDocument();
  expect(canvas.getByTestId("error-message-error")).toBeInTheDocument();
};
