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

const Template: StoryFn<ComponentProps<typeof Field.Input>> = (args) => <Field.Input placeholder="Placeholder-auto" {...args} />;

export const Default = Template.bind({});

Default.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  const inputElement = canvas.getByPlaceholderText("Placeholder");
  await userEvent.type(inputElement, "test");
  await expect(inputElement).toHaveValue("test");
};

export const WithLabel = Template.bind({});
WithLabel.args = {
  label: "Label",
};

WithLabel.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await expect(canvas.getByText("Label")).toBeInTheDocument();
};

export const Error = Template.bind({});
Error.args = {
  errorMessage: "Error message",
};

Error.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await expect(canvas.getByTestId("error-message-error")).toBeInTheDocument();
};

export const ErrorAndLabel = Template.bind({});
ErrorAndLabel.args = {
  errorMessage: "Error message",
  label: "Label",
};

ErrorAndLabel.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await expect(canvas.getByText("Label")).toBeInTheDocument();
  await expect(canvas.getByTestId("error-message-error")).toBeInTheDocument();
};

export const WithHelpfulText = Template.bind({});
WithHelpfulText.args = {
  helpfulText: "Helpful text",
};

export const WithAllProps = Template.bind({});
WithAllProps.args = {
  label: "Label",
  helpfulText: "Helpful text",
  errorMessage: "Error message",
};
