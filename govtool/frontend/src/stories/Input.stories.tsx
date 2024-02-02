import type { Meta, StoryFn } from "@storybook/react";

import { Input } from "@atoms";
import { ComponentProps } from "react";
import { userEvent, within } from "@storybook/testing-library";
import { expect } from "@storybook/jest";
import { useUrlAndHashFormController } from "@hooks";

const meta = {
  title: "Example/Input",
  component: Input,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof Input>;

export default meta;

const Template: StoryFn<ComponentProps<typeof Input>> = (args) => {
  const { control } = useUrlAndHashFormController();

  return <Input {...args} control={control} />;
};

export const Default = Template.bind({});
Default.args = {
  placeholder: "Enter URL",
  width: "400px",
  formFieldName: "url",
};

Default.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  const inputElement = canvas.getByPlaceholderText("Enter URL");
  await userEvent.type(inputElement, "test");
  expect(inputElement).toHaveValue("test");
};

export const WithLabel = Template.bind({});
WithLabel.args = {
  label: "Label",
  placeholder: "Enter URL",
  width: "400px",
  formFieldName: "url",
};

WithLabel.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  expect(canvas.getByText("Label")).toBeInTheDocument();
};

export const Error = Template.bind({});
Error.args = {
  placeholder: "Enter URL",
  errorMessage: "Invalid URL format",
  width: "400px",
  formFieldName: "url",
};

Error.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  expect(canvas.getByTestId("invalid-url-format-error")).toBeInTheDocument();
};
