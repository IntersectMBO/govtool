import type { Meta, StoryFn } from "@storybook/react";

import { Field } from "@molecules";
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

const Template: StoryFn<ComponentProps<typeof Field.Checkbox>> = (args) => {
  return <Field.Checkbox {...args} />;
};

export const Default = Template.bind({});

export const WithLabel = Template.bind({});
WithLabel.args = {
  label: "Label",
};

export const Error = Template.bind({});
Error.args = {
  errorMessage: "Error message",
};

export const ErrorAndLabel = Template.bind({});
ErrorAndLabel.args = {
  errorMessage: "Error message",
  label: "Label",
};
