import type { Meta, StoryFn } from "@storybook/react";

import { Field } from "@molecules";
import { expect, userEvent, within } from "@storybook/test";
import { ComponentProps } from "react";

const meta = {
  title: "Example/Input",
  component: Field.Input,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof Field.Input>;

export default meta;

const Template: StoryFn<ComponentProps<typeof Field.Input>> = (args) => (
  <Field.Input placeholder="Placeholder" {...args} />
);

export const Default = Template.bind({});

async function assertInput(canvas: ReturnType<typeof within>) {
  const inputElement = canvas.getByPlaceholderText("Placeholder");
  await userEvent.type(inputElement, "test");
  await expect(inputElement).toHaveValue("test");
}

Default.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await assertInput(canvas);
};

export const WithLabel = Template.bind({});
WithLabel.args = {
  label: "Label",
};

WithLabel.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);

  await assertInput(canvas);
  await expect(canvas.getByText("Label")).toBeInTheDocument();
};

export const Error = Template.bind({});
Error.args = {
  errorMessage: "Error message",
};

Error.play = async ({ canvasElement, args }) => {
  const canvas = within(canvasElement);

  await assertInput(canvas);
  await expect(
    canvas.getByTestId(
      `${args.errorMessage!.replace(/\s+/g, "-").toLowerCase()}-error`,
    ),
  ).toBeVisible();
};

export const ErrorAndLabel = Template.bind({});
ErrorAndLabel.args = {
  errorMessage: "Error message",
  label: "Label",
};
ErrorAndLabel.play = async ({ canvasElement, args }) => {
  const canvas = within(canvasElement);

  await assertInput(canvas);
  await expect(canvas.getByText("Label")).toBeInTheDocument();
  await expect(
    canvas.getByTestId(
      `${args.errorMessage!.replace(/\s+/g, "-").toLowerCase()}-error`,
    ),
  ).toBeVisible();
};

export const WithHelpfulText = Template.bind({});
WithHelpfulText.args = {
  helpfulText: "Helpful text",
};
WithHelpfulText.play = async ({ canvasElement, args }) => {
  const canvas = within(canvasElement);

  await assertInput(canvas);
  await expect(canvas.getByText(args.helpfulText!)).toBeVisible();
};

export const WithAllProps = Template.bind({});
WithAllProps.args = {
  label: "Label",
  helpfulText: "Helpful text",
  errorMessage: "Error message",
};
WithAllProps.play = async ({ canvasElement, args }) => {
  const canvas = within(canvasElement);

  await assertInput(canvas);
  await expect(canvas.getByText("Label")).toBeInTheDocument();
  await expect(
    canvas.getByTestId(
      `${args.errorMessage!.replace(/\s+/g, "-").toLowerCase()}-error`,
    ),
  ).toBeVisible();
  await expect(canvas.getByText(args.helpfulText!)).toBeVisible();
};
