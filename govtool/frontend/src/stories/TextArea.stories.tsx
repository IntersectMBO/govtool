import { expect } from "@storybook/jest";
import type { Meta, StoryFn } from "@storybook/react";

import { Field } from "@molecules";
import { userEvent, within } from "@storybook/testing-library";
import { ComponentProps, useState } from "react";

const meta: Meta<typeof Field.TextArea> = {
  title: "Example/TextArea",
  component: Field.TextArea,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
};

export default meta;

const Template: StoryFn<ComponentProps<typeof Field.TextArea>> = (args) => {
  const [val, setVal] = useState("");

  return (
    <Field.TextArea
      {...args}
      value={val}
      onChange={(e) => setVal(e.target.value)}
    />
  );
};

async function assertTextbox(canvas: ReturnType<typeof within>) {
  const text = "test";
  const inputElement = canvas.getByRole("textbox");
  await userEvent.type(inputElement, text);
  await expect(inputElement).toHaveValue(text);
  await expect(canvas.getByText(`${text.length}/500`)).toBeVisible();
}

export const Default = Template.bind({});
Default.play = async ({ canvasElement }) => {
  const canvas = within(canvasElement);
  await assertTextbox(canvas);
};

export const WithLabel = Template.bind({});
WithLabel.args = {
  label: "Label",
};
WithLabel.play = async ({ canvasElement, args }) => {
  const canvas = within(canvasElement);
  await assertTextbox(canvas);

  await expect(canvas.getByText(args.label!)).toBeVisible();
};

export const WithHelpfulText = Template.bind({});
WithHelpfulText.args = {
  helpfulText: "Helpful text here",
};
WithHelpfulText.play = async ({ canvasElement, args }) => {
  const canvas = within(canvasElement);
  await assertTextbox(canvas);

  await expect(canvas.getByText(args.helpfulText!)).toBeVisible();
};

export const Error = Template.bind({});
Error.args = {
  errorMessage: "Error message",
};
Error.play = async ({ canvasElement, args }) => {
  const canvas = within(canvasElement);

  await assertTextbox(canvas);
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

  await assertTextbox(canvas);

  await expect(canvas.getByText(args.label!)).toBeVisible();
  await expect(
    canvas.getByTestId(
      `${args.errorMessage!.replace(/\s+/g, "-").toLowerCase()}-error`,
    ),
  ).toBeVisible();
};

export const WithAllProps = Template.bind({});
WithAllProps.args = {
  label: "Label",
  helpfulText: "Helpful text",
  errorMessage: "Error message",
};
WithAllProps.play = async ({ canvasElement, args }) => {
  const canvas = within(canvasElement);

  await assertTextbox(canvas);

  await expect(canvas.getByText(args.label!)).toBeVisible();
  await expect(
    canvas.getByTestId(
      `${args.errorMessage!.replace(/\s+/g, "-").toLowerCase()}-error`,
    ),
  ).toBeVisible();
  await expect(canvas.getByText(args.helpfulText!)).toBeVisible();
};
