import { Button } from "@atoms";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";
import { expect, jest } from "@storybook/jest";
import { Meta, StoryObj } from "@storybook/react";

import { Field, Step } from "@molecules";
import { userEvent, within } from "@storybook/testing-library";

const meta: Meta<typeof Step> = {
  title: "Example/Step",
  component: Step,
  parameters: {
    layout: "centered",
  },
};

export default meta;

const infoMock = jest.fn();
export const WithButton: StoryObj<typeof Step> = {
  args: {
    label: "Download this file",
    stepNumber: 1,
    component: (
      <Button
        onClick={infoMock}
        size="extraLarge"
        sx={{ width: "fit-content" }}
      >
        Info.jsonld
      </Button>
    ),
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);

    await expect(canvas.getByText(args.stepNumber)).toBeVisible();
    await expect(canvas.getByText(args.label)).toBeVisible();
    await userEvent.click(canvas.getByRole("button"));

    await expect(infoMock).toHaveBeenCalled();
  },
};

const readFullGuideMock = jest.fn();
export const WithIconButton: StoryObj<typeof Step> = {
  args: {
    label:
      "Save this file in a location that provides a public URL (e.g. github)",
    stepNumber: 2,
    component: (
      <Button
        onClick={readFullGuideMock}
        endIcon={
          <OpenInNewIcon
            sx={{
              color: "primary",
              height: 17,
              width: 17,
            }}
          />
        }
        size="extraLarge"
        sx={{ width: "fit-content" }}
        variant="text"
      >
        Read full guide
      </Button>
    ),
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);

    await expect(canvas.getByText(args.stepNumber)).toBeVisible();
    await expect(canvas.getByText(args.label)).toBeVisible();
    await userEvent.click(canvas.getByRole("button"));

    await expect(readFullGuideMock).toHaveBeenCalled();
  },
};

export const WithInput: StoryObj<typeof Step> = {
  args: {
    label:
      "Save this file in a location that provides a public URL (e.g. github)",
    stepNumber: 2,
    component: (
      <Field.Input dataTestId="url-input" name="storingURL" placeholder="URL" />
    ),
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);

    await expect(canvas.getByText(args.stepNumber)).toBeVisible();
    await expect(canvas.getByText(args.label)).toBeVisible();
    await expect(canvas.getByTestId("url-input")).toBeVisible();
  },
};
