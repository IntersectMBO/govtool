import { Meta, StoryObj } from "@storybook/react";
import { Button } from "@atoms";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";

import { Field, Step } from "@molecules";

const meta: Meta<typeof Step> = {
  title: "Example/Step",
  component: Step,
  parameters: {
    layout: "centered",
  },
};

export default meta;

export const WithButton: StoryObj<typeof Step> = {
  args: {
    label: "Download this file",
    stepNumber: 1,
    component: (
      <Button size="extraLarge" sx={{ width: "fit-content" }}>
        Info.jsonld
      </Button>
    ),
  },
};

export const WithIconButton: StoryObj<typeof Step> = {
  args: {
    label:
      "Save this file in a location that provides a public URL (ex. github)",
    stepNumber: 2,
    component: (
      <Button
        endIcon={(
          <OpenInNewIcon
            sx={{
              color: "primary",
              height: 17,
              width: 17,
            }}
          />
        )}
        size="extraLarge"
        sx={{ width: "fit-content" }}
        variant="text"
      >
        Read full guide
      </Button>
    ),
  },
};

export const WithInput: StoryObj<typeof Step> = {
  args: {
    label:
      "Save this file in a location that provides a public URL (ex. github)",
    stepNumber: 2,
    component: <Field.Input name="storingURL" placeholder="URL" />,
  },
};
