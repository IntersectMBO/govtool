import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, within } from "storybook/test";

import { CenteredBoxPageWrapper } from "@molecules";
import { RolesAndResponsibilities } from "@organisms";

const meta = {
  title: "Example/FlowFrame",
  component: CenteredBoxPageWrapper,
  parameters: { layout: "fullscreen" },
  tags: ["autodocs"],
} satisfies Meta<typeof CenteredBoxPageWrapper>;

export default meta;
type Story = StoryObj<typeof meta>;

export const DRepRegistrationFirstStep: Story = {
  args: {
    pageTitle: "DRep registration",
    stepper: {
      steps: ["Understand", "Profile", "Prepare file", "Publish"],
      current: 1,
    },
    children: <RolesAndResponsibilities setStep={() => {}} />,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByTestId("flow-stepper")).toHaveTextContent("Step 1 of 4");
    await expect(canvas.getByRole("heading", { level: 1 })).toBeVisible();
    await expect(canvas.getByTestId("continue-button")).toBeVisible();
  },
};
