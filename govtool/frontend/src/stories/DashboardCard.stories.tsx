import type { Meta, StoryObj } from "@storybook/react";
import { expect, within } from "@storybook/test";

import { CopyableInfo, DashboardActionCard } from "@molecules";
import { IMAGES } from "@/consts";

const meta = {
  title: "Example/DashboardCard",
  component: DashboardActionCard,
  args: {
    type: "d-rep",
  },
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof DashboardActionCard>;

export default meta;
type Story = StoryObj<typeof meta>;

async function assertCardInfo(canvas: ReturnType<typeof within>) {
  expect(canvas.getByText("Action card")).toBeVisible();
  expect(canvas.queryByText(/lorem/i)).toBeInTheDocument();
  expect(canvas.queryAllByRole("img")).not.toHaveLength(0);
}

export const DashboardCardComponent: Story = {
  args: {
    buttons: [{ children: "first button" }, { children: "second button" }],
    description: "Lorem ipsum dolor sit amet, consectetur adipisicing elit.",
    imageURL: IMAGES.govActionDelegateImage,
    title: "Action card",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await assertCardInfo(canvas);

    const buttons = canvas.getAllByRole("button");
    await expect(buttons[0].textContent).toBe("first button");
    await expect(buttons[1].textContent).toBe("second button");
  },
};

export const WithDRepIdDashboardCardComponent: Story = {
  args: {
    buttons: [
      { children: "first button", variant: "contained" },
      { children: "second button" },
    ],
    description: "Lorem ipsum dolor sit amet, consectetur adipisicing elit.",
    imageURL: IMAGES.govActionDelegateImage,
    state: "active",
    title: "Action card",
    children: (
      <CopyableInfo
        dataTestId="dRep-id-display-card-dashboard"
        label="Your DRep ID"
        sx={{ mt: 1 }}
        value="drep133vmtzlem9asdkl4jfs3f9mrg5jg34tymf4uzwj2nx0fgwyg9ds"
      />
    ),
  },
  play: async ({ canvasElement }) => {
    const dRepId = "drep133vmtzlem9asdkl4jfs3f9mrg5jg34tymf4uzwj2nx0fgwyg9ds";
    const canvas = within(canvasElement);

    await assertCardInfo(canvas);
    await expect(
      canvas.getByTestId("dRep-id-display-card-dashboard"),
    ).toHaveTextContent(dRepId);
  },
};

export const LoadingDashboardCard: Story = {
  args: {
    buttons: [{ children: "first button" }, { children: "second button" }],
    description: "Lorem ipsum dolor sit amet, consectetur adipisicing elit.",
    imageURL: IMAGES.govActionDelegateImage,
    title: "Action card",
    isLoading: true,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    expect(canvas.queryByText("Action card")).not.toBeInTheDocument();
    expect(canvas.queryByText(/lorem/i)).not.toBeInTheDocument();
    await expect(canvas.queryAllByRole("button")).toHaveLength(0);
    expect(canvas.queryByRole("img")).not.toBeInTheDocument();
  },
};

export const InProgressDashboardCard: Story = {
  args: {
    buttons: [{ children: "first button" }, { children: "second button" }],
    description: "Lorem ipsum dolor sit amet, consectetur adipisicing elit.",
    imageURL: IMAGES.govActionDelegateImage,
    title: "Action card",
    state: "inProgress",
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await assertCardInfo(canvas);
    await expect(canvas.getAllByText(/in progress/i)).toHaveLength(2);
  },
};
