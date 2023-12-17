import type { Meta, StoryObj } from "@storybook/react";
import { screen, userEvent, waitFor, within } from "@storybook/testing-library";
import { GovernanceActionDetailsCard } from "@organisms";
import { expect } from "@storybook/jest";

const meta = {
  title: "Example/GovernanceActionDetailsCard",
  component: GovernanceActionDetailsCard,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof GovernanceActionDetailsCard>;

export default meta;
type Story = StoryObj<typeof meta>;

export const GovernanceActionDetailsCardComponent: Story = {
  args: {
    abstainVotes: 1000000,
    createdDate: new Date().toLocaleDateString(),
    expiryDate: new Date().toLocaleDateString(),
    shortenedGovActionId: "Example id",
    noVotes: 1000000,
    type: "Gov Type",
    url: "https://exampleurl.com",
    yesVotes: 1000000,
    details: {
      key: "value",
      key2: ["key-list", "key-list", "key-list"],
    },
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const todayDate = new Date().toLocaleDateString();
    await expect(canvas.getAllByText(todayDate)).toHaveLength(2);

    const tooltips = canvas.getAllByTestId("InfoOutlinedIcon");
    await userEvent.hover(tooltips[0]);
    await waitFor(async () => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveTextContent(/Submission Date/i);
      await userEvent.unhover(tooltips[0]);
    });
    await userEvent.hover(tooltips[1]);

    expect(canvas.getByText("Gov Type")).toBeInTheDocument();
    expect(canvas.getByText("Example id")).toBeInTheDocument();

    await expect(canvas.getByText("key: value")).toBeInTheDocument();
    await expect(canvas.getAllByText("key-list")).toHaveLength(3);

    await expect(canvas.getByText(/Yes/i)).toBeInTheDocument();
    await expect(canvas.getByText(/Abstain/i)).toBeInTheDocument();
    await expect(canvas.getByText(/No/i)).toBeInTheDocument();

    await userEvent.click(canvas.getByTestId("view-other-details-button"));
  },
};
