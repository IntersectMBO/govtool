import type { Meta, StoryObj } from "@storybook/react";
import { screen, userEvent, waitFor, within } from "@storybook/testing-library";
import { GovernanceActionDetailsCard } from "@organisms";
import { expect } from "@storybook/jest";
import { MetadataValidationStatus } from "@models";

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

const commonArgs = {
  abstainVotes: 1000000,
  createdDate: new Date().toLocaleDateString(),
  expiryDate: new Date().toLocaleDateString(),
  noVotes: 1000000,
  type: "Gov Type",
  url: "https://exampleurl.com",
  yesVotes: 1000000,
  createdEpochNo: 302,
  expiryEpochNo: 350,
  govActionId: "exampleId1232312312312",
  isDataMissing: false,
  title: "Example title",
};

export const GovernanceActionDetailsCardComponent: Story = {
  args: {
    ...commonArgs,
    about: "Example about section",
    rationale: "Example rationale section",
    motivation: "Example motivation section",
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const todayDate = new Date().toLocaleDateString();
    await expect(canvas.getAllByText(todayDate)).toHaveLength(2);
    const tooltips = canvas.getAllByTestId("InfoOutlinedIcon");
    await userEvent.hover(tooltips[0]);
    await waitFor(async () => {
      await expect(screen.getByRole("tooltip")).toBeInTheDocument();
      await expect(screen.getByRole("tooltip")).toHaveTextContent(
        /Submission Date/i,
      );
      await userEvent.unhover(tooltips[0]);
    });
    await userEvent.hover(tooltips[1]);
    await expect(canvas.getByText("Gov Type")).toBeInTheDocument();
    await expect(canvas.getByText("Example id")).toBeInTheDocument();
    await expect(canvas.getByText("key: value")).toBeInTheDocument();
    await expect(canvas.getAllByText("key-list")).toHaveLength(3);
    await expect(canvas.getByText(/Yes/i)).toBeInTheDocument();
    await expect(canvas.getByText(/Abstain/i)).toBeInTheDocument();
    await expect(canvas.getByText(/No/i)).toBeInTheDocument();
    await userEvent.click(canvas.getByTestId("view-other-details-button"));
  },
};

export const GovernanceActionDetailsDrep: Story = {
  args: {
    ...commonArgs,
    isDashboard: true,
    isVoter: true,
    about: "Example about section",
    rationale: "Example rationale section",
    motivation: "Example motivation section",
  },
};

export const GovernanceActionDetailsDataMissing: Story = {
  args: {
    ...commonArgs,
    isDataMissing: MetadataValidationStatus.URL_NOT_FOUND,
  },
};

export const GovernanceActionDetailsIncorrectFormat: Story = {
  args: {
    ...commonArgs,
    isDataMissing: MetadataValidationStatus.INVALID_JSONLD,
  },
};

export const GovernanceActionDetailsNotVerifiable: Story = {
  args: {
    ...commonArgs,
    isDataMissing: MetadataValidationStatus.INVALID_HASH,
  },
};
