import { MetadataValidationStatus } from "@models";
import { expect, jest } from "@storybook/jest";
import type { Meta, StoryObj } from "@storybook/react";
import { screen, userEvent, waitFor, within } from "@storybook/testing-library";
import { formatDisplayDate } from "@utils";
import { GovernanceActionCard } from "@/components/molecules";

const meta = {
  title: "Example/GovernanceActionCard",
  component: GovernanceActionCard,
  parameters: {
    layout: "centered",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof GovernanceActionCard>;

export default meta;

type Story = StoryObj<typeof meta>;

const commonArgs = {
  about: "About this Governance Action",
  createdDate: "1970-01-01T00:00:00Z",
  createdEpochNo: 302,
  expiryDate: "1970-02-01T00:00:00Z",
  expiryEpochNo: 420,
  index: 2,
  inProgress: false,
  onClick: jest.fn(),
  title: "Example title",
  txHash: "sad78afdsf7jasd98d",
  type: "exampleType",
  metadataValid: true,
  metadataStatus: null,
  dRepYesVotes: 1,
  dRepNoVotes: 0,
  dRepAbstainVotes: 0,
  ccYesVotes: 0,
  ccNoVotes: 0,
  ccAbstainVotes: 0,
  poolYesVotes: 0,
  poolNoVotes: 0,
  poolAbstainVotes: 0,
  protocolParams: null,
};

export const GovernanceActionCardComponent: Story = {
  args: commonArgs,

  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByTestId("exampleType-type")).toBeInTheDocument();
    expect(canvas.getByTestId("sad78afdsf7jasd98d#2-id")).toBeInTheDocument();
    expect(
      canvas.getByText(formatDisplayDate("1970-01-01T00:00:00Z")),
    ).toBeInTheDocument();
    expect(
      canvas.getByText(formatDisplayDate("1970-02-01T00:00:00Z")),
    ).toBeInTheDocument();
    const tooltips = canvas.getAllByTestId("InfoOutlinedIcon");
    await userEvent.hover(tooltips[0]);
    await waitFor(async () => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveTextContent(/Submission Date/i);
      await userEvent.unhover(tooltips[0]);
    });
    await userEvent.hover(tooltips[1]);
    await waitFor(() => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveTextContent(/Expiry Date/i);
    });
    await userEvent.click(
      canvas.getByTestId("govaction-sad78afdsf7jasd98d#2-view-detail"),
    );
    await expect(args.onClick).toHaveBeenCalled();
  },
};

export const GovernanceActionCardIsLoading: Story = {
  args: {
    ...commonArgs,
    inProgress: true,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText(/in progress/i)).toBeVisible();
  },
};

export const GovernanceActionCardDataMissing: Story = {
  args: {
    ...commonArgs,
    metadataStatus: MetadataValidationStatus.URL_NOT_FOUND,
    metadataValid: false,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const tooltips = canvas.getAllByTestId("InfoOutlinedIcon");

    await expect(canvas.getByText("Data Missing")).toBeVisible();
    await userEvent.hover(tooltips[0]);
    await waitFor(async () => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveTextContent(/Data Missing/i);
      await userEvent.unhover(tooltips[0]);
    });
  },
};

export const GovernanceActionCardIncorectFormat: Story = {
  args: {
    ...commonArgs,
    metadataStatus: MetadataValidationStatus.INVALID_JSONLD,
    metadataValid: false,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const tooltips = canvas.getAllByTestId("InfoOutlinedIcon");

    await expect(canvas.getByText("Data Formatted Incorrectly")).toBeVisible();
    await userEvent.hover(tooltips[0]);
    await waitFor(async () => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveTextContent(
        /Data Formatted Incorrectly/i,
      );
      await userEvent.unhover(tooltips[0]);
    });
  },
};

export const GovernanceActionCardNotVerifiable: Story = {
  args: {
    ...commonArgs,
    metadataStatus: MetadataValidationStatus.INVALID_HASH,
    metadataValid: false,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const tooltips = canvas.getAllByTestId("InfoOutlinedIcon");

    await expect(canvas.getByText("Data Not Verifiable")).toBeVisible();
    await userEvent.hover(tooltips[0]);
    await waitFor(async () => {
      expect(screen.getByRole("tooltip")).toBeInTheDocument();
      expect(screen.getByRole("tooltip")).toHaveTextContent(
        /Data Not Verifiable/i,
      );
      await userEvent.unhover(tooltips[0]);
    });
  },
};
