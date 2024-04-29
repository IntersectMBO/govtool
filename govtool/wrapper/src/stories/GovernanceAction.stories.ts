import type { Meta, StoryObj } from "@storybook/react";
import { within, userEvent, waitFor, screen } from "@storybook/testing-library";
import { expect, jest } from "@storybook/jest";
import { formatDisplayDate } from "@utils";
import { MetadataValidationStatus } from "@models";
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
  isDataMissing: false,
  onClick: jest.fn(),
  title: "Example title",
  txHash: "sad78afdsf7jasd98d",
  type: "exampleType",
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
};

export const GovernanceActionCardDataMissing: Story = {
  args: {
    ...commonArgs,
    isDataMissing: MetadataValidationStatus.URL_NOT_FOUND,
  },
};

export const GovernanceActionCardIncorectFormat: Story = {
  args: {
    ...commonArgs,
    isDataMissing: MetadataValidationStatus.INVALID_JSONLD,
  },
};

export const GovernanceActionCardNotVerifiable: Story = {
  args: {
    ...commonArgs,
    isDataMissing: MetadataValidationStatus.INVALID_HASH,
  },
};
