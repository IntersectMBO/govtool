import {
  expect,
  screen,
  userEvent,
  waitFor,
  within,
  fn,
} from "storybook/test";
import type { Meta, StoryObj } from "@storybook/react-vite";

import { formatDisplayDate, getProposalTypeNoEmptySpaces } from "@utils";
import { GovernanceActionCard } from "@/components/molecules";
import { GovernanceActionType } from "@/types/governanceAction";
import { MetadataValidationStatus } from "@/models";

const meta = {
  title: "Example/GovernanceActionCard",
  component: GovernanceActionCard,
  parameters: {
    layout: "padded",
  },
  tags: ["autodocs"],
} satisfies Meta<typeof GovernanceActionCard>;

export default meta;

type Story = StoryObj<typeof meta>;

const commonArgs: Story["args"] = {
  abstract: "About this Governance Action",
  createdDate: "1970-01-01T00:00:00Z",
  createdEpochNo: 302,
  expiryDate: "1970-02-01T00:00:00Z",
  expiryEpochNo: 420,
  index: 2,
  inProgress: false,
  onClick: fn(),
  title: "Example title",
  txHash: "sad78afdsf7jasd98d",
  type: GovernanceActionType.InfoAction,
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
  prevGovActionIndex: null,
  prevGovActionTxHash: null,
  metadataHash: "exampleMetadataHash",
  url: "https://exampleMetadataUrl.com",
};

export const GovernanceActionCardComponent: Story = {
  args: commonArgs,

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(
      canvas.getByTestId(
        `${getProposalTypeNoEmptySpaces(GovernanceActionType.InfoAction)}-type`,
      ),
    ).toBeInTheDocument();
    expect(canvas.getByText("Example title")).toBeVisible();
    expect(canvas.getByText("Epoch 420")).toBeVisible();
    ["drep", "spo", "cc"].forEach((role) =>
      expect(
        canvas.getByTestId(`govaction-sad78afdsf7jasd98d#2-${role}-tally`),
      ).toBeInTheDocument(),
    );
    const expiryDate = formatDisplayDate(
      "1970-02-01T00:00:00Z",
      "do MMM yyyy, HH:mm",
    );
    await userEvent.hover(canvas.getByLabelText(expiryDate));
    await waitFor(() => {
      expect(screen.getByRole("tooltip")).toHaveTextContent(expiryDate);
    });
    await expect(
      canvas.getByTestId("govaction-sad78afdsf7jasd98d#2-view-detail"),
    ).toBeVisible();
  },
};

export const GovernanceActionCardIsLoading: Story = {
  args: { ...commonArgs, inProgress: true },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText(/in progress/i)).toBeVisible();
  },
};

export const GovernanceActionCardDataMissing: Story = {
  args: {
    ...commonArgs,
    metadataStatus: MetadataValidationStatus.URL_NOT_FOUND,
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
    metadataStatus: MetadataValidationStatus.INCORRECT_FORMAT,
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
