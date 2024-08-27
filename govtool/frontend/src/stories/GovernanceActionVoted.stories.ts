import type { Meta, StoryObj } from "@storybook/react";

import { GovernanceVotedOnCard } from "@molecules";
import { userEvent, waitFor, within, screen } from "@storybook/testing-library";
import { expect } from "@storybook/jest";
import { formatDisplayDate, getProposalTypeNoEmptySpaces } from "@/utils";
import { GovernanceActionType } from "@/types/governanceAction";

const meta = {
  title: "Example/GovernanceVotedOnCard",
  component: GovernanceVotedOnCard,
  parameters: {
    layout: "centered",
  },

  tags: ["autodocs"],
} satisfies Meta<typeof GovernanceVotedOnCard>;

export default meta;
type Story = StoryObj<typeof meta>;

async function checkGovActionVisibility(canvas: ReturnType<typeof within>) {
  expect(
    canvas.getByTestId(
      `${getProposalTypeNoEmptySpaces(GovernanceActionType.InfoAction)}-type`,
    ),
  ).toBeInTheDocument();
  expect(canvas.getByTestId("exampleHash#1-id")).toBeInTheDocument();
  expect(canvas.getByText(/Votes submitted/i)).toBeInTheDocument();

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

  await expect(
    canvas.getByTestId("govaction-exampleHash#1-change-your-vote"),
  ).toBeInTheDocument();
}

export const GovernanceVotedOnCardComponent: Story = {
  args: {
    votedProposal: {
      vote: {
        date: new Date().toLocaleDateString(),
        drepId: "drep1_exampledrepid1231231",
        epochNo: 222,
        metadataHash: "ababa1ababab1abababa1ababab1ababa1aba1",
        proposalId: "exampleproposalid12dsadasdasda",
        url: "https://exampleurl.com",
        vote: "yes",
        txHash: "dwq78dqw78qwd78wdq78dqw78dqw",
      },
      proposal: {
        createdEpochNo: 232,
        expiryEpochNo: 323,
        metadataStatus: null,
        metadataValid: true,
        createdDate: "1970-01-01T00:00:00Z",
        expiryDate: "1970-02-01T00:00:00Z",
        id: "exampleId",
        type: GovernanceActionType.InfoAction,
        index: 1,
        txHash: "exampleHash",
        url: "https://example.com",
        metadataHash: "exampleHash",
        dRepYesVotes: 1,
        dRepNoVotes: 0,
        dRepAbstainVotes: 2,
        poolYesVotes: 1,
        poolNoVotes: 0,
        poolAbstainVotes: 2,
        ccYesVotes: 1,
        ccNoVotes: 0,
        ccAbstainVotes: 2,
        protocolParams: null,
        prevGovActionIndex: null,
        prevGovActionTxHash: null,
      },
    },
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await checkGovActionVisibility(canvas);
  },
};

export const GovernanceVotedOnCardAbstain: Story = {
  args: {
    votedProposal: {
      vote: {
        date: new Date().toLocaleDateString(),
        drepId: "drep1_exampledrepid1231231",
        epochNo: 222,
        metadataHash: "ababa1ababab1abababa1ababab1ababa1aba1",
        proposalId: "exampleproposalid12dsadasdasda",
        url: "https://exampleurl.com",
        vote: "abstain",
        txHash: "dwq78dqw78qwd78wdq78dqw78dqw",
      },
      proposal: {
        createdEpochNo: 232,
        expiryEpochNo: 323,
        metadataStatus: null,
        metadataValid: true,
        createdDate: "1970-01-01T00:00:00Z",
        expiryDate: "1970-02-01T00:00:00Z",
        id: "exampleId",
        type: GovernanceActionType.InfoAction,
        index: 1,
        txHash: "exampleHash",
        url: "https://example.com",
        metadataHash: "exampleHash",
        dRepYesVotes: 1,
        dRepNoVotes: 0,
        dRepAbstainVotes: 2,
        poolYesVotes: 1,
        poolNoVotes: 0,
        poolAbstainVotes: 2,
        ccYesVotes: 1,
        ccNoVotes: 0,
        ccAbstainVotes: 2,
        protocolParams: null,
        prevGovActionIndex: null,
        prevGovActionTxHash: null,
      },
    },
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await checkGovActionVisibility(canvas);
    expect(canvas.getByText(/abstain/i)).toBeInTheDocument();
  },
};

export const GovernanceVotedOnCardYes: Story = {
  args: {
    votedProposal: {
      vote: {
        date: new Date().toLocaleDateString(),
        drepId: "drep1_exampledrepid1231231",
        epochNo: 222,
        metadataHash: "ababa1ababab1abababa1ababab1ababa1aba1",
        proposalId: "exampleproposalid12dsadasdasda",
        url: "https://exampleurl.com",
        vote: "yes",
        txHash: "dwq78dqw78qwd78wdq78dqw78dqw",
      },
      proposal: {
        createdEpochNo: 232,
        expiryEpochNo: 323,
        metadataStatus: null,
        metadataValid: true,
        createdDate: "1970-01-01T00:00:00Z",
        expiryDate: "1970-02-01T00:00:00Z",
        id: "exampleId",
        type: GovernanceActionType.InfoAction,
        index: 1,
        txHash: "exampleHash",
        url: "https://example.com",
        metadataHash: "exampleHash",
        dRepYesVotes: 1,
        dRepNoVotes: 0,
        dRepAbstainVotes: 2,
        poolYesVotes: 1,
        poolNoVotes: 0,
        poolAbstainVotes: 2,
        ccYesVotes: 1,
        ccNoVotes: 0,
        ccAbstainVotes: 2,
        protocolParams: null,
        prevGovActionIndex: null,
        prevGovActionTxHash: null,
      },
    },
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await checkGovActionVisibility(canvas);
    expect(canvas.getByText(/yes/i)).toBeInTheDocument();
  },
};

export const GovernanceVotedOnCardNo: Story = {
  args: {
    votedProposal: {
      vote: {
        date: new Date().toLocaleDateString(),
        drepId: "drep1_exampledrepid1231231",
        epochNo: 222,
        metadataHash: "ababa1ababab1abababa1ababab1ababa1aba1",
        proposalId: "exampleproposalid12dsadasdasda",
        url: "https://exampleurl.com",
        vote: "no",
        txHash: "dwq78dqw78qwd78wdq78dqw78dqw",
      },
      proposal: {
        createdEpochNo: 232,
        expiryEpochNo: 323,
        metadataStatus: null,
        metadataValid: true,
        createdDate: "1970-01-01T00:00:00Z",
        expiryDate: "1970-02-01T00:00:00Z",
        id: "exampleId",
        type: GovernanceActionType.InfoAction,
        index: 1,
        txHash: "exampleHash",
        url: "https://example.com",
        metadataHash: "exampleHash",
        dRepYesVotes: 1,
        dRepNoVotes: 0,
        dRepAbstainVotes: 2,
        poolYesVotes: 1,
        poolNoVotes: 0,
        poolAbstainVotes: 2,
        ccYesVotes: 1,
        ccNoVotes: 0,
        ccAbstainVotes: 2,
        protocolParams: null,
        prevGovActionIndex: null,
        prevGovActionTxHash: null,
      },
    },
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await checkGovActionVisibility(canvas);
    expect(canvas.getByText(/no/i)).toBeInTheDocument();
  },
};
