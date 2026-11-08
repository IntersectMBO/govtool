import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, userEvent, waitFor, within, screen } from "storybook/test";

import { GovernanceVotedOnCard } from "@molecules";
import { formatDisplayDate, getProposalTypeNoEmptySpaces } from "@/utils";
import { GovernanceActionType } from "@/types/governanceAction";
import { MetadataValidationStatus } from "@/models/metadataValidation";

const meta = {
  title: "Example/GovernanceVotedOnCard",
  component: GovernanceVotedOnCard,
  parameters: {
    layout: "padded",
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
  expect(canvas.getByText("Epoch 323")).toBeVisible();

  const expiryDate = formatDisplayDate(
    "1970-02-01T00:00:00Z",
    "do MMM yyyy, HH:mm",
  );
  await userEvent.hover(canvas.getByLabelText(expiryDate));
  await waitFor(() => {
    expect(screen.getByRole("tooltip")).toHaveTextContent(expiryDate);
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

export const GovernanceVotedOnCardDataFormattedIncorrectly: Story = {
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
    metadataStatus: MetadataValidationStatus.INCORRECT_FORMAT,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    expect(canvas.getByText(/Data Formatted Incorrectly/i)).toBeInTheDocument();
  },
};

export const GovernanceVotedOnCardValidating: Story = {
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
    isValidating: true,
  },
};
