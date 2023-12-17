import type { Meta, StoryObj } from "@storybook/react";

import { GovernanceVotedOnCard } from "@molecules";
import { userEvent, waitFor, within, screen } from "@storybook/testing-library";
import { expect, jest } from "@storybook/jest";
import { formatDisplayDate } from "@/utils";

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

async function checkGovActionVisibility(canvas: any) {
  expect(canvas.getByTestId("exampleType-type")).toBeInTheDocument();
  expect(canvas.getByTestId("exampleHash#1-id")).toBeInTheDocument();
  expect(canvas.getByText(/vote submitted/i)).toBeInTheDocument();

  expect(
    canvas.getByText(formatDisplayDate("1970-01-01T00:00:00Z"))
  ).toBeInTheDocument();
  expect(
    canvas.getByText(formatDisplayDate("1970-02-01T00:00:00Z"))
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
    canvas.getByTestId("govaction-exampleHash#1-change-your-vote")
  ).toBeInTheDocument();
}

export const GovernanceVotedOnCardComponent: Story = {
  args: {
    votedProposal: {
      proposal: {
        createdDate: "1970-01-01T00:00:00Z",
        expiryDate: "1970-02-01T00:00:00Z",
        id: "exampleId",
        type: "exampleType",
        index: 1,
        txHash: "exampleHash",
      },
      vote: { vote: "no" },
    },
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    await checkGovActionVisibility(canvas);
  },
};

export const GovernanceVotedOnCardAbstain: Story = {
  args: {
    votedProposal: {
      proposal: {
        createdDate: "1970-01-01T00:00:00Z",
        expiryDate: "1970-02-01T00:00:00Z",
        id: "exampleId",
        type: "exampleType",
        index: 1,
        txHash: "exampleHash",
      },
      vote: { vote: "abstain" },
    },
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    await checkGovActionVisibility(canvas);
    expect(canvas.getByText(/abstain/i)).toBeInTheDocument();
  },
};

export const GovernanceVotedOnCardYes: Story = {
  args: {
    votedProposal: {
      proposal: {
        createdDate: "1970-01-01T00:00:00Z",
        expiryDate: "1970-02-01T00:00:00Z",
        id: "exampleId",
        type: "exampleType",
        index: 1,
        txHash: "exampleHash",
      },
      vote: { vote: "yes" },
    },
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    await checkGovActionVisibility(canvas);
    expect(canvas.getByText(/yes/i)).toBeInTheDocument();
  },
};

export const GovernanceVotedOnCardNo: Story = {
  args: {
    votedProposal: {
      proposal: {
        createdDate: "1970-01-01T00:00:00Z",
        expiryDate: "1970-02-01T00:00:00Z",
        id: "exampleId",
        type: "exampleType",
        index: 1,
        txHash: "exampleHash",
      },
      vote: { vote: "no" },
    },
  },
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    await checkGovActionVisibility(canvas);
    expect(canvas.getByText(/no/i)).toBeInTheDocument();
  },
};
