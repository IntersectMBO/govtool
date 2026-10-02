import type { HTMLAttributes, PropsWithChildren } from "react";
import { render, screen } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";

import type { OutcomeGovernanceAction, OutcomeVoteAggregate } from "@models";
import { OutcomeGovernanceVoting } from "./OutcomeGovernanceVoting";

vi.mock("@hooks", () => ({
  useTranslation: () => ({ t: (key: string) => key }),
}));
vi.mock("@atoms", () => ({
  Typography: ({
    children,
    role,
    "data-testid": testId,
  }: PropsWithChildren<
    HTMLAttributes<HTMLParagraphElement> & { "data-testid"?: string }
  >) => (
    <p role={role} data-testid={testId}>
      {children}
    </p>
  ),
}));
vi.mock("@molecules", async () => ({
  OutcomeStatusChip: ({ label }: { label: string }) => <p>{label}</p>,
  OutcomeVoteSection: (await import("../molecules/OutcomeVoteSection"))
    .OutcomeVoteSection,
  OutcomeIndicator: (await import("../molecules/OutcomeIndicator"))
    .OutcomeIndicator,
}));
vi.mock(
  "@intersect.mbo/intersectmbo.org-icons-set",
  async (importOriginal) => ({
    ...(await importOriginal<object>()),
    IconThumbUp: () => <span>passed</span>,
    IconThumbDown: () => <span>failed</span>,
  }),
);
vi.mock("@utils", () => import("../../utils/outcomeVoteAggregate"));
vi.mock("@consts", () => ({
  SECURITY_RELEVANT_PARAMS_MAP: {
    maxTxSize: "max_tx_size",
    maxBlockExecutionSteps: "max_block_ex_steps",
  },
  primaryBlue: { c500: "blue" },
  successGreen: { c500: "green", c600: "green" },
  errorRed: { c500: "red" },
}));

const action: OutcomeGovernanceAction = {
  id: "action",
  tx_hash: "ab".repeat(32),
  index: 0,
  type: "InfoAction",
  description: null,
  expiry_date: "",
  expiration: 12,
  time: "",
  epoch_no: 10,
  url: "",
  data_hash: "",
  proposal_params: null,
  json_metadata: null,
  status: {
    ratified_epoch: 11,
    enacted_epoch: 12,
    dropped_epoch: null,
    expired_epoch: null,
  },
  status_times: {
    ratified_time: null,
    enacted_time: null,
    dropped_time: null,
    expired_time: null,
  },
  yes_votes: 0,
  no_votes: 0,
  abstain_votes: 0,
  pool_yes_votes: 0,
  pool_no_votes: 0,
  pool_abstain_votes: 0,
  cc_yes_votes: 0,
  cc_no_votes: 0,
  cc_abstain_votes: 0,
  prev_gov_action_index: null,
  prev_gov_action_tx_hash: null,
};

describe("action-specific outcome tallies", () => {
  const drep: OutcomeVoteAggregate = {
    role: "drep",
    representation: "stake",
    yes: "67000000",
    no: "3000000",
    abstain: "10000000",
    notVoted: "30000000",
    totalEligible: "110000000",
    threshold: { numerator: 67, denominator: 100 },
  };
  const spo: OutcomeVoteAggregate = {
    role: "spo",
    representation: "stake",
    yes: "40000000",
    no: "10000000",
    abstain: "0",
    notVoted: "50000000",
    totalEligible: "100000000",
    threshold: { numerator: 51, denominator: 100 },
  };

  it("does not count automatic votes a second time", () => {
    render(
      <OutcomeGovernanceVoting
        action={{
          ...action,
          type: "NoConfidence",
          yes_votes: 75000000,
          pool_yes_votes: 40000000,
          pool_abstain_votes: 20000000,
          vote_aggregates: [
            {
              ...drep,
              yes: "75000000",
              no: "0",
              abstain: "0",
              notVoted: "75000000",
              totalEligible: "150000000",
            },
            {
              ...spo,
              abstain: "20000000",
              no: "0",
              notVoted: "40000000",
            },
          ],
        }}
      />,
    );
    // Automatic DRep votes are already included in yes; passive SPO votes
    // are already included in abstain. Each non-abstaining denominator is 50% yes.
    expect(
      screen.getByTestId("outcome.votes.dReps-yes-votes-submitted"),
    ).toHaveTextContent("₳ 75 - 50.00%");
    expect(
      screen.getByTestId("outcome.votes.sPos-yes-votes-submitted"),
    ).toHaveTextContent("₳ 40 - 50.00%");
  });

  it("shows a group without totals as unavailable, not as no votes", () => {
    render(
      <OutcomeGovernanceVoting
        action={{
          ...action,
          type: "NoConfidence",
          pool_yes_votes: null,
          pool_no_votes: null,
          pool_abstain_votes: null,
          vote_aggregates: [drep],
        }}
      />,
    );
    expect(
      screen.getByTestId("SPOs-voting-results-data-unavailable"),
    ).toBeVisible();
    expect(
      screen.getByTestId("outcome.votes.dReps-yes-votes-submitted"),
    ).toBeVisible();
    expect(
      screen.queryByTestId("outcome.votes.sPos-yes-votes-submitted"),
    ).not.toBeInTheDocument();
  });

  it("renders an available SPO aggregate even when proposal parameters are unavailable", () => {
    render(
      <OutcomeGovernanceVoting
        action={{ ...action, type: "ParameterChange", vote_aggregates: [spo] }}
      />,
    );
    expect(
      screen.getByTestId("outcome.votes.sPos-yes-votes-submitted"),
    ).toHaveTextContent("₳ 40 - 40.00%");
  });

  it("marks missing SPO data as unavailable for a change to block execution steps", () => {
    render(
      <OutcomeGovernanceVoting
        action={{
          ...action,
          type: "ParameterChange",
          proposal_params: {
            max_block_ex_steps: 1000000,
          } as OutcomeGovernanceAction["proposal_params"],
        }}
      />,
    );
    expect(
      screen.getByTestId("SPOs-voting-results-data-unavailable"),
    ).toHaveTextContent("outcome.votes.dataUnavailable");
  });

  it("renders DRep and SPO results when the historical committee tally is unsupported", () => {
    render(
      <OutcomeGovernanceVoting
        action={{
          ...action,
          type: "HardForkInitiation",
          vote_aggregates: [drep, spo],
        }}
      />,
    );
    expect(screen.getByText("outcome.status.enacted")).toBeVisible();
    expect(
      screen.getByTestId("outcome.votes.dReps-yes-votes-submitted"),
    ).toHaveTextContent("₳ 67 - 67.00%");
    expect(
      screen.getByTestId("outcome.votes.sPos-yes-votes-submitted"),
    ).toHaveTextContent("₳ 40 - 40.00%");
    expect(
      screen.getByTestId("CC-voting-results-data-unavailable"),
    ).toHaveTextContent("outcome.votes.dataUnavailable");
    expect(screen.getByTestId("CC-voting-results-outcome")).toHaveTextContent(
      "-",
    );
    expect(
      screen.getByTestId("DReps-voting-results-outcome"),
    ).not.toHaveTextContent("-");
  });

  it("distinguishes a group that does not vote from an unsupported applicable group", () => {
    render(
      <OutcomeGovernanceVoting
        action={{
          ...action,
          type: "TreasuryWithdrawals",
          vote_aggregates: [drep],
        }}
      />,
    );
    expect(screen.getByTestId("voting-not-available-label")).toHaveTextContent(
      "outcome.votes.sPos outcome.votes.votingNotAvailable",
    );
    expect(
      screen.getByTestId("CC-voting-results-data-unavailable"),
    ).toBeVisible();
    expect(
      screen.queryByTestId("SPOs-voting-results-data-unavailable"),
    ).not.toBeInTheDocument();
  });

  it("renders the committee's eligible member denominator independently", () => {
    const cc: OutcomeVoteAggregate = {
      role: "cc",
      representation: "count",
      yes: "2",
      no: "0",
      abstain: "1",
      notVoted: "1",
      totalEligible: "4",
      threshold: { numerator: 2, denominator: 3 },
    };
    render(
      <OutcomeGovernanceVoting
        action={{
          ...action,
          type: "HardForkInitiation",
          vote_aggregates: [cc],
        }}
      />,
    );
    expect(
      screen.getByTestId("active-constitutional-committee-count"),
    ).toHaveTextContent("4");
    expect(
      screen.getByTestId("outcome.votes.cCommitteeFull-yes-votes-submitted"),
    ).toHaveTextContent("2 - 66.67%");
    expect(
      screen.getByTestId("DReps-voting-results-data-unavailable"),
    ).toBeVisible();
  });

  it("does not fabricate results from absent aggregates or legacy zero counters", () => {
    render(<OutcomeGovernanceVoting action={action} />);
    expect(screen.getAllByRole("status")).toHaveLength(3);
    expect(screen.queryByRole("progressbar")).not.toBeInTheDocument();
    expect(screen.getByText("outcome.status.enacted")).toBeVisible();
  });

  it.each([
    [{ numerator: 67, denominator: 100 }, "failed"],
    [{ numerator: 0, denominator: 1 }, "passed"],
  ])(
    "decides a zero non-abstaining denominator as a zero ratio against %o",
    (threshold, decision) => {
      render(
        <OutcomeGovernanceVoting
          action={{
            ...action,
            type: "HardForkInitiation",
            vote_aggregates: [
              {
                ...drep,
                yes: "0",
                no: "0",
                abstain: "110000000",
                notVoted: "0",
                threshold,
              },
            ],
          }}
        />,
      );
      expect(screen.getByText("outcome.votes.noEligibleVotes")).toBeVisible();
      expect(screen.queryByRole("progressbar")).not.toBeInTheDocument();
      expect(
        screen.getByTestId("DReps-voting-results-outcome"),
      ).toHaveTextContent(decision);
    },
  );

  it("does not report InfoAction as passing even with all yes votes", () => {
    render(
      <OutcomeGovernanceVoting
        action={{
          ...action,
          vote_aggregates: [
            {
              ...drep,
              yes: "110000000",
              no: "0",
              abstain: "0",
              notVoted: "0",
              threshold: { numerator: 1, denominator: 1 },
            },
          ],
        }}
      />,
    );
    expect(
      screen.getByTestId("DReps-voting-results-outcome"),
    ).toHaveTextContent("-");
    expect(screen.getByRole("progressbar")).toBeVisible();
  });
});
