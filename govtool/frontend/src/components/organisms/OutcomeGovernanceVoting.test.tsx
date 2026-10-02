import { render, screen } from "@testing-library/react";
import { beforeEach, describe, expect, it, vi } from "vitest";

import "@/i18n";
import type { OutcomeGovernanceAction, OutcomeNetworkMetrics } from "@models";
import { GovernanceActionType } from "@/types/governanceAction";

import { OutcomeGovernanceVoting } from "./OutcomeGovernanceVoting";

const ADA = 1_000_000;

const state: { metrics: OutcomeNetworkMetrics } = {
  metrics: {} as OutcomeNetworkMetrics,
};

// The barrels pull in every provider and the theme; these are enough.
vi.mock("@atoms", async () => ({
  Typography: (await import("../atoms/Typography")).Typography,
}));

vi.mock("@consts", async () => ({
  ...(await import("@/consts/colors")),
  SECURITY_RELEVANT_PARAMS_MAP: {},
}));

vi.mock("@utils", async () => ({
  ...(await import("@/utils/outcomes")),
  getGovActionVotingThreshold: () => undefined,
}));

vi.mock("@molecules", async () => ({
  OutcomeIndicator: (await import("../molecules/OutcomeIndicator"))
    .OutcomeIndicator,
  OutcomeStatusChip: (await import("../molecules/OutcomeStatusChip"))
    .OutcomeStatusChip,
  OutcomeVoteSection: (await import("../molecules/OutcomeVoteSection"))
    .OutcomeVoteSection,
}));

vi.mock("@hooks", async () => ({
  useTranslation: (await import("react-i18next")).useTranslation,
  useGetOutcomeNetworkMetrics: () => ({
    networkMetrics: state.metrics,
    epochParams: undefined,
    isLoading: false,
    areDRepVoteTotalsDisplayed: () => true,
    areSPOVoteTotalsDisplayed: () => true,
    areCCVoteTotalsDisplayed: () => false,
  }),
}));

// As every provider reports it: the automatic votes are already in the
// action's figures (always-no-confidence DRep stake in yes on a NoConfidence
// action, passive pool always-abstain stake in pool abstain).
const noConfidenceAction = (
  overrides: Partial<OutcomeGovernanceAction> = {},
): OutcomeGovernanceAction =>
  ({
    type: GovernanceActionType.NoConfidence,
    proposal_params: null,
    status: {
      ratified_epoch: null,
      enacted_epoch: null,
      dropped_epoch: null,
      expired_epoch: null,
    },
    yes_votes: 75 * ADA,
    no_votes: 0,
    abstain_votes: 0,
    pool_yes_votes: 40 * ADA,
    pool_no_votes: 0,
    pool_abstain_votes: 20 * ADA,
    cc_yes_votes: 0,
    cc_no_votes: 0,
    cc_abstain_votes: 0,
    ...overrides,
  }) as unknown as OutcomeGovernanceAction;

const yesLabels = () =>
  Array.from(
    document.querySelectorAll('[data-testid$="-yes-votes-submitted"]'),
  ).map((element) => element.textContent);

describe("OutcomeGovernanceVoting", () => {
  beforeEach(() => {
    state.metrics = {
      epoch_no: 500,
      // Active DReps 100 + always-abstain 0 + always-no-confidence 50.
      total_stake_controlled_by_active_dreps: String(150 * ADA),
      total_stake_controlled_by_stake_pools: String(100 * ADA),
      always_abstain_voting_power: "0",
      spos_abstain_voting_power: String(20 * ADA),
      always_no_confidence_voting_power: String(50 * ADA),
      spos_no_confidence_voting_power: "0",
      no_of_committee_members: 7,
      quorum_numerator: 2,
      quorum_denominator: 3,
    };
  });

  it("does not count the automatic votes a second time", () => {
    render(<OutcomeGovernanceVoting action={noConfidenceAction()} />);

    // DReps: 75 yes of 150 non-abstaining. SPOs: 40 yes of 100 - 20 abstain.
    const labels = yesLabels();
    expect(labels).toHaveLength(2);
    labels.forEach((label) => expect(label).toMatch(/- 50\.00%$/));
  });

  it("shows a group without totals as unavailable, not as no votes", () => {
    render(
      <OutcomeGovernanceVoting
        action={noConfidenceAction({
          pool_yes_votes: null,
          pool_no_votes: null,
          pool_abstain_votes: null,
        })}
      />,
    );

    expect(
      screen.getByTestId("vote-totals-unavailable-label"),
    ).toBeInTheDocument();
    expect(yesLabels()).toHaveLength(1);
  });
});
