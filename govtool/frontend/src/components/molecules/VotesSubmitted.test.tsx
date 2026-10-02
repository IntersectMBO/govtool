import { render, screen } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";

import "@/i18n";
import type { SubmittedVotesData } from "@models";
import { GovernanceActionType } from "@/types/governanceAction";

import { VotesSubmitted } from "./VotesSubmitted";

const ADA = 1_000_000;

// The barrels pull in every provider and the theme; these are enough.
vi.mock("@atoms", async () => ({
  Typography: (await import("../atoms/Typography")).Typography,
  VotePill: (await import("../atoms/VotePill")).VotePill,
}));

vi.mock("@consts", async () => ({
  IMAGES: (await import("@/consts/images")).IMAGES,
  SECURITY_RELEVANT_PARAMS_MAP: {},
}));

vi.mock("@utils", async () => ({
  correctAdaFormatWithSuffix: (await import("@/utils/adaFormat"))
    .correctAdaFormatWithSuffix,
  getGovActionVotingThreshold: () => undefined,
}));

// Active DReps 100, always-no-confidence 50: the backend's total is 150.
vi.mock("@hooks", async () => ({
  useTranslation: (await import("react-i18next")).useTranslation,
  useGetNetworkTotalStake: () => ({
    networkTotalStake: {
      totalStakeControlledByDReps: 150 * ADA,
      totalStakeControlledBySPOs: 0,
      alwaysAbstainVotingPower: 0,
      alwaysNoConfidenceVotingPower: 50 * ADA,
    },
    fetchNetworkTotalStake: () => Promise.resolve(),
  }),
  useGetNetworkMetrics: () => ({
    networkMetrics: undefined,
    fetchNetworkMetrics: () => Promise.resolve(),
  }),
}));

vi.mock("@/context", () => ({
  useAppContext: () => ({ epochParams: undefined }),
  useFeatureFlag: () => ({
    areDRepVoteTotalsDisplayed: () => true,
    areSPOVoteTotalsDisplayed: () => false,
    areCCVoteTotalsDisplayed: () => false,
    isFeatureAvailable: () => true,
  }),
}));

describe("VotesSubmitted", () => {
  it("counts as not voted only the stake left after yes and no", () => {
    // 25 ADA explicit yes on a NoConfidence action, plus the 50 ADA
    // always-no-confidence stake the backend already counts as yes.
    const votes: SubmittedVotesData = {
      type: GovernanceActionType.NoConfidence,
      protocolParams: null,
      dRepYesVotes: 75 * ADA,
      dRepNoVotes: 0,
      dRepAbstainVotes: 0,
      poolYesVotes: 0,
      poolNoVotes: 0,
      poolAbstainVotes: 0,
      ccYesVotes: 0,
      ccNoVotes: 0,
      ccAbstainVotes: 0,
    };
    render(<VotesSubmitted type={votes.type} votes={votes} />);

    expect(
      screen.getByTestId("submitted-votes-dReps-notVoted"),
    ).toHaveTextContent("₳ 75");
    expect(
      screen.getByTestId("submitted-votes-dReps-yes-percentage"),
    ).toHaveTextContent("50.00%");
  });
});
