import { VoterType } from "@/components/molecules";
import { GovernanceActionType } from "@/types/governanceAction";
import {
  getGovActionVotingThreshold,
  getGovActionVotingThresholdKey,
} from "../getGovActionVotingThresholdKey";

describe("getGovActionVotingThresholdKey", () => {
  it("should return the correct voting threshold key for NewCommittee and dReps voter type", () => {
    const govActionType = GovernanceActionType.NewCommittee;
    const voterType: VoterType = "dReps";
    const result = getGovActionVotingThresholdKey({
      govActionType,
      voterType,
    });
    expect(result).toBe("dvt_committee_normal");
  });

  it("should return the correct voting threshold key for NewCommittee and sPos voter type", () => {
    const govActionType = GovernanceActionType.NewCommittee;
    const voterType: VoterType = "sPos";
    const result = getGovActionVotingThresholdKey({
      govActionType,
      voterType,
    });
    expect(result).toBe("pvt_committee_normal");
  });

  it("should return undefined for NewCommittee and ccCommittee voter type", () => {
    const govActionType = GovernanceActionType.NewCommittee;
    const voterType: VoterType = "ccCommittee";
    const result = getGovActionVotingThresholdKey({
      govActionType,
      voterType,
    });
    expect(result).toBeUndefined();
  });

  it("should return the correct voting threshold key for HardForkInitiation and dReps voter type", () => {
    const govActionType = GovernanceActionType.HardForkInitiation;
    const voterType: VoterType = "dReps";
    const result = getGovActionVotingThresholdKey({
      govActionType,
      voterType,
    });
    expect(result).toBe("dvt_hard_fork_initiation");
  });

  it("should return the correct voting threshold key for HardForkInitiation and sPos voter type", () => {
    const govActionType = GovernanceActionType.HardForkInitiation;
    const voterType: VoterType = "sPos";
    const result = getGovActionVotingThresholdKey({
      govActionType,
      voterType,
    });
    expect(result).toBe("pvt_hard_fork_initiation");
  });

  it("should return undefined for unknown governance action type", () => {
    const govActionType = GovernanceActionType.InfoAction;
    const voterType: VoterType = "dReps";
    const result = getGovActionVotingThresholdKey({
      govActionType,
      voterType,
    });
    expect(result).toBeUndefined();
  });

  it("should return undefined for ParameterChange and sPos voter type", () => {
    const govActionType = GovernanceActionType.ParameterChange;
    const voterType: VoterType = "sPos";
    const result = getGovActionVotingThresholdKey({
      govActionType,
      voterType,
    });
    expect(result).toBeUndefined();
  });

  describe("ParameterChange", () => {
    const govActionType = GovernanceActionType.ParameterChange;

    it("gives SPOs the security group threshold for a network-group security parameter", () => {
      expect(
        getGovActionVotingThresholdKey({
          govActionType,
          protocolParams: { max_block_size: 90112, collateral_percent: null },
          voterType: "sPos",
        }),
      ).toBe("pvtpp_security_group");
    });

    it("gives SPOs the security group threshold for an economic security parameter", () => {
      expect(
        getGovActionVotingThresholdKey({
          govActionType,
          protocolParams: { min_fee_a: 44, max_tx_size: 16384 },
          voterType: "sPos",
        }),
      ).toBe("pvtpp_security_group");
      expect(
        getGovActionVotingThresholdKey({
          govActionType,
          protocolParams: { min_fee_a: 44, max_tx_size: 16384 },
          voterType: "dReps",
        }),
      ).toBe("dvt_p_p_economic_group");
    });

    it("gives SPOs no threshold when no security parameter changes", () => {
      expect(
        getGovActionVotingThresholdKey({
          govActionType,
          protocolParams: { max_tx_ex_mem: 14000000, max_block_size: null },
          voterType: "sPos",
        }),
      ).toBeUndefined();
      expect(
        getGovActionVotingThresholdKey({
          govActionType,
          protocolParams: { max_tx_ex_mem: 14000000 },
          voterType: "dReps",
        }),
      ).toBe("dvt_p_p_network_group");
    });

    it("gives the committee no threshold key", () => {
      expect(
        getGovActionVotingThresholdKey({
          govActionType,
          protocolParams: { max_block_size: 90112 },
          voterType: "ccCommittee",
        }),
      ).toBeUndefined();
    });
  });

  describe("getGovActionVotingThreshold", () => {
    const epochParams = {
      dvt_treasury_withdrawal: 0.67,
      pvt_committee_normal: 0.51,
    };

    it("shows DReps and SPOs against 100% on an info action", () => {
      (["dReps", "sPos"] as VoterType[]).forEach((voterType) => {
        expect(
          getGovActionVotingThreshold({
            govActionType: GovernanceActionType.InfoAction,
            voterType,
            epochParams,
          }),
        ).toBe(1);
      });
    });

    it("gives the committee no threshold on an info action", () => {
      expect(
        getGovActionVotingThreshold({
          govActionType: GovernanceActionType.InfoAction,
          voterType: "ccCommittee",
          epochParams,
        }),
      ).toBeUndefined();
    });

    it("reads the parameter's value for other actions", () => {
      expect(
        getGovActionVotingThreshold({
          govActionType: GovernanceActionType.TreasuryWithdrawals,
          voterType: "dReps",
          epochParams,
        }),
      ).toBe(0.67);
      expect(
        getGovActionVotingThreshold({
          govActionType: GovernanceActionType.NewCommittee,
          voterType: "sPos",
          epochParams: null,
        }),
      ).toBeUndefined();
    });
  });
});
