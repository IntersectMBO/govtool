import { VoterType } from "@/components/molecules";
import { GovernanceActionType } from "@/types/governanceAction";
import { getGovActionVotingThresholdKey } from "../getGovActionVotingThresholdKey";

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
});
