import { VoterType } from "@/components/molecules";
import {
  PPU_ECONOMIC_GROUP_PARAMS_KEYS,
  PPU_GOVERNANCE_GROUP_PARAMS_KEYS,
  PPU_NETWORK_GROUP_PARAMS_KEYS,
  PPU_TECHNICAL_GROUP_PARAMS_KEYS,
  SECURITY_RELEVANT_PARAMS_MAP,
} from "@/consts";
import { EpochParams } from "@/models";
import { GovernanceActionType } from "@/types/governanceAction";

type VotingThresholdKey = keyof EpochParams | undefined;

type GetGovActionVotingThresholdKeyOptions = {
  govActionType: GovernanceActionType;
  protocolParams?: Partial<EpochParams> | null;
  voterType?: VoterType;
};

const getParameterChangeThreshold = (
  protocolParams?: Partial<EpochParams> | null,
  voterType?: VoterType,
): VotingThresholdKey => {
  if (!protocolParams) return undefined;

  const protocolParamKeys = Object.keys(protocolParams).filter((key) =>
    ["number", "string"].includes(
      typeof protocolParams[key as keyof EpochParams],
    ),
  );
  const changes = (keys: string[]) =>
    keys.some((key) => protocolParamKeys.includes(key));

  // SPOs vote on a parameter change only when it touches a security-relevant
  // parameter, whichever group that parameter belongs to.
  if (voterType === "sPos") {
    return changes(Object.values(SECURITY_RELEVANT_PARAMS_MAP))
      ? "pvtpp_security_group"
      : undefined;
  }
  if (voterType !== "dReps") return undefined;

  if (changes(PPU_ECONOMIC_GROUP_PARAMS_KEYS)) return "dvt_p_p_economic_group";
  if (changes(PPU_GOVERNANCE_GROUP_PARAMS_KEYS)) return "dvt_p_p_gov_group";
  if (changes(PPU_NETWORK_GROUP_PARAMS_KEYS)) return "dvt_p_p_network_group";
  if (changes(PPU_TECHNICAL_GROUP_PARAMS_KEYS)) {
    return "dvt_p_p_technical_group";
  }
  return undefined;
};

export const getGovActionVotingThresholdKey = ({
  govActionType,
  protocolParams,
  voterType,
}: GetGovActionVotingThresholdKeyOptions): VotingThresholdKey => {
  const govActionThresholds: Record<GovernanceActionType, VotingThresholdKey> =
    {
      [GovernanceActionType.NewCommittee]:
        voterType === "dReps"
          ? "dvt_committee_normal"
          : voterType === "sPos"
            ? "pvt_committee_normal"
            : undefined,
      [GovernanceActionType.HardForkInitiation]:
        voterType === "dReps"
          ? "dvt_hard_fork_initiation"
          : voterType === "sPos"
            ? "pvt_hard_fork_initiation"
            : undefined,
      [GovernanceActionType.ParameterChange]: getParameterChangeThreshold(
        protocolParams,
        voterType,
      ),
      [GovernanceActionType.TreasuryWithdrawals]:
        voterType === "dReps" ? "dvt_treasury_withdrawal" : undefined,
      [GovernanceActionType.NewConstitution]:
        voterType === "dReps" ? "dvt_update_to_constitution" : undefined,
      [GovernanceActionType.NoConfidence]:
        voterType === "dReps"
          ? "dvt_motion_no_confidence"
          : "pvt_motion_no_confidence",
      [GovernanceActionType.InfoAction]: undefined,
    };

  return govActionThresholds[govActionType] ?? undefined;
};
