import { VoterType } from "@/components/molecules";
import {
  PPU_ECONOMIC_GROUP_PARAMS_KEYS,
  PPU_GOVERNANCE_GROUP_PARAMS_KEYS,
  PPU_NETWORK_GROUP_PARAMS_KEYS,
  PPU_TECHNICAL_GROUP_PARAMS_KEYS,
} from "@/consts";
import { EpochParams } from "@/models";
import { GovernanceActionType } from "@/types/governanceAction";

/**
 * Retrieves the voting threshold key for a given governance action type,
 *  protocol parameters, and voter type.
 * @param options - The options object containing the governance action type,
 *  protocol parameters, and voter type.
 * @returns The voting threshold key corresponding to
 * the provided options, or undefined if no matching key is found.
 */
export const getGovActionVotingThresholdKey = ({
  govActionType,
  protocolParams,
  voterType,
}: {
  govActionType: GovernanceActionType;
  protocolParams?: Partial<EpochParams> | null;
  voterType?: VoterType;
}) => {
  switch (govActionType) {
    case GovernanceActionType.NewCommittee: {
      // TODO: handle dvt_committee_normal and pvt_committee_normal
      return voterType === "dReps"
        ? "dvt_committee_normal"
        : voterType === "sPos"
        ? "pvt_committee_normal"
        : undefined;
    }
    case GovernanceActionType.HardForkInitiation: {
      return voterType === "dReps"
        ? "dvt_hard_fork_initiation"
        : voterType === "sPos"
        ? "pvt_hard_fork_initiation"
        : undefined;
    }
    case GovernanceActionType.ParameterChange: {
      if (protocolParams) {
        const filteredProtocolParameterKeys = Object.entries(
          protocolParams,
        ).map(([key, value]) => {
          if (typeof value === "number" || typeof value === "string") {
            return key;
          }
        });

        if (
          PPU_ECONOMIC_GROUP_PARAMS_KEYS.some((key) =>
            filteredProtocolParameterKeys.includes(key),
          )
        ) {
          return voterType === "dReps" ? "dvt_p_p_economic_group" : undefined;
        }
        if (
          PPU_GOVERNANCE_GROUP_PARAMS_KEYS.some((key) =>
            filteredProtocolParameterKeys.includes(key),
          )
        ) {
          return voterType === "dReps" ? "dvt_p_p_gov_group" : undefined;
        }
        if (
          PPU_NETWORK_GROUP_PARAMS_KEYS.some((key) =>
            filteredProtocolParameterKeys.includes(key),
          )
        ) {
          return voterType === "dReps"
            ? "dvt_p_p_network_group"
            : voterType === "sPos"
            ? "pvtpp_security_group"
            : undefined;
        }
        if (
          PPU_TECHNICAL_GROUP_PARAMS_KEYS.some((key) =>
            filteredProtocolParameterKeys.includes(key),
          )
        ) {
          return voterType === "dReps" ? "dvt_p_p_technical_group" : undefined;
        }
        return undefined;
      }
      break;
    }
    case GovernanceActionType.TreasuryWithdrawals:
      return voterType === "dReps" ? "dvt_treasury_withdrawal" : undefined;
    case GovernanceActionType.NewConstitution:
      return voterType === "dReps" ? "dvt_update_to_constitution" : undefined;
    case GovernanceActionType.NoConfidence:
      return voterType === "dReps"
        ? "dvt_motion_no_confidence"
        : "pvt_motion_no_confidence";
    default:
      return undefined;
  }
};
