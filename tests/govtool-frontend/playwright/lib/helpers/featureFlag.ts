import {
  GovernanceActionType,
  IProposal,
  outcomeProposal,
  outcomeType,
} from "@types";
import { isBootStrapingPhase } from "./cardano";
import { SECURITY_RELEVANT_PARAMS_MAP } from "@constants/index";

const getProposalType = (
  type: keyof typeof outcomeType,
  fallback: GovernanceActionType,
  proposal: IProposal | outcomeProposal
) =>
  "proposal_params" in proposal
    ? Object.keys(outcomeType).find(
        (key) => outcomeType[key] === outcomeType[type]
      )
    : fallback;

export const areDRepVoteTotalsDisplayed = async (
  proposal: IProposal | outcomeProposal
) => {
  const isInBootstrapPhase = await isBootStrapingPhase();
  const isSecurityGroup = Object.values(SECURITY_RELEVANT_PARAMS_MAP).some(
    (paramKey) => {
      const params =
        "protocolParams" in proposal
          ? proposal.protocolParams
          : proposal.proposal_params;
      return params?.[paramKey as keyof typeof params] !== null;
    }
  );

  if (isInBootstrapPhase) {
    const HardForkInitiation = getProposalType(
      "HardForkInitiation",
      GovernanceActionType.HardFork,
      proposal
    );

    const ProtocolParameterChange = getProposalType(
      "ParameterChange",
      GovernanceActionType.ProtocolParameterChange,
      proposal
    );

    return !(
      proposal.type === HardForkInitiation ||
      (proposal.type === ProtocolParameterChange && !isSecurityGroup)
    );
  }

  return true;
};

export const areSPOVoteTotalsDisplayed = async (
  proposal: IProposal | outcomeProposal
) => {
  const isInBootstrapPhase = await isBootStrapingPhase();
  const isSecurityGroup = Object.values(SECURITY_RELEVANT_PARAMS_MAP).some(
    (paramKey) => {
      const params =
        "protocolParams" in proposal
          ? proposal.protocolParams
          : proposal.proposal_params;
      return params?.[paramKey as keyof typeof params] !== null;
    }
  );

  const ProtocolParameterChange = getProposalType(
    "ParameterChange",
    GovernanceActionType.ProtocolParameterChange,
    proposal
  );
  const UpdatetotheConstitution = getProposalType(
    "NewConstitution",
    GovernanceActionType.UpdatetotheConstitution,
    proposal
  );
  const TreasuryWithdrawal = getProposalType(
    "TreasuryWithdrawals",
    GovernanceActionType.TreasuryWithdrawal,
    proposal
  );

  if (isInBootstrapPhase) {
    return proposal.type !== ProtocolParameterChange;
  }

  return !(
    proposal.type === UpdatetotheConstitution ||
    proposal.type === TreasuryWithdrawal ||
    (proposal.type === ProtocolParameterChange && !isSecurityGroup)
  );
};

export const areCCVoteTotalsDisplayed = (
  proposal: IProposal | outcomeProposal
) => {
  const NoConfidence = getProposalType(
    "NoConfidence",
    GovernanceActionType.NoConfidence,
    proposal
  );
  const NewCommittee = getProposalType(
    "NewCommittee",
    GovernanceActionType.NewCommittee,
    proposal
  );
  return ![NewCommittee, NoConfidence].includes(proposal.type);
};
