import { GovernanceActionType, IProposal } from "@types";
import { isBootStrapingPhase } from "./cardano";
import { SECURITY_RELEVANT_PARAMS_MAP } from "@constants/index";

export const areDRepVoteTotalsDisplayed = async (proposal: IProposal) => {
  const isInBootstrapPhase = await isBootStrapingPhase();
  const isSecurityGroup = Object.values(SECURITY_RELEVANT_PARAMS_MAP).some(
    (paramKey) =>
      proposal.protocolParams?.[
        paramKey as keyof typeof proposal.protocolParams
      ] !== null
  );
  if (isInBootstrapPhase) {
    return !(
      proposal.type === GovernanceActionType.HardFork ||
      (proposal.type === GovernanceActionType.ProtocolParameterChange &&
        !isSecurityGroup)
    );
  }

  return ![
    GovernanceActionType.NoConfidence,
    GovernanceActionType.NewCommittee,
    GovernanceActionType.UpdatetotheConstitution,
  ].includes(proposal.type as GovernanceActionType);
};

export const areSPOVoteTotalsDisplayed = async (proposal: IProposal) => {
  const isInBootstrapPhase = await isBootStrapingPhase();
  const isSecurityGroup = Object.values(SECURITY_RELEVANT_PARAMS_MAP).some(
    (paramKey) =>
      proposal.protocolParams?.[
        paramKey as keyof typeof proposal.protocolParams
      ] !== null
  );
  if (isInBootstrapPhase) {
    return proposal.type !== GovernanceActionType.ProtocolParameterChange;
  }

  return !(
    proposal.type === GovernanceActionType.UpdatetotheConstitution ||
    proposal.type === GovernanceActionType.TreasuryWithdrawal ||
    (proposal.type === GovernanceActionType.ProtocolParameterChange &&
      !isSecurityGroup)
  );
};

export const areCCVoteTotalsDisplayed = (
  governanceActionType: GovernanceActionType
) => {
  return ![
    GovernanceActionType.NoConfidence,
    GovernanceActionType.NewCommittee,
  ].includes(governanceActionType);
};
