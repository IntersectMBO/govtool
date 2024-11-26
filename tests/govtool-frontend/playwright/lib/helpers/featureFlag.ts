import { GrovernanceActionType, IProposal } from "@types";
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
      proposal.type === GrovernanceActionType.HardFork ||
      (proposal.type === GrovernanceActionType.ProtocolParameterChange &&
        !isSecurityGroup)
    );
  }

  return ![
    GrovernanceActionType.NoConfidence,
    GrovernanceActionType.NewCommittee,
    GrovernanceActionType.UpdatetotheConstitution,
  ].includes(proposal.type as GrovernanceActionType);
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
    return proposal.type !== GrovernanceActionType.ProtocolParameterChange;
  }

  return !(
    proposal.type === GrovernanceActionType.UpdatetotheConstitution ||
    proposal.type === GrovernanceActionType.TreasuryWithdrawal ||
    (proposal.type === GrovernanceActionType.ProtocolParameterChange &&
      !isSecurityGroup)
  );
};

export const areCCVoteTotalsDisplayed = async (
  governanceActionType: GrovernanceActionType
) => {
  return ![
    GrovernanceActionType.NoConfidence,
    GrovernanceActionType.NewCommittee,
  ].includes(governanceActionType);
};
