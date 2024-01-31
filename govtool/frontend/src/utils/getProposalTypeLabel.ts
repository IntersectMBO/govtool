import { GOVERNANCE_ACTIONS_FILTERS } from "@consts";

export const getProposalTypeLabel = (type: string) => {
  const label = GOVERNANCE_ACTIONS_FILTERS.find((i) => { return i.key === type })?.label;
  return label || type;
};