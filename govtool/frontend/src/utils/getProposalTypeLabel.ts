import { GOVERNANCE_ACTIONS_FILTERS } from "@consts";

export const getProposalTypeLabel = (type: string) => {
  const label = GOVERNANCE_ACTIONS_FILTERS.find((i) => i.key === type)?.label;
  return label || type;
};
export const getProposalTypeNoEmptySpaces = (type: string) =>
  getProposalTypeLabel(type).replace(/ /g, "");
