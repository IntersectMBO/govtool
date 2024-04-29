export const removeDuplicatedProposals = (proposals: ActionTypeToDsiplay[]) => {
  if (!proposals) return [];
  const uniqueGovActionIds = new Set();
  return proposals.filter((item) => {
    const govActionId = item.txHash + item.index;
    if (!uniqueGovActionIds.has(govActionId)) {
      uniqueGovActionIds.add(govActionId);
      return true;
    }
    return false;
  });
};
