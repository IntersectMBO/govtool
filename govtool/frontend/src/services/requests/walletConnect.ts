import { GovernanceActionsAPI } from "../GovernanceActionsAPI";

/** A stake key's voting power in lovelace; a failure stays in its panel. */
export const getStakeKeyVotingPower = async (stakeKey: string) => {
  const { data } = await GovernanceActionsAPI.get<number>(
    `/ada-holder/get-voting-power/${encodeURIComponent(stakeKey)}`,
  );
  const value = Number(data);
  return Number.isFinite(value) ? value : 0;
};
