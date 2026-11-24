import { GovernanceActionsAPI } from "../GovernanceActionsAPI";

/** A stake key's voting power in lovelace; a failure stays in its panel. */
export const getStakeKeyVotingPower = async (stakeKey: string) => {
  const { data } = await GovernanceActionsAPI.get<number>(
    `/ada-holder/get-voting-power/${encodeURIComponent(stakeKey)}`,
  );
  const value = typeof data === "number" ? data : Number.NaN;
  // Refuse rather than show a made-up ₳ 0.
  if (!Number.isFinite(value)) throw new Error("Voting power is not a number");
  return value;
};
