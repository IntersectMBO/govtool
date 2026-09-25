export type CurrentDelegationRow = {
  drep_raw: string | null;
  drep_view: string;
  has_script: boolean;
  encode: string;
};

export type DelegationResponse = {
  dRepHash: string | null;
  dRepView: string;
  isDRepScriptBased: boolean;
  txHash: string;
};

export type VotingPowerRow = {
  total_balance: number | string;
  stake_address: string;
};
