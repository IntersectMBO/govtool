export interface IDRepIdentity {
  id: string;
  key: string;
}

export interface KuberValue {
  [policyId: string]: Record<string, BigInt | number> | BigInt | number;
}

export interface KuberBalanceResponse {
  address: string;
  txin: string;
  value: KuberValue;
}
