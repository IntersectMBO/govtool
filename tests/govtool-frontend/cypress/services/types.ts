export interface IFaucetResponse {
  amount: {
    lovelace: number;
  };
  txid: string;
  txin: string;
}
