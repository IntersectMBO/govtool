/**
 * Chain Data API — `/transactions/*`
 *
 * Confirmation status and the governance effects a transaction produced. This
 * is the post-submission polling path: the wallet returns a tx hash, and the
 * UI needs to know when it landed and what it did.
 */

import type { Envelope, EpochStamp, Hex } from "./common";
import type { GovActionRef, VoterRef } from "./refs";
import type { Delegation } from "./accounts";
import type { VoteRecord } from "./governance/votes";

export type TxStatus = "unknown" | "pending" | "confirmed" | "failed";

export type TxGovernanceEffect =
  | { kind: "vote"; vote: VoteRecord }
  | { kind: "drepRegistration"; drep: VoterRef; action: "register" | "update" | "retire" }
  | { kind: "delegation"; delegation: Delegation }
  | { kind: "proposal"; proposal: GovActionRef };

export interface TransactionState {
  txHash: Hex;
  status: TxStatus;
  confirmations?: number;
  includedAt?: EpochStamp;
  /** Governance effects observed for this tx, so success screens can be precise. */
  effects: TxGovernanceEffect[];
}

export interface TransactionsApi {
  /** `GET /transactions/{txHash}` */
  get(txHash: string): Promise<Envelope<TransactionState>>;
}
