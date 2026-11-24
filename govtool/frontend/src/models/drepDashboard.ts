// Reads behind a DRep's own dashboard that the backend may not serve yet.
// The shapes are the routes in docs/api/drep-dashboard.md.

/** An epoch and the time it is dated by. */
export type EpochStamp = { epoch: number; time: string };

/** GET /drep/{drepId}/delegators, one row. Lovelace as a decimal string. */
export type DRepDelegatorRow = {
  stakeAddress: string;
  votingPower: string;
  delegatedAt: EpochStamp | null;
};

export type DRepHistoryKind =
  | "registered"
  | "reregistered"
  | "updated"
  | "retired"
  | "statusChanged";

/** GET /drep/{drepId}/history, one row. */
export type DRepHistoryRow = {
  kind: DRepHistoryKind;
  /** The DRep's status after this event. */
  status: "active" | "inactive" | "retired";
  at: EpochStamp;
  /** Null for a status change, which has no certificate. */
  txHash: string | null;
  anchor: { url: string; hash: string } | null;
};

/** GET /account/{stakeAddress}/delegation-history, one row. */
export type DelegationHistoryRow = {
  target: {
    type: "drep" | "abstain" | "noConfidence";
    dRepId: string | null;
  };
  at: EpochStamp;
  txHash: string;
};

/** GET /drep/{drepId}/voting-power-history, one point per epoch. */
export type VotingPowerHistoryPoint = {
  epoch: number;
  votingPower: string;
  delegators: number | null;
};

/** A forum thread the DRep posted or commented on. */
export type DRepDiscussion = {
  id: number;
  title: string;
  abstract: string;
  topic: string | null;
  author: string;
  isOwn: boolean;
  createdAt: string | null;
  replies: number;
  likes: number;
  repliedByYou: boolean;
  lastReply: { author: string; at: string; isOwn: boolean } | null;
};
