export type TransactionTypeWithoutResource =
  | "createGovAction"
  | "registerAsDrep"
  | "registerAsSoleVoter"
  | "retireAsDrep"
  | "retireAsSoleVoter"
  | "updateMetaData";

export type TransactionTypeWithResource = "delegate" | "vote";

export type TransactionType =
  | TransactionTypeWithoutResource
  | TransactionTypeWithResource;

export type TransactionStateWithoutResource = {
  type: TransactionTypeWithoutResource;
  transactionHash: string;
  time: Date;
  resourceId?: never;
};

export type TransactionStateWithResource = {
  type: TransactionTypeWithResource;
  transactionHash: string;
  time: Date;
  resourceId: string;
};

export type TransactionState =
  | TransactionStateWithResource
  | TransactionStateWithoutResource;

export type PendingTransaction =
  | Record<
    TransactionTypeWithoutResource,
    TransactionStateWithoutResource | null
  > &
  Record<
    TransactionTypeWithResource,
    TransactionStateWithResource | null
  >;
