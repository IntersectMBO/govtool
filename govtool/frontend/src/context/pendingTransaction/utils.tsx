import { QueryClient, QueryKey } from "react-query";
import { QUERY_KEYS } from "@/consts";
import { TransactionType, TransactionState } from "./types";
import {
  AutomatedVotingOptionCurrentDelegation,
  AutomatedVotingOptionDelegationId,
} from "@/types/automatedVotingOptions";

export const getDesiredResult = (
  type: TransactionType,
  resourceId: string | undefined,
) => {
  switch (type) {
    case "delegate": {
      // current delegation
      if (resourceId === AutomatedVotingOptionDelegationId.no_confidence)
        return AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence;
      if (resourceId === AutomatedVotingOptionDelegationId.abstain)
        return AutomatedVotingOptionCurrentDelegation.drep_always_abstain;
      return resourceId;
    }
    case "registerAsDrep":
    case "registerAsDirectVoter":
      // is registered
      return true;
    case "retireAsDrep":
    case "retireAsDirectVoter":
      // is registered
      return false;
    default:
      return undefined;
  }
};

export const getQueryKey = (
  type: TransactionType,
  transaction: TransactionState | null,
) => {
  switch (type) {
    case "registerAsDrep":
    case "retireAsDrep":
    case "registerAsDirectVoter":
    case "retireAsDirectVoter":
      return [QUERY_KEYS.useGetDRepInfoKey, transaction?.transactionHash];
    case "delegate":
      return [
        QUERY_KEYS.getAdaHolderCurrentDelegationKey,
        transaction?.transactionHash,
      ];
    default:
      return undefined;
  }
};

export const refetchData = async (
  type: TransactionType,
  queryClient: QueryClient,
  queryKey: QueryKey | undefined,
  resourceId: string | undefined,
) => {
  if (queryKey === undefined) return;

  await queryClient.invalidateQueries(queryKey);
  // TODO add better type for query data
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const data = await queryClient.getQueryData<any>(queryKey);

  if (type === "delegate") {
    if (
      resourceId === AutomatedVotingOptionDelegationId.no_confidence ||
      resourceId === AutomatedVotingOptionDelegationId.abstain
    ) {
      return data?.dRepView;
    }
    return data?.dRepHash;
  }
  if (type === "registerAsDrep" || type === "retireAsDrep")
    return data?.isRegisteredAsDRep;
  if (type === "registerAsDirectVoter" || type === "retireAsDirectVoter")
    return data?.isRegisteredAsSoleVoter;
  return undefined;
};
