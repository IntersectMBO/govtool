import { QueryClient, QueryKey } from "@tanstack/react-query";
import { QUERY_KEYS } from "@/consts";
import { TransactionType, TransactionState } from "./types";
import {
  AutomatedVotingOptionCurrentDelegation,
  AutomatedVotingOptionDelegationId,
} from "@/types/automatedVotingOptions";
import { CurrentDelegation, VoterInfo } from "@/models";

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

    case "vote":
      return true;

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
    case "vote":
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

  await queryClient.invalidateQueries({ queryKey });

  if (type === "delegate") {
    const data = await queryClient.getQueryData<CurrentDelegation>(queryKey);
    if (
      resourceId === AutomatedVotingOptionDelegationId.no_confidence ||
      resourceId === AutomatedVotingOptionDelegationId.abstain
    ) {
      return data?.dRepView;
    }
    return data?.dRepHash;
  }

  // Voter info is also keyed by the DRep ID after this prefix, so match by
  // prefix; only the wallet that sent this transaction has entries under it.
  const data =
    queryKey[0] === QUERY_KEYS.useGetDRepInfoKey
      ? queryClient
          .getQueriesData<VoterInfo>({ queryKey })
          .map(([, value]) => value)
          .find((value) => value !== undefined)
      : await queryClient.getQueryData(queryKey);

  if (type === "registerAsDrep" || type === "retireAsDrep")
    return (data as VoterInfo)?.isRegisteredAsDRep;
  if (type === "registerAsDirectVoter" || type === "retireAsDirectVoter")
    return (data as VoterInfo)?.isRegisteredAsSoleVoter;
  if (type === "vote") {
    return true;
  }
  return undefined;
};
