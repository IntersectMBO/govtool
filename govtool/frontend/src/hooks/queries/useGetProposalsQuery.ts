import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { ProposalData } from "@models";
import { getProposals, GetProposalsArguments } from "@services";
import { useGetVoterInfo } from ".";

export const useGetProposalsQuery = ({
  filters = [],
  searchPhrase,
  sorting,
  enabled,
}: GetProposalsArguments) => {
  const { dRepID } = useCardano();
  const { voter } = useGetVoterInfo();

  const fetchProposals = async (): Promise<ProposalData[]> => {
    const allProposals = await Promise.all(
      filters.map((filter) =>
        getProposals({
          dRepID:
            voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter
              ? dRepID
              : undefined,
          filters: [filter],
          searchPhrase,
          sorting,
        }),
      ),
    );

    return allProposals.flatMap((proposal) => proposal.elements);
  };

  const { data, isLoading, isFetching } = useQuery(
    [
      QUERY_KEYS.useGetProposalsKey,
      filters,
      searchPhrase,
      sorting,
      dRepID,
      voter?.isRegisteredAsDRep,
      voter?.isRegisteredAsSoleVoter,
    ],
    fetchProposals,
    {
      enabled,
      refetchOnWindowFocus: false,
      refetchOnReconnect: false,
      keepPreviousData: true,
    },
  );

  const isProposalsLoading = isLoading || isFetching;

  const proposals = Object.values(groupByType(data) ?? []);

  return {
    isProposalsLoading,
    proposals,
  };
};

const groupByType = (data?: ProposalData[]) =>
  data?.reduce<Record<string, ArrayElement<ToVoteDataType>>>((groups, item) => {
    const itemType = item.type;

    if (!groups[itemType]) {
      groups[itemType] = {
        title: itemType,
        actions: [],
      };
    }
    groups[itemType].actions.push(item);

    return groups;
  }, {});
