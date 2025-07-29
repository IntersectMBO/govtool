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
  enabled = true,
}: GetProposalsArguments) => {
  const { dRepID } = useCardano();
  const { voter } = useGetVoterInfo();

// Determine if voter is ready to be used in the query
const voterReady =
  !!dRepID && (voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter);

// Only run the query if enabled externally and voter info is ready
const shouldFetch = enabled && voterReady;


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

  const { data, isLoading } = useQuery(
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
      enabled: shouldFetch,
      refetchOnWindowFocus: true,
      keepPreviousData: true,
    },
  );

  const proposals = Object.values(groupByType(data) ?? []);

  return {
    isProposalsLoading: isLoading,
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
