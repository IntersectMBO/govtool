import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getProposals, getProposalsArguments } from "@services";
import { getFullGovActionId } from "@utils";

export const useGetProposalsQuery = ({
  filters = [],
  sorting,
  searchPhrase,
}: getProposalsArguments) => {
  const { dRepID, pendingTransaction } = useCardano();

  const fetchProposals = async (): Promise<ActionType[]> => {
    const allProposals = await Promise.all(
      filters.map((filter) =>
        getProposals({ dRepID, filters: [filter], sorting }),
      ),
    );

    return allProposals.flatMap((proposal) => proposal.elements);
  };

  const { data, isLoading } = useQuery(
    [
      QUERY_KEYS.useGetProposalsKey,
      filters,
      sorting,
      dRepID,
      pendingTransaction.vote,
    ],
    fetchProposals,
  );

  const mappedData = Object.values(
    (groupedByType(
      data?.filter((i) =>
        getFullGovActionId(i.txHash, i.index)
          .toLowerCase()
          .includes(searchPhrase.toLowerCase()))
    ) ?? []) as ToVoteDataType
  );

  return {
    isProposalsLoading: isLoading,
    proposals: mappedData,
  };
};

const groupedByType = (data?: ActionType[]) => data?.reduce((groups, item) => {
  const itemType = item.type;

  // TODO: Provide better typing for groups
  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-expect-error
  if (!groups[itemType]) {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-expect-error
    groups[itemType] = {
      title: itemType,
      actions: [],
    };
  }
  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-expect-error
  groups[itemType].actions.push(item);

  return groups;
}, {});
