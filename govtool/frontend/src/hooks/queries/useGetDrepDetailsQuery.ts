import { useGetDRepListInfiniteQuery } from "./useGetDRepListQuery";

export const useGetDRepDetailsQuery = (
  dRepId: string | null | undefined,
  options?: { enabled: boolean },
) => {
  const { dRepData, isDRepListLoading } = useGetDRepListInfiniteQuery(
    { searchPhrase: dRepId ?? undefined },
    { enabled: options?.enabled && !!dRepId },
  );

  return { dRep: dRepData?.[0], isLoading: isDRepListLoading };
};
