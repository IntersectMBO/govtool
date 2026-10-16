import { useGetDRepListInfiniteQuery } from "./useGetDRepListInfiniteQuery";

export const useGetDRepDetailsQuery = (
  dRepId: string | null | undefined,
  options?: { enabled?: boolean },
) => {
  const { dRepData, isDRepListLoading } = useGetDRepListInfiniteQuery(
    { searchPhrase: dRepId ?? undefined },
    { enabled: options?.enabled || !!dRepId, ...options },
  );

  return { dRep: dRepData?.[0], isLoading: isDRepListLoading };
};
