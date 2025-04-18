import { UseInfiniteQueryOptions } from "react-query";

import { Infinite, DRepData } from "@/models";
import { useGetDRepListInfiniteQuery } from "./useGetDRepListQuery";

export const useGetDRepDetailsQuery = (
  dRepId: string | null | undefined,
  options?: UseInfiniteQueryOptions<Infinite<DRepData>>,
) => {
  const { dRepData, isDRepListLoading } = useGetDRepListInfiniteQuery(
    { searchPhrase: dRepId ?? undefined },
    {
      enabled: options?.enabled || !!dRepId,
      ...options,
      keepPreviousData: false,
      refetchOnWindowFocus: true,
      cacheTime: 0,
      staleTime: 0,
    },
  );

  return { dRep: dRepData?.[0], isLoading: isDRepListLoading };
};
