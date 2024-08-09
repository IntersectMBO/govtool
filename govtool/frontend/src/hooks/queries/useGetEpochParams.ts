import { useQuery } from "react-query";

import { getEpochParams } from "@services";
import { QUERY_KEYS } from "@consts";

export const useGetEpochParams = () => {
  const { data: epochParams, refetch: fetchEpochParams } = useQuery({
    queryKey: QUERY_KEYS.useGetEpochParamsKey,
    queryFn: () => getEpochParams(),
    enabled: false,
  });

  return { epochParams, fetchEpochParams };
};
