import { useQuery } from "react-query";

import { getVoteContextTextFromFile } from "@/services";
import { QUERY_KEYS } from "@/consts";
import { useCardano } from "@/context";
import { useGetVoterInfo } from ".";

export const useGetVoteContextTextFromFile = (url: string | undefined,
                                            contextHash : string | undefined) => {
  const { dRepID } = useCardano();
  const { voter } = useGetVoterInfo();

  const { data, isLoading } = useQuery(
  [QUERY_KEYS.useGetVoteContextFromFile, url],
  () => getVoteContextTextFromFile(url, contextHash),
  {
    enabled:
      !!url &&
      !!dRepID &&
      (!!voter?.isRegisteredAsDRep || !!voter?.isRegisteredAsSoleVoter),
  },
  );

  const voteContextText = (data?.metadata as { comment?: string })?.comment || "";

  if (data?.valid) {
    return {
      voteContextText,
      isLoading
    };
  }
  return {
      voteContextText: undefined,
      isLoading
    };
};
