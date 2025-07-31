import { useQuery } from "react-query";

import { getVoteContextTextFromFile } from "@/services";
import { QUERY_KEYS } from "@/consts";
import { useCardano } from "@/context";
import { useGetVoterInfo } from ".";

export const useGetVoteContextTextFromFile = (url: string | undefined,
                                            contextHash : string | undefined) => {
  const { dRepID } = useCardano();
  const { voter } = useGetVoterInfo();

  if (url && url.startsWith("ipfs://")) {
      url = url.replace("ipfs://", "https://ipfs.io/ipfs/");
  }

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

  if (url === undefined || contextHash === undefined) {
    return {
      voteContextText: undefined,
      isLoading: false,
      valid: true
    };
  }
  if (data) {
    if (data?.valid) {
      return {
        voteContextText,
        isLoading,
        valid: true
      };
    }
    return {
        voteContextText: undefined,
        isLoading,
        valid: false
      };
  }
};
