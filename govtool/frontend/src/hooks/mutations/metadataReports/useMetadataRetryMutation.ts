import { useMutation, useQueryClient } from "@tanstack/react-query";

import { MUTATION_KEYS, QUERY_KEYS } from "@consts";
import { postMetadataRetry } from "@services";
import type { MetadataAnchor, MetadataRefreshOutcome } from "@models";

/**
 * Retry an anchor past its cached failure (D118, D125). Whatever comes back is
 * the latest result for the anchor, so it replaces the resolve query's data;
 * the history gains a row only when something was actually fetched.
 */
export const useMetadataRetryMutation = (anchor: MetadataAnchor) => {
  const queryClient = useQueryClient();

  const { mutateAsync, isPending } = useMutation({
    mutationKey: [MUTATION_KEYS.postMetadataRetryKey, anchor.hash, anchor.url],
    mutationFn: () => postMetadataRetry(anchor.hash, anchor.url),
    onSuccess: (outcome: MetadataRefreshOutcome) => {
      queryClient.setQueryData(
        [QUERY_KEYS.useGetMetadataResolveKey, anchor.hash, anchor.url],
        outcome.result,
      );
      if (outcome.refetched) {
        queryClient.invalidateQueries({
          queryKey: [QUERY_KEYS.useGetMetadataReportsKey, anchor.hash, anchor.url],
        });
      }
      if (outcome.result.ok) {
        // The document now resolves: drop every cached view of it so the page
        // re-renders with the metadata instead of the old failure.
        queryClient.invalidateQueries({
          queryKey: [MUTATION_KEYS.postValidateKey, anchor.hash, anchor.url],
        });
        queryClient.invalidateQueries({
          queryKey: [QUERY_KEYS.useGetProposalKey],
        });
        queryClient.invalidateQueries({
          queryKey: [QUERY_KEYS.useGetDRepListInfiniteKey],
        });
      }
    },
  });

  return { retry: mutateAsync, isRetrying: isPending };
};
