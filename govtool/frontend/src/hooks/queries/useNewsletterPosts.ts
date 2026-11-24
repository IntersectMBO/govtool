import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@/consts/queryKeys";
import { getNewsletterPosts } from "@/services/requests/newsletter";

const TEN_MINUTES = 10 * 60 * 1000;

/** The newsletter's latest posts; Home links to the newsletter for the rest. */
export const useNewsletterPosts = () => {
  const query = useQuery({
    queryKey: [QUERY_KEYS.newsletterPostsKey],
    queryFn: () => getNewsletterPosts(1),
    staleTime: TEN_MINUTES,
    retry: 1,
  });
  return {
    items: query.data?.items ?? [],
    isLoading: query.isLoading,
    isError: query.isError,
  };
};
