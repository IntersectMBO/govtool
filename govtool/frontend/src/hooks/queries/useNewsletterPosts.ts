import { useInfiniteQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@/consts/queryKeys";
import { getNewsletterPosts } from "@/services/requests/newsletter";

const TEN_MINUTES = 10 * 60 * 1000;

/** The newsletter's posts, four at a time, newest first; Load More reads on. */
export const useNewsletterPosts = () => {
  const query = useInfiniteQuery({
    queryKey: [QUERY_KEYS.newsletterPostsKey],
    queryFn: ({ pageParam }) => getNewsletterPosts(pageParam),
    initialPageParam: 1,
    getNextPageParam: (last) => last.nextPage ?? undefined,
    staleTime: TEN_MINUTES,
    retry: 1,
  });
  return {
    items: query.data?.pages.flatMap((page) => page.items) ?? [],
    isLoading: query.isLoading,
    isError: query.isError,
    hasMore: query.hasNextPage,
    isLoadingMore: query.isFetchingNextPage,
    loadMore: () => query.fetchNextPage(),
  };
};
