/** One newsletter post as Home lists it. */
export type NewsItem = {
  id: string;
  /** "November 2026", from the publish date. */
  month: string;
  /** The post's primary tag, such as "Governance Digest". */
  tag?: string;
  title: string;
  /** Plain text: the author's excerpt, or Ghost's own. */
  excerpt: string;
  url: string;
};

/** A page of posts with the next page number, if there is one. */
export type NewsPage = {
  items: NewsItem[];
  nextPage: number | null;
};
