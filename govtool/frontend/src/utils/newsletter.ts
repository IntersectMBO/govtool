import { NEWSLETTER } from "@/consts/links";
import type { NewsItem } from "@/models/newsletter";

import { formatDisplayDate } from "./formatDate";

const isText = (value: unknown): value is string =>
  typeof value === "string" && value.trim() !== "";

/** Only a link into the newsletter site itself, over https. */
const isNewsletterUrl = (value: unknown): value is string => {
  if (typeof value !== "string") return false;
  try {
    const url = new URL(value);
    return url.protocol === "https:" && url.origin === NEWSLETTER.SITE;
  } catch {
    return false;
  }
};

/**
 * Posts from the newsletter's Ghost Content API, as Home lists them. The
 * response comes from another site, so each field is checked and a post with
 * no title or a link elsewhere is left out.
 */
export const toNewsItems = (response: unknown): NewsItem[] => {
  const posts = (response as { posts?: unknown })?.posts;
  if (!Array.isArray(posts)) return [];
  return posts.flatMap((raw) => {
    const item = raw as Record<string, unknown>;
    if (!isText(item?.title) || !isNewsletterUrl(item.url)) return [];
    const tag = (item.primary_tag as { name?: unknown } | null)?.name;
    const excerpt = isText(item.custom_excerpt) ? item.custom_excerpt : item.excerpt;
    return [
      {
        id: String(item.id ?? item.url),
        month: formatDisplayDate(item.published_at as string, "MMMM yyyy"),
        ...(isText(tag) ? { tag } : {}),
        title: item.title,
        excerpt: isText(excerpt) ? excerpt : "",
        url: item.url,
      },
    ];
  });
};

/** A plausible email address: something, an @, a domain with a dot. */
export const isEmailAddress = (value: string) =>
  /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(value.trim());
