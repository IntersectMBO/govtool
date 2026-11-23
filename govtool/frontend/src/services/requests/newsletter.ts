import axios from "axios";

import { NEWSLETTER } from "@/consts/links";
import type { NewsPage } from "@/models/newsletter";
import { toNewsItems } from "@/utils/newsletter";

const PAGE_SIZE = 4;

/**
 * One page of the newsletter's published posts, from its Ghost Content API.
 * Plain axios, not GovTool's API instance: another site, and a failure stays
 * in Home's section instead of redirecting to the error page.
 */
export const getNewsletterPosts = async (page: number): Promise<NewsPage> => {
  const { data } = await axios.get(`${NEWSLETTER.SITE}/ghost/api/content/posts/`, {
    params: { key: NEWSLETTER.CONTENT_KEY, limit: PAGE_SIZE, page, include: "tags" },
  });
  const next = data?.meta?.pagination?.next;
  return {
    items: toNewsItems(data),
    nextPage: typeof next === "number" ? next : null,
  };
};

/**
 * Asks Ghost to email a sign-up link, as its own portal does: fetch an
 * integrity token, then send the address. The subscription starts only once
 * the reader follows that link.
 */
export const subscribeToNewsletter = async (email: string) => {
  const { data: integrityToken } = await axios.get<string>(
    `${NEWSLETTER.SITE}/members/api/integrity-token/`,
    { responseType: "text" },
  );
  await axios.post(`${NEWSLETTER.SITE}/members/api/send-magic-link/`, {
    email: email.trim(),
    emailType: "signup",
    integrityToken,
  });
};
