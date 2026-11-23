import axios from "axios";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { getNewsletterPosts, subscribeToNewsletter } from "./newsletter";

vi.mock("axios", () => ({ default: { get: vi.fn(), post: vi.fn() } }));

const get = vi.mocked(axios.get);
const post = vi.mocked(axios.post);

describe("newsletter requests", () => {
  beforeEach(() => {
    get.mockReset();
    post.mockReset();
  });

  it("reads one page of posts with their tags and the next page", async () => {
    get.mockResolvedValue({
      data: {
        posts: [
          {
            id: "1",
            title: "Digest",
            url: "https://newsletter.gov.tools/digest/",
            excerpt: "x",
            published_at: "2026-11-06T00:00:00Z",
          },
        ],
        meta: { pagination: { next: 2 } },
      },
    });
    const page = await getNewsletterPosts(1);
    expect(get).toHaveBeenCalledWith("https://newsletter.gov.tools/ghost/api/content/posts/", {
      params: { key: "896d614e96f501eb3f1d2fdb9a", limit: 4, page: 1, include: "tags" },
    });
    expect(page.items.map((item) => item.title)).toEqual(["Digest"]);
    expect(page.nextPage).toBe(2);
  });

  it("signs up with Ghost's integrity token, as its portal does", async () => {
    get.mockResolvedValue({ data: "token-1" });
    post.mockResolvedValue({ status: 201 });
    await subscribeToNewsletter(" you@example.com ");
    expect(get).toHaveBeenCalledWith(
      "https://newsletter.gov.tools/members/api/integrity-token/",
      { responseType: "text" },
    );
    expect(post).toHaveBeenCalledWith(
      "https://newsletter.gov.tools/members/api/send-magic-link/",
      { email: "you@example.com", emailType: "signup", integrityToken: "token-1" },
    );
  });
});
