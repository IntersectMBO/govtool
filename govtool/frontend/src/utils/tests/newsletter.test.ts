import { describe, expect, it } from "vitest";

import { isEmailAddress, toNewsItems } from "../newsletter";

const post = (over: Record<string, unknown> = {}) => ({
  id: "1",
  title: "Cardano Governance Digest : 26 Oct – 2 Nov 2026",
  url: "https://newsletter.gov.tools/cardano-governance-digest/",
  excerpt: "This week in Cardano governance: 1 new action.",
  custom_excerpt: null,
  published_at: "2026-11-06T09:39:11.000+00:00",
  primary_tag: { name: "Governance Digest" },
  ...over,
});

describe("toNewsItems", () => {
  it("maps a post to its month, tag, title, excerpt and link", () => {
    expect(toNewsItems({ posts: [post()] })).toEqual([
      {
        id: "1",
        month: "November 2026",
        tag: "Governance Digest",
        title: "Cardano Governance Digest : 26 Oct – 2 Nov 2026",
        excerpt: "This week in Cardano governance: 1 new action.",
        url: "https://newsletter.gov.tools/cardano-governance-digest/",
      },
    ]);
  });

  it("prefers the author's excerpt and drops a missing tag", () => {
    const [item] = toNewsItems({
      posts: [post({ custom_excerpt: "Written by hand.", primary_tag: null })],
    });
    expect(item.excerpt).toBe("Written by hand.");
    expect(item.tag).toBeUndefined();
  });

  it("keeps only posts that link into the newsletter site over https", () => {
    const posts = [
      // eslint-disable-next-line no-script-url -- the case under test
      post({ id: "a", url: "javascript:alert(1)" }),
      post({ id: "b", url: "http://newsletter.gov.tools/x/" }),
      post({ id: "c", url: "https://evil.example/newsletter.gov.tools/" }),
      post({ id: "d", title: 42 }),
      post({ id: "e" }),
    ];
    expect(toNewsItems({ posts }).map((item) => item.id)).toEqual(["e"]);
  });

  it("reads a malformed response as no posts", () => {
    expect(toNewsItems(undefined)).toEqual([]);
    expect(toNewsItems({ posts: "nope" })).toEqual([]);
  });
});

describe("isEmailAddress", () => {
  it("accepts an address and refuses anything else", () => {
    expect(isEmailAddress(" you@example.com ")).toBe(true);
    expect(isEmailAddress("you@example")).toBe(false);
    expect(isEmailAddress("you example.com")).toBe(false);
    expect(isEmailAddress("")).toBe(false);
  });
});
