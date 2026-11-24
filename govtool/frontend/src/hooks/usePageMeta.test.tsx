import { ReactNode } from "react";
import { act, renderHook } from "@testing-library/react";
import { MemoryRouter, useNavigate } from "react-router";
import { beforeEach, describe, expect, it } from "vitest";

import { usePageMeta } from "./usePageMeta";

const SERVER_HEAD = [
  '<title data-seo>Treasury withdrawal: X | Cardano GovTool</title>',
  '<meta name="description" content="Written by the server." data-seo>',
  '<link rel="canonical" href="https://gov.tools/actions/x" data-seo>',
  '<meta name="robots" content="noindex" data-seo>',
].join("");

const render = (initialPath: string) =>
  renderHook(
    () => {
      usePageMeta();
      return useNavigate();
    },
    {
      wrapper: ({ children }: { children: ReactNode }) => (
        <MemoryRouter initialEntries={[initialPath]}>{children}</MemoryRouter>
      ),
    },
  );

const canonical = () =>
  document.head
    .querySelector('link[rel="canonical"][data-seo]')
    ?.getAttribute("href");
const description = () =>
  document.head
    .querySelector('meta[name="description"][data-seo]')
    ?.getAttribute("content");

describe("usePageMeta", () => {
  beforeEach(() => {
    document.head.innerHTML = SERVER_HEAD;
  });

  it("leaves the server's tags alone on the first page", () => {
    render("/actions/x");
    expect(document.title).toBe("Treasury withdrawal: X | Cardano GovTool");
    expect(description()).toBe("Written by the server.");
    expect(canonical()).toBe("https://gov.tools/actions/x");
  });

  it("updates the tags in place after in-app navigation", () => {
    const { result } = render("/actions/x");
    act(() => {
      result.current("/dreps");
    });
    expect(document.title).toBe("DRep Directory | Cardano GovTool");
    expect(document.head.querySelectorAll("title")).toHaveLength(1);
    expect(canonical()).toBe(`${window.location.origin}/dreps`);
    expect(description()).toBe(
      "Take part in Cardano on-chain governance: read and vote on governance actions, delegate to a DRep, or become one.",
    );
    expect(document.head.querySelector('meta[name="robots"]')).toBeNull();
  });

  it("gives the home page its brand-first title", () => {
    const { result } = render("/actions/x");
    act(() => {
      result.current("/");
    });
    expect(document.title).toBe(
      "Cardano GovTool: vote, delegate and follow Cardano governance",
    );
  });

  it("drops the canonical for a link whose #index names the action", () => {
    const { result } = render("/dreps");
    act(() => {
      result.current(`/governance_actions/${"ab".repeat(32)}#1`);
    });
    // Chrome on Android shares the canonical: without one it shares the
    // whole URL, index included.
    expect(canonical()).toBeUndefined();
    act(() => {
      result.current("/faqs");
    });
    expect(canonical()).toBe(`${window.location.origin}/faqs`);
  });

  it("adds the tags when the first page had none", () => {
    document.head.innerHTML = "";
    const { result } = render("/");
    act(() => {
      result.current("/faqs");
    });
    expect(canonical()).toBe(`${window.location.origin}/faqs`);
    expect(description()).toBeTruthy();
  });
});
