import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import { useTranslation } from "react-i18next";
import { beforeEach, describe, expect, it, vi } from "vitest";

import "@/i18n";

import { HomeNews } from "./HomeNews";

const news = vi.hoisted(() => ({
  value: {
    items: [] as unknown[],
    isLoading: false,
    isError: false,
  },
}));
const subscribe = vi.hoisted(() => vi.fn());

vi.mock("@hooks", () => ({ useTranslation }));
vi.mock("@/hooks/queries/useNewsletterPosts", () => ({
  useNewsletterPosts: () => news.value,
}));
vi.mock("@/services/requests/newsletter", () => ({
  subscribeToNewsletter: subscribe,
}));

const ITEM = {
  id: "1",
  month: "November 2026",
  tag: "Governance Digest",
  title: "Cardano Governance Digest",
  excerpt: "This week in Cardano governance.",
  url: "https://newsletter.gov.tools/digest/",
};

describe("HomeNews", () => {
  beforeEach(() => {
    news.value = { ...news.value, items: [ITEM], isError: false };
    subscribe.mockReset();
  });

  it("numbers each post with its month, tag and a link to read it", () => {
    render(<HomeNews />);
    const item = screen.getByTestId("home-news-item-0");
    expect(item).toHaveTextContent("01");
    expect(item).toHaveTextContent("November 2026");
    expect(item).toHaveTextContent("Governance Digest");
    expect(screen.getByRole("link", { name: "Read Cardano Governance Digest" })).toHaveAttribute(
      "href",
      ITEM.url,
    );
  });

  it("sends the reader to the newsletter for every other post", () => {
    render(<HomeNews />);
    expect(screen.queryByTestId("home-news-load-more")).toBeNull();
    const all = screen.getByTestId("home-news-view-all");
    expect(all).toHaveAttribute("href", "https://newsletter.gov.tools");
    expect(all).toHaveAttribute("target", "_blank");
  });

  it("still offers the newsletter when the posts cannot be read", () => {
    news.value = { ...news.value, items: [], isError: true };
    render(<HomeNews />);
    expect(screen.getByTestId("home-news-error")).toBeInTheDocument();
    expect(screen.getByTestId("home-news-view-all")).toBeInTheDocument();
  });

  it("refuses an address that is not one, without a request", () => {
    render(<HomeNews />);
    fireEvent.change(screen.getByLabelText("Email address"), { target: { value: "you@example" } });
    fireEvent.click(screen.getByTestId("newsletter-subscribe-button"));
    expect(screen.getByTestId("newsletter-status")).toHaveTextContent("valid email");
    expect(subscribe).not.toHaveBeenCalled();
  });

  it("asks the reader to confirm by email once Ghost accepts the address", async () => {
    subscribe.mockResolvedValue(undefined);
    render(<HomeNews />);
    fireEvent.change(screen.getByLabelText("Email address"), {
      target: { value: "you@example.com" },
    });
    fireEvent.click(screen.getByTestId("newsletter-subscribe-button"));
    await waitFor(() =>
      expect(screen.getByTestId("newsletter-status")).toHaveTextContent("Check your inbox"));
    expect(subscribe).toHaveBeenCalledWith("you@example.com");
  });

  it("says the sign-up failed when Ghost refuses it", async () => {
    subscribe.mockRejectedValue(new Error("429"));
    render(<HomeNews />);
    fireEvent.change(screen.getByLabelText("Email address"), {
      target: { value: "you@example.com" },
    });
    fireEvent.click(screen.getByTestId("newsletter-subscribe-button"));
    await waitFor(() =>
      expect(screen.getByTestId("newsletter-status")).toHaveTextContent("could not subscribe"));
  });
});
