import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";

import "@/i18n";

import { PaginationFooter } from "./PaginationFooter";

const renderFooter = (page: number, onPageChange = vi.fn()) => {
  render(
    <PaginationFooter
      page={page}
      total={30}
      pageSize={10}
      onPageChange={onPageChange}
      onPageSizeChange={vi.fn()}
    />,
  );
  return onPageChange;
};

describe("PaginationFooter", () => {
  it("is a labelled navigation whose arrows are not read out", () => {
    renderFooter(2);
    const nav = screen.getByRole("navigation", { name: "Pagination" });
    expect(nav).toBeInTheDocument();
    expect(screen.queryByRole("button", { name: /→|←/ })).toBeNull();
  });

  it("highlights the current page", () => {
    renderFooter(2);
    const current = screen.getByRole("button", { name: "2" });
    expect(current).toHaveAttribute("aria-current", "page");
    expect(current).toHaveStyle({ backgroundColor: "#0038C7" });
    expect(screen.getByRole("button", { name: "1" })).toHaveStyle({
      backgroundColor: "#FFFFFF",
    });
  });

  it("disables Next on the last page", () => {
    renderFooter(3);
    expect(screen.getByRole("button", { name: "Next" })).toBeDisabled();
    expect(screen.getByRole("button", { name: "Previous" })).toBeEnabled();
  });

  it("goes to a page when its number is clicked", () => {
    const onPageChange = renderFooter(1);
    fireEvent.click(screen.getByRole("button", { name: "3" }));
    expect(onPageChange).toHaveBeenCalledWith(3);
  });

  it("says which results are showing", () => {
    renderFooter(2);
    expect(screen.getByText("Showing 11–20 of 30 results")).toBeInTheDocument();
  });

  it("collapses long page runs with an ellipsis", () => {
    render(
      <PaginationFooter
        page={5}
        total={200}
        pageSize={10}
        onPageChange={vi.fn()}
        onPageSizeChange={vi.fn()}
      />,
    );
    expect(screen.getByRole("button", { name: "1" })).toBeInTheDocument();
    expect(screen.getByRole("button", { name: "20" })).toBeInTheDocument();
    expect(screen.queryByRole("button", { name: "10" })).toBeNull();
    expect(screen.getAllByText("…")).toHaveLength(2);
  });

  describe("on a phone", () => {
    const desktopWidth = window.innerWidth;
    beforeEach(() => {
      window.innerWidth = 375;
    });
    afterEach(() => {
      window.innerWidth = desktopWidth;
    });

    it("shows Previous, the page count and Next instead of numbered pages", () => {
      renderFooter(2);
      expect(screen.getByText("Page 2 of 3")).toBeInTheDocument();
      expect(screen.queryByRole("button", { name: "1" })).toBeNull();
      expect(screen.getByRole("button", { name: "Next" })).toHaveStyle({
        whiteSpace: "nowrap",
        flexShrink: "0",
      });
    });
  });
});
