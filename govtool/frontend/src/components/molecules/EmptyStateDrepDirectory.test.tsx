import { describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";

import "@/i18n";

import { EmptyStateDrepDirectory } from "./EmptyStateDrepDirectory";

describe("EmptyStateDrepDirectory", () => {
  it("quotes the search and offers both ways out", () => {
    const onClearSearch = vi.fn();
    const onBrowseAll = vi.fn();
    render(
      <EmptyStateDrepDirectory
        onBrowseAll={onBrowseAll}
        onClearSearch={onClearSearch}
        searchTerm="ankit"
      />,
    );
    expect(screen.getByText('"ankit"')).toBeInTheDocument();
    fireEvent.click(screen.getByTestId("clear-search-button"));
    fireEvent.click(screen.getByTestId("browse-all-dreps-button"));
    expect(onClearSearch).toHaveBeenCalled();
    expect(onBrowseAll).toHaveBeenCalled();
  });

  it("offers no clear-search button when no search caused it", () => {
    render(<EmptyStateDrepDirectory onBrowseAll={vi.fn()} onClearSearch={vi.fn()} />);
    expect(screen.queryByTestId("clear-search-button")).toBeNull();
    expect(screen.getByTestId("browse-all-dreps-button")).toBeInTheDocument();
  });
});
