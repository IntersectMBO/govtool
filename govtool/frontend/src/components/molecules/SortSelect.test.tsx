import { describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";

import "@/i18n";

import { SortSelect } from "./SortSelect";

const options = [
  { key: "NewestCreated", label: "Newest first" },
  { key: "SoonestToExpire", label: "Soon to expire" },
];

describe("SortSelect", () => {
  it("names the chosen order, or the default when none is chosen", () => {
    const { rerender } = render(<SortSelect onChange={vi.fn()} options={options} value="" />);
    expect(screen.getByTestId("sort-button")).toHaveTextContent("Sort:Default");
    rerender(<SortSelect onChange={vi.fn()} options={options} value="SoonestToExpire" />);
    expect(screen.getByTestId("sort-button")).toHaveTextContent("Sort:Soon to expire");
  });

  it("reports the picked option and closes", () => {
    const onChange = vi.fn();
    render(<SortSelect onChange={onChange} options={options} value="" />);
    fireEvent.click(screen.getByTestId("sort-button"));
    fireEvent.click(screen.getByTestId("SoonestToExpire-radio"));
    expect(onChange).toHaveBeenCalledWith("SoonestToExpire");
    expect(screen.getByTestId("sort-button")).toHaveAttribute("aria-expanded", "false");
  });
});
