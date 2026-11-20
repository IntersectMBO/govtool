import { describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";

import { FilterSidebar } from "./FilterSidebar";

const options = [
  { key: "InfoAction", label: "Info Action", count: 2 },
  { key: "HardForkInitiation", label: "Hard Fork", count: 0 },
];

const renderSidebar = (selected: string[], onChange = vi.fn()) => {
  render(
    <FilterSidebar
      allCount={2}
      allLabel="All Governance Actions"
      onChange={onChange}
      options={options}
      selected={selected}
      title="Action type"
    />,
  );
  return onChange;
};

describe("FilterSidebar", () => {
  it("marks the all row selected when nothing is chosen", () => {
    renderSidebar([]);
    expect(screen.getByTestId("AllGovernanceActions-checkbox")).toHaveAttribute("aria-pressed", "true");
    expect(screen.getByTestId("InfoAction-checkbox")).toHaveAttribute("aria-pressed", "false");
  });

  it("adds and removes an option on click", () => {
    const onChange = renderSidebar(["InfoAction"]);
    fireEvent.click(screen.getByTestId("HardFork-checkbox"));
    expect(onChange).toHaveBeenLastCalledWith(["InfoAction", "HardForkInitiation"]);
    fireEvent.click(screen.getByTestId("InfoAction-checkbox"));
    expect(onChange).toHaveBeenLastCalledWith([]);
  });

  it("clears the selection from the all row", () => {
    const onChange = renderSidebar(["InfoAction"]);
    fireEvent.click(screen.getByTestId("AllGovernanceActions-checkbox"));
    expect(onChange).toHaveBeenLastCalledWith([]);
  });

  it("shows counts, zero included", () => {
    renderSidebar([]);
    expect(screen.getByTestId("HardFork-checkbox")).toHaveTextContent("0");
    expect(screen.getByTestId("InfoAction-checkbox")).toHaveTextContent("2");
  });

  it("draws checkboxes that say which rows are chosen", () => {
    render(
      <FilterSidebar
        allLabel="All Status"
        checkboxes
        onChange={vi.fn()}
        options={[
          { key: "expired", label: "Expired", color: "#B42318" },
          { key: "enacted", label: "Enacted" },
        ]}
        selected={["expired"]}
      />,
    );
    const expired = screen.getByTestId("Expired-checkbox");
    expect(expired).toHaveAttribute("role", "checkbox");
    expect(expired).toHaveAttribute("aria-checked", "true");
    expect(screen.getByTestId("Enacted-checkbox")).toHaveAttribute("aria-checked", "false");
    expect(screen.getByTestId("AllStatus-checkbox")).toHaveAttribute("aria-checked", "false");
    expect(screen.getByText("Expired")).toHaveStyle({ color: "#B42318" });
  });
});
