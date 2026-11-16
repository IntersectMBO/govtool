import { describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
import { useState } from "react";

import { SegmentedTabs } from "./SegmentedTabs";

const TABS = [
  { value: "toVote", label: "To vote", dataTestId: "to-vote-tab" },
  { value: "voted", label: "Voted on", dataTestId: "voted-tab" },
];

const Harness = ({ onChange = vi.fn() }: { onChange?: (value: string) => void }) => {
  const [value, setValue] = useState("toVote");
  return (
    <>
      <SegmentedTabs
        idPrefix="list"
        label="Governance actions"
        onChange={(next) => {
          setValue(next);
          onChange(next);
        }}
        tabs={TABS}
        value={value}
      />
      <div {...SegmentedTabs.panelProps("list", value)}>panel</div>
    </>
  );
};

describe("SegmentedTabs", () => {
  it("is a labelled tablist whose tabs control the panel", () => {
    render(<Harness />);
    expect(screen.getByRole("tablist", { name: "Governance actions" })).toBeInTheDocument();
    const tab = screen.getByRole("tab", { name: "To vote" });
    expect(tab).toHaveAttribute("aria-selected", "true");
    expect(tab).toHaveAttribute("aria-controls", "list-panel");
    const panel = screen.getByRole("tabpanel");
    expect(panel).toHaveAttribute("id", "list-panel");
    expect(panel).toHaveAttribute("aria-labelledby", tab.id);
  });

  it("keeps only the selected tab in the tab order", () => {
    render(<Harness />);
    expect(screen.getByRole("tab", { name: "To vote" })).toHaveAttribute("tabindex", "0");
    expect(screen.getByRole("tab", { name: "Voted on" })).toHaveAttribute("tabindex", "-1");
  });

  it("moves between tabs with the arrow keys and focuses the new one", () => {
    const onChange = vi.fn();
    render(<Harness onChange={onChange} />);
    fireEvent.keyDown(screen.getByRole("tab", { name: "To vote" }), { key: "ArrowRight" });
    expect(onChange).toHaveBeenLastCalledWith("voted");
    expect(document.activeElement).toBe(screen.getByRole("tab", { name: "Voted on" }));
    fireEvent.keyDown(screen.getByRole("tab", { name: "Voted on" }), { key: "ArrowRight" });
    expect(onChange).toHaveBeenLastCalledWith("toVote");
    fireEvent.keyDown(screen.getByRole("tab", { name: "To vote" }), { key: "End" });
    expect(onChange).toHaveBeenLastCalledWith("voted");
  });

  it("keeps the old test ids", () => {
    render(<Harness />);
    expect(screen.getByTestId("to-vote-tab")).toBeInTheDocument();
    expect(screen.getByTestId("voted-tab")).toBeInTheDocument();
  });
});
