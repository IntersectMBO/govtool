import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";

import "@/i18n";

import type { GovernanceActionVoteAggregate } from "@/models";
import { VoteTallyRow } from "./VoteTallyRow";

const aggregate: GovernanceActionVoteAggregate = {
  role: "drep",
  representation: "stake",
  yes: "70",
  no: "10",
  abstain: "0",
  notVoted: "20",
  totalEligible: "100",
  threshold: { numerator: 67, denominator: 100 },
};

describe("VoteTallyRow", () => {
  it("shows yes and threshold percentages", () => {
    render(<VoteTallyRow aggregate={aggregate} dataTestId="row" voterRole="drep" />);
    expect(screen.getByTestId("row")).toHaveTextContent("70.0% / 67.0%");
  });

  it("says not eligible when the role does not vote", () => {
    render(<VoteTallyRow dataTestId="row" voterRole="spo" />);
    expect(screen.getByTestId("row")).toHaveTextContent(/not eligible/i);
  });

  it("shows a dash, not 'not eligible', while the tally is unknown", () => {
    render(<VoteTallyRow dataTestId="row" isUnknown voterRole="cc" variant="stacked" />);
    expect(screen.getByTestId("row")).toHaveTextContent("—");
    expect(screen.getByTestId("row")).not.toHaveTextContent(/not eligible/i);
  });
});

describe("VoteTallyRow pass colouring", () => {
  it("colours a passing yes share green", () => {
    render(<VoteTallyRow aggregate={aggregate} voterRole="drep" />);
    expect(screen.getByText("70.0%")).toHaveStyle({ color: "#039855" });
  });

  it("keeps the yes share neutral when pass or fail does not apply", () => {
    render(<VoteTallyRow aggregate={aggregate} showPassing={false} voterRole="drep" />);
    expect(screen.getByText("70.0%")).toHaveStyle({ color: "#101828" });
  });
});
