import { render, screen, waitFor } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";

import { ProposalData } from "@/models";
import { ValidatedGovernanceActionCard } from "./ValidatedGovernanceActionCard";
import { ValidatedGovernanceVotedOnCard } from "./ValidatedGovernanceVotedOnCard";

vi.mock("@/hooks/mutations", () => ({
  useValidateMutation: () => ({
    validateMetadata: vi.fn().mockResolvedValue({
      status: undefined,
      metadata: { title: "Title from metadata" },
    }),
  }),
}));

// The cards themselves are out of scope: show what the wrappers pass down.
// (`../molecules` resolves to the same module as `@molecules`.)
vi.mock("@molecules", () => ({
  GovernanceActionCard: ({
    title,
    inProgress,
  }: {
    title?: string;
    inProgress?: boolean;
  }) => (
    <p>
      {title} {inProgress ? "in-progress" : "idle"}
    </p>
  ),
  GovernanceVotedOnCard: ({
    votedProposal,
  }: {
    votedProposal: { vote: { vote: string }; proposal: { title?: string } };
  }) => (
    <p>
      {votedProposal.proposal.title} {votedProposal.vote.vote}
    </p>
  ),
}));

const proposal = {
  txHash: "ab".repeat(32),
  index: 0,
  type: "InfoAction",
  url: "https://example.org/ga.jsonld",
  metadataHash: "cd".repeat(32),
  title: null,
} as unknown as ProposalData;

describe("ValidatedGovernanceActionCard", () => {
  it("follows a change to inProgress after mounting, and keeps the resolved metadata", async () => {
    const { rerender } = render(
      <ValidatedGovernanceActionCard {...proposal} inProgress />,
    );
    await waitFor(() =>
      expect(screen.getByText("Title from metadata in-progress")).toBeVisible(),
    );

    rerender(<ValidatedGovernanceActionCard {...proposal} inProgress={false} />);

    expect(screen.getByText("Title from metadata idle")).toBeVisible();
  });
});

describe("ValidatedGovernanceVotedOnCard", () => {
  it("shows a changed vote after mounting", async () => {
    const voted = (vote: string) =>
      ({ proposal, vote: { vote } }) as unknown as Parameters<
        typeof ValidatedGovernanceVotedOnCard
      >[0]["votedProposal"];
    const { rerender } = render(
      <ValidatedGovernanceVotedOnCard votedProposal={voted("yes")} />,
    );
    await waitFor(() =>
      expect(screen.getByText("Title from metadata yes")).toBeVisible(),
    );

    rerender(<ValidatedGovernanceVotedOnCard votedProposal={voted("no")} />);

    expect(screen.getByText("Title from metadata no")).toBeVisible();
  });
});
