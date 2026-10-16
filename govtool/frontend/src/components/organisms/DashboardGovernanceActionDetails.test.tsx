import { render, screen, waitFor } from "@testing-library/react";
import { MemoryRouter, Route, Routes } from "react-router";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { ProposalData, ProposalVote } from "@/models";
import { DashboardGovernanceActionDetails } from "./DashboardGovernanceActionDetails";

const useGetProposalQuery = vi.fn();

vi.mock("@hooks", () => ({
  useGetProposalQuery: (id: string, enabled?: boolean) =>
    useGetProposalQuery(id, enabled),
  useGetVoterInfo: () => ({ voter: undefined }),
  useScreenDimension: () => ({ isMobile: false, screenWidth: 1400 }),
  useTranslation: () => ({ t: (key: string) => key }),
}));

vi.mock("@context", () => ({
  useCardano: () => ({ pendingTransaction: {}, isEnableLoading: false }),
}));

vi.mock("@/hooks/mutations", () => ({
  useValidateMutation: () => ({
    validateMetadata: vi.fn().mockResolvedValue({
      status: undefined,
      valid: true,
      issues: [],
      metadata: { title: "Checked title" },
    }),
  }),
}));

vi.mock("@molecules", () => ({ Breadcrumbs: () => null }));

// The card is out of scope: show what the page hands it.
vi.mock("@organisms", () => ({
  GovernanceActionDetailsCard: ({
    proposal,
    vote,
    isDocumentLoading,
  }: {
    proposal: ProposalData;
    vote?: ProposalVote;
    isDocumentLoading?: boolean;
  }) => (
    <p data-testid="card">
      {JSON.stringify({
        title: proposal.title,
        authors: proposal.authors?.map((author) => author.name),
        hasJson: proposal.json != null,
        vote: vote?.vote ?? null,
        isDocumentLoading: !!isDocumentLoading,
      })}
    </p>
  ),
}));

const TX = "ab".repeat(32);

const row = {
  txHash: TX,
  index: 0,
  type: "InfoAction",
  url: "https://example.org/ga.jsonld",
  metadataHash: "cd".repeat(32),
  title: null,
  json: null,
  authors: [],
} as unknown as ProposalData;

const myVote = { vote: "yes" } as unknown as ProposalVote;

const openWith = (proposal: ProposalData) =>
  render(
    <MemoryRouter
      initialEntries={[
        { pathname: `/ga/${TX}`, hash: "#0", state: { proposal, vote: myVote } },
      ]}
    >
      <Routes>
        <Route
          path="/ga/:proposalId"
          element={<DashboardGovernanceActionDetails />}
        />
      </Routes>
    </MemoryRouter>,
  );

const card = () => JSON.parse(screen.getByTestId("card").textContent ?? "{}");

describe("DashboardGovernanceActionDetails", () => {
  beforeEach(() => useGetProposalQuery.mockReset());

  it("reads the action again for a row that came without its document", async () => {
    useGetProposalQuery.mockReturnValue({
      data: {
        proposal: {
          ...row,
          title: "Backend title",
          json: { body: {} },
          authors: [{ name: "Alice" }],
        },
        vote: null,
      },
      isLoading: false,
      error: null,
    });

    openWith(row);

    expect(useGetProposalQuery).toHaveBeenCalledWith(`${TX}#0`, true);
    await waitFor(() =>
      expect(card()).toEqual({
        // The text the page checked itself, the document and authors read
        // again, and the row's own vote.
        title: "Checked title",
        authors: ["Alice"],
        hasJson: true,
        vote: "yes",
        isDocumentLoading: false,
      }),
    );
  });

  it("shows the row while its document is read, with the authors pending", () => {
    useGetProposalQuery.mockReturnValue({
      data: undefined,
      isLoading: true,
      error: null,
    });

    openWith(row);

    expect(card()).toMatchObject({ isDocumentLoading: true, vote: "yes" });
  });

  it("does not read again a row that carries its document", () => {
    useGetProposalQuery.mockReturnValue({
      data: undefined,
      isLoading: false,
      error: null,
    });

    openWith({ ...row, json: { body: {} }, authors: [{ name: "Bob" }] });

    expect(useGetProposalQuery).toHaveBeenCalledWith(`${TX}#0`, false);
    expect(card()).toMatchObject({ authors: ["Bob"], hasJson: true });
  });
});
