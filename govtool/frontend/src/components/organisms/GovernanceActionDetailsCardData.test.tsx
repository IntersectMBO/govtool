import { render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";
import { describe, expect, it, vi } from "vitest";

import { ProposalData } from "@/models";
import { GovernanceActionDetailsCardData } from "./GovernanceActionDetailsCardData";

vi.mock("@/context", async (importOriginal) => ({
  ...(await importOriginal<typeof import("@/context")>()),
  useAppContext: () => ({ epochParams: {} }),
}));

const proposal = {
  txHash: "ab".repeat(32),
  index: 0,
  type: "InfoAction",
  url: "https://example.org/ga.jsonld",
  metadataHash: "cd".repeat(32),
  title: "A title",
  createdDate: "2026-10-01T00:00:00Z",
  createdEpochNo: 500,
  expiryDate: "2026-11-01T00:00:00Z",
  expiryEpochNo: 506,
  protocolParams: null,
  details: null,
} as unknown as ProposalData;

const authorsLine = (overrides: Partial<ProposalData>) => {
  render(
    <MemoryRouter>
      <GovernanceActionDetailsCardData
        isOneColumn
        proposal={{ ...proposal, ...overrides }}
      />
    </MemoryRouter>,
  );
  return screen.getByTestId("authors");
};

describe("GovernanceActionDetailsCardData authors", () => {
  it("says the authors are unknown when the document did not load", () => {
    expect(authorsLine({ json: undefined, authors: [] })).toHaveTextContent(
      "Unknown: the document could not be loaded",
    );
  });

  it("says there are none only when the loaded document lists none", () => {
    expect(
      authorsLine({ json: { body: {} }, authors: [] }),
    ).toHaveTextContent("No data available");
  });

  it("lists the authors of a loaded document", () => {
    expect(
      authorsLine({
        json: { body: {} },
        authors: [
          {
            name: "Alice",
            publicKey: "c".repeat(64),
            signature: "d".repeat(128),
            witnessAlgorithm: "ed25519",
          },
        ],
      }),
    ).toHaveTextContent("Alice");
  });
});
