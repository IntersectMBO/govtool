import axios from "axios";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { getGovernanceActionProposalDiscussion } from "./governanceActions";

vi.mock("@/config/env", () => ({
  env: {
    VITE_BASE_URL: "https://govtool.example/api",
    VITE_PDF_API_URL: "https://pdf.example/",
  },
}));

const txHash = "AB".repeat(32);

describe("governance action proposal discussion", () => {
  beforeEach(() => {
    vi.restoreAllMocks();
  });

  it("asks the pdf API by submission tx hash and answers its first item", async () => {
    const proposal = { id: 7, attributes: {} };
    const get = vi
      .spyOn(axios, "get")
      .mockResolvedValue({ data: { data: [proposal], meta: {} } });

    await expect(getGovernanceActionProposalDiscussion(txHash)).resolves.toBe(
      proposal,
    );
    expect(get).toHaveBeenCalledWith("/api/proposals", {
      baseURL: "https://pdf.example",
      params: {
        "filters[prop_submission_tx_hash][$eq]": txHash.toLowerCase(),
        "pagination[page]": 1,
        "pagination[pageSize]": 1,
      },
      timeout: 30_000,
    });
  });

  it("answers null when no proposal was discussed", async () => {
    vi.spyOn(axios, "get").mockResolvedValue({ data: { data: [] } });

    await expect(
      getGovernanceActionProposalDiscussion(txHash),
    ).resolves.toBeNull();
  });
});
