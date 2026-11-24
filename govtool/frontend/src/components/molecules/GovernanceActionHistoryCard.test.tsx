import { describe, expect, it, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import "@/i18n";
import type { GovernanceActionRecord } from "@/models";

import { GovernanceActionHistoryCard } from "./GovernanceActionHistoryCard";

vi.mock("@hooks", async () => ({
  useTranslation: (await import("react-i18next")).useTranslation,
  useGetGovernanceActionMetadata: () => ({
    metadata: { data: { title: "An action" } },
    metadataValid: true,
    isMetadataLoading: false,
  }),
}));

const action = (over: Partial<GovernanceActionRecord>) =>
  ({
    id: "1",
    tx_hash: "ab".repeat(32),
    index: 0,
    type: "TreasuryWithdrawals",
    time: "2026-07-01T00:00:00Z",
    epoch_no: 640,
    expiry_date: "2026-08-01T00:00:00Z",
    expiration: 646,
    status: {
      ratified_epoch: 644,
      enacted_epoch: 645,
      dropped_epoch: null,
      expired_epoch: null,
    },
    status_times: {
      ratified_time: "2026-07-20T00:00:00Z",
      enacted_time: "2026-07-25T00:00:00Z",
      dropped_time: null,
      expired_time: null,
    },
    vote_aggregates: [
      {
        role: "drep",
        representation: "stake",
        yes: "70",
        no: "10",
        abstain: "0",
        notVoted: "20",
        totalEligible: "100",
        threshold: { numerator: 67, denominator: 100 },
      },
    ],
    ...over,
  }) as unknown as GovernanceActionRecord;

const renderCard = (record: GovernanceActionRecord) =>
  render(
    <MemoryRouter>
      <GovernanceActionHistoryCard action={record} />
    </MemoryRouter>,
  );

describe("GovernanceActionHistoryCard", () => {
  it("labels the ratification date of an enacted action as ratified", () => {
    renderCard(action({}));
    expect(screen.getByText("Ratified")).toBeInTheDocument();
    expect(screen.getByText("Jul 20, 2026 · Epoch 644")).toBeInTheDocument();
    expect(screen.getByText("Enacted")).toBeInTheDocument();
  });

  it("gives info action tallies no pass or fail colour", () => {
    renderCard(action({ type: "InfoAction" }));
    const square = screen.getByTestId(/-drep-tally$/).firstElementChild;
    expect(square).toHaveStyle({ backgroundColor: "#98A2B3" });
  });
});
