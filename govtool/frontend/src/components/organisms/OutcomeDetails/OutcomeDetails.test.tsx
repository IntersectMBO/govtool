import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import { MemoryRouter, useLocation } from "react-router";
import { describe, expect, it, vi } from "vitest";

import "@/i18n";

import type { GovernanceActionRecord } from "@/models/governanceActions";
import { getGovernanceActionCIP129Id } from "@/utils/governanceActions";

import { OutcomeDetails } from "./OutcomeDetails";

vi.mock("@/context/wallet", () => ({ useCardano: () => ({ isEnabled: false }) }));
vi.mock("@/context/snackbar", () => ({ useSnackbar: () => ({ addErrorAlert: vi.fn(), addSuccessAlert: vi.fn() }) }));
vi.mock("@/context/modal", () => ({ useModal: () => ({ openModal: vi.fn() }) }));
vi.mock("@/context/featureFlag", () => ({
  useFeatureFlag: () => ({ isProposalDiscussionForumEnabled: true }),
}));
vi.mock("@/components/organisms/DRepDashboard/ShareDRepDialog", () => ({
  ShareDialog: () => null,
}));
vi.mock("@/components/molecules/GovernanceActionAuthors", () => ({
  GovernanceActionAuthors: () => null,
}));

const NONE = {
  ratified_epoch: null,
  enacted_epoch: null,
  dropped_epoch: null,
  expired_epoch: null,
};
const NO_TIMES = {
  ratified_time: null,
  enacted_time: null,
  dropped_time: null,
  expired_time: null,
};

const action = {
  id: "1",
  tx_hash: "ab".repeat(32),
  index: 1,
  type: "TreasuryWithdrawals",
  description: null,
  expiry_date: "2026-09-02T00:00:00Z",
  expiration: 653,
  time: "2026-07-31T10:00:00Z",
  epoch_no: 646,
  url: "",
  data_hash: "",
  proposal_params: null,
  json_metadata: null,
  status: { ...NONE, expired_epoch: 653 },
  status_times: { ...NO_TIMES, expired_time: "2026-09-02T00:00:00Z" },
  vote_aggregates: [
    {
      role: "drep",
      representation: "count",
      yes: "75",
      no: "2",
      abstain: "10",
      notVoted: "0",
      totalEligible: "87",
      threshold: { numerator: 2, denominator: 3 },
    },
  ],
  prev_gov_action_index: null,
  prev_gov_action_tx_hash: null,
} as unknown as GovernanceActionRecord;

const Where = () => {
  const { search, hash } = useLocation();
  return <output data-testid="where">{`${search}${hash}`}</output>;
};

const renderAt = (path: string) =>
  render(
    <MemoryRouter initialEntries={[path]}>
      <OutcomeDetails
        action={action}
        isMetadataLoading={false}
        isProposalLoading={false}
        references={[]}
        title="Withdraw 120,000,000 ada"
      />
      <Where />
    </MemoryRouter>,
  );

describe("OutcomeDetails", () => {
  it("keeps the action's #index in the URL when the tab changes", () => {
    renderAt(`/governance_actions/history/${"ab".repeat(32)}#1`);
    fireEvent.click(screen.getByTestId("outcome-tab-votes"));
    expect(screen.getByTestId("where")).toHaveTextContent("?tab=votes#1");
    fireEvent.click(screen.getByTestId("outcome-tab-overview"));
    expect(screen.getByTestId("where")).toHaveTextContent(/^#1$/);
  });

  it("shows each treasury withdrawal's amount beside its address", () => {
    render(
      <MemoryRouter initialEntries={["/governance_actions/history/x#1"]}>
        <OutcomeDetails
          action={{ ...action, description: [{ receivingAddress: "stake1a", amount: 120000000000000 }] } as unknown as GovernanceActionRecord}
          isMetadataLoading={false}
          isProposalLoading={false}
          references={[]}
        />
      </MemoryRouter>,
    );
    expect(screen.getByTestId("outcome-withdrawal-0-amount")).toHaveTextContent("₳ 120,000,000");
  });

  it("copies either id or the receiving address from the More menu", async () => {
    const writeText = vi.fn().mockResolvedValue(undefined);
    Object.assign(navigator, { clipboard: { writeText } });
    const address = "stake1uyl49lk2c6vfcvjq9zy0r640rf73k0q";
    const description = [{ receivingAddress: address, amount: 5 }];
    render(
      <MemoryRouter initialEntries={["/governance_actions/history/x#1"]}>
        <OutcomeDetails
          action={{ ...action, description } as unknown as GovernanceActionRecord}
          isMetadataLoading={false}
          isProposalLoading={false}
          references={[]}
        />
      </MemoryRouter>,
    );
    const open = () => fireEvent.click(screen.getByTestId("outcome-more-button"));

    open();
    fireEvent.click(screen.getByRole("menuitem", { name: "Copy governance action ID" }));
    await waitFor(() => expect(writeText).toHaveBeenLastCalledWith(`${"ab".repeat(32)}#1`));

    open();
    fireEvent.click(screen.getByRole("menuitem", { name: "Copy CIP-129 governance action ID" }));
    await waitFor(() =>
      expect(writeText).toHaveBeenLastCalledWith(getGovernanceActionCIP129Id(action)));

    open();
    fireEvent.click(screen.getByRole("menuitem", { name: "Copy receiving address" }));
    await waitFor(() => expect(writeText).toHaveBeenLastCalledWith(address));
  });

  it("links a visitor without a wallet to the action's forum proposal", () => {
    render(
      <MemoryRouter initialEntries={["/governance_actions/history/x?tab=discussions#1"]}>
        <OutcomeDetails
          action={action}
          isMetadataLoading={false}
          isProposalLoading={false}
          proposal={{ id: 7, attributes: { prop_comments_number: 2 } }}
          references={[]}
        />
      </MemoryRouter>,
    );
    expect(screen.getByTestId("proposal-7-view-details")).toHaveAttribute(
      "href",
      "/proposal_discussion/7",
    );
  });

  it("keeps the abstract's single line breaks and renders its maths", () => {
    render(
      <MemoryRouter initialEntries={["/governance_actions/history/x#1"]}>
        <OutcomeDetails
          abstract={"First line\nSecond line with $a^2$"}
          action={action}
          isMetadataLoading={false}
          isProposalLoading={false}
          references={[]}
        />
      </MemoryRouter>,
    );
    const abstract = screen.getByTestId("governance-action-abstract");
    expect(abstract.querySelector("br")).not.toBeNull();
    expect(abstract.querySelector(".katex")).not.toBeNull();
  });

  it("dates the header by the status its pill shows", () => {
    render(
      <MemoryRouter initialEntries={["/governance_actions/history/x#1"]}>
        <OutcomeDetails
          action={{
            ...action,
            status: { ...NONE, expired_epoch: 656, dropped_epoch: 657 },
            status_times: { ...NO_TIMES, expired_time: "2026-09-17T00:00:00Z", dropped_time: "2026-09-22T00:00:00Z" },
          }}
          isMetadataLoading={false}
          isProposalLoading={false}
          references={[]}
        />
      </MemoryRouter>,
    );
    const header = screen.getByTestId("single-action-header");
    expect(header).toHaveTextContent("Expired on");
    expect(header).not.toHaveTextContent("Dropped on");
  });

  it("words a DRep count tally as yes and no, not as the committee's", () => {
    renderAt(`/governance_actions/history/${"ab".repeat(32)}?tab=votes#1`);
    const breakdown = screen.getByTestId("outcome-breakdown");
    expect(breakdown).not.toHaveTextContent("Constitutional:");
    expect(breakdown).toHaveTextContent("Yes Power:");
  });
});
