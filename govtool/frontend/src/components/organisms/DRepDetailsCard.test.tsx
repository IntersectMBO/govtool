import { render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";
import { useTranslation } from "react-i18next";
import { beforeEach, describe, expect, it, vi } from "vitest";

// Loads consts before the theme, as DRepCard.test.ts does, to break their cycle.
import "@consts";
import "@/i18n";

import { DRepStatus, type DRepData } from "@/models";

import { DRepDetailsCard } from "./DRepDetailsCard";

const mocks = vi.hoisted(() => ({
  votingPower: undefined as number | undefined,
}));

vi.mock("@hooks", () => ({
  useTranslation,
  useDelegateTodRep: () => ({ delegate: vi.fn(), isDelegating: false }),
  useGetAdaHolderVotingPowerQuery: () => ({ votingPower: mocks.votingPower }),
  useGetDRepVotesByIdQuery: () => ({
    dRepVotes: [],
    areDRepVotesLoading: false,
    isDRepVotesError: false,
  }),
}));

vi.mock("@context", () => ({
  useCardano: () => ({ pendingTransaction: {}, stakeKey: "stake_test1" }),
  useModal: () => ({ openModal: vi.fn() }),
  useSnackbar: () => ({ addErrorAlert: vi.fn(), addSuccessAlert: vi.fn() }),
}));

vi.mock("@/hooks/mutations", () => ({
  useValidateMutation: () => ({ validateMetadata: vi.fn() }),
}));

// The votes table, the delegation review and the share dialog read through
// React Query or the wallet; they have their own tests.
vi.mock("./DRepActivityTable", () => ({ DRepActivityTable: () => null }));
vi.mock("./DelegationReviewDialog", () => ({
  DelegationReviewDialog: () => null,
}));
vi.mock("./DRepDashboard/ShareDRepDialog", () => ({
  ShareDRepDialog: () => null,
}));

const dRep = {
  drepId: "b102197ee2affaebd50fcb8ca69fb4fa9eba931f4cc219f18db6d7e6",
  view: "drep1kyppjlhz4lawh4g0ewx2d8a5l20t4yclfnppnuvdkmt7vccg836",
  isScriptBased: false,
  status: DRepStatus.Active,
  type: "DRep",
  votingPower: 8100000000000,
  givenName: "Alice",
  image: null,
} as unknown as DRepData;

const renderCard = (props: { isMe?: boolean; isMyDrep?: boolean }) =>
  render(
    <MemoryRouter>
      <DRepDetailsCard dRepData={dRep} isConnected {...props} />
    </MemoryRouter>,
  );

describe("DRepDetailsCard delegation banner", () => {
  beforeEach(() => {
    mocks.votingPower = 1_234_567_000_000;
  });

  it("shows how much the user delegated to this DRep", () => {
    renderCard({ isMyDrep: true });

    expect(screen.getByTestId("my-delegation-amount")).toHaveTextContent(
      "You have delegated ₳ 1,234,567 to Alice.",
    );
  });

  it("shows nothing for a DRep the user has not delegated to", () => {
    renderCard({});

    expect(screen.queryByTestId("delegated-banner")).toBeNull();
    expect(screen.queryByTestId("my-delegation-amount")).toBeNull();
  });

  it("leaves the amount out on the user's own DRep page", () => {
    renderCard({ isMe: true, isMyDrep: true });

    expect(screen.getByTestId("delegated-banner")).toBeInTheDocument();
    expect(screen.queryByTestId("my-delegation-amount")).toBeNull();
  });

  it("waits for the voting power before showing the amount", () => {
    mocks.votingPower = undefined;
    renderCard({ isMyDrep: true });

    expect(screen.getByTestId("delegated-banner")).toHaveTextContent(
      "Your voting power is currently delegated to Alice.",
    );
    expect(screen.queryByTestId("my-delegation-amount")).toBeNull();
  });
});
