import { render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";
import { beforeEach, describe, expect, it, vi } from "vitest";

import "@/i18n";
import type { DRepData } from "@models";

import { DRepDetailsCardHeader } from "./DRepDetailsCardHeader";

let votingPower: number | undefined;

vi.mock("@context", () => ({
  useCardano: () => ({ stakeKey: "stake_test1" }),
}));

vi.mock("@hooks", async () => {
  const { useTranslation } = await import("react-i18next");
  return {
    useTranslation,
    useScreenDimension: () => ({ screenWidth: 1440 }),
    useGetAdaHolderVotingPowerQuery: () => ({ votingPower }),
  };
});

// The atoms and molecules barrels pull in every context provider; the header
// needs only a button and the title.
vi.mock("@atoms", async () => ({
  Button: (await import("../atoms/Button")).Button,
}));

vi.mock("@molecules", () => ({
  DataMissingHeader: ({ title }: { title?: string }) => <h1>{title}</h1>,
}));

const dRep = { givenName: "Alice", image: null } as unknown as DRepData;

const renderHeader = (props: { isMe?: boolean; isMyDrep?: boolean }) =>
  render(
    <MemoryRouter>
      <DRepDetailsCardHeader dRepData={dRep} {...props} />
    </MemoryRouter>,
  );

describe("DRepDetailsCardHeader", () => {
  beforeEach(() => {
    votingPower = 1_234_567_000_000;
  });

  it("shows how much the user delegated to this DRep", () => {
    renderHeader({ isMyDrep: true });

    expect(screen.getByTestId("my-delegation-chip")).toHaveTextContent(
      "You have delegated ₳ 1,234,567 to this DRep",
    );
  });

  it("shows nothing for a DRep the user has not delegated to", () => {
    renderHeader({});

    expect(screen.queryByTestId("my-delegation-chip")).toBeNull();
  });

  it("shows nothing on the user's own DRep page", () => {
    renderHeader({ isMe: true, isMyDrep: true });

    expect(screen.queryByTestId("my-delegation-chip")).toBeNull();
    expect(screen.getByTestId("edit-drep-data-button")).toBeInTheDocument();
  });

  it("waits for the voting power before showing the amount", () => {
    votingPower = undefined;
    renderHeader({ isMyDrep: true });

    expect(screen.queryByTestId("my-delegation-chip")).toBeNull();
  });
});
