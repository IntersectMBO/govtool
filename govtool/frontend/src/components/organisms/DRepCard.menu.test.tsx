import { fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import { MemoryRouter } from "react-router";
import { useTranslation } from "react-i18next";
import { beforeEach, describe, expect, it, vi } from "vitest";

// Loads consts before the theme, as DRepCard.test.ts does, to break their cycle.
import "@consts";
import "@/i18n";

import { DRepStatus, type DRepData } from "@/models";

import { DRepCard } from "./DRepCard";

const stats = vi.hoisted(() => ({
  value: {
    participation: 94 as number | null,
    isParticipationLoading: false,
    delegators: 920 as number | undefined,
    areDelegatorsServed: true,
    areDelegatorsLoading: false,
  },
}));
const alerts = vi.hoisted(() => ({ success: vi.fn(), error: vi.fn(), openModal: vi.fn() }));

vi.mock("@hooks", () => ({ useTranslation }));
vi.mock("@context", () => ({
  useModal: () => ({ openModal: alerts.openModal }),
  useSnackbar: () => ({ addErrorAlert: alerts.error, addSuccessAlert: alerts.success }),
}));
vi.mock("@/hooks/mutations", () => ({
  useValidateMutation: () => ({ validateMetadata: vi.fn() }),
}));
vi.mock("@/hooks/queries/useDRepDashboard", () => ({
  useDRepCardStats: () => stats.value,
}));

const VIEW = "drep1kyppjlhz4lawh4g0ewx2d8a5l20t4yclfnppnuvdkmt7vccg836";

const dRep = {
  drepId: "b102197ee2affaebd50fcb8ca69fb4fa9eba931f4cc219f18db6d7e6",
  view: VIEW,
  isScriptBased: false,
  status: DRepStatus.Active,
  type: "DRep",
  votingPower: 8100000000000,
  givenName: "Alex Rivera",
  latestRegistrationDate: "2025-01-15T00:00:00Z",
  image: null,
  objectives: "Advocating for developer grants.",
} as unknown as DRepData;

const renderCard = (isConnected = false) =>
  render(
    <MemoryRouter>
      <DRepCard dRep={dRep} isConnected={isConnected} onDelegate={vi.fn()} />
    </MemoryRouter>,
  );

describe("DRepCard", () => {
  beforeEach(() => {
    stats.value = { ...stats.value, delegators: 920, areDelegatorsServed: true };
  });

  it("shows voting power, participation and delegators", () => {
    renderCard();
    expect(screen.getByTestId(`${VIEW}-voting-power`)).toHaveTextContent("8,100,000");
    expect(screen.getByTestId(`${VIEW}-participation`)).toHaveTextContent("94%");
    expect(screen.getByTestId(`${VIEW}-delegators`)).toHaveTextContent("920");
  });

  it("keeps the registration date while delegators are not served", () => {
    stats.value = { ...stats.value, delegators: undefined, areDelegatorsServed: false };
    renderCard();
    expect(screen.queryByTestId(`${VIEW}-delegators`)).toBeNull();
    expect(screen.getByTestId(`${VIEW}-registered`)).toHaveTextContent("Jan 15, 2025");
  });

  it("offers delegation, the profile, its votes and both ids in the menu", async () => {
    const writeText = vi.fn().mockResolvedValue(undefined);
    Object.assign(navigator, { clipboard: { writeText } });
    renderCard();
    fireEvent.click(screen.getByTestId(`${VIEW}-menu-button`));
    const menu = within(screen.getByRole("menu"));
    expect(menu.getAllByRole("menuitem").map((item) => item.textContent)).toEqual([
      "Delegate Voting Power",
      "View Profile",
      "View Voting History",
      "Copy DRep ID",
      "Copy Legacy DRep ID (CIP-105)",
    ]);
    expect(menu.getByText("View Voting History").closest("a")).toHaveAttribute(
      "href",
      `/drep_directory/${VIEW}#voting-history`,
    );
    fireEvent.click(menu.getByText("Copy Legacy DRep ID (CIP-105)"));
    await waitFor(() => expect(writeText).toHaveBeenCalledWith(VIEW));
    expect(alerts.success).toHaveBeenCalled();
  });

  it("asks a visitor to connect a wallet when delegating from the menu", () => {
    renderCard(false);
    fireEvent.click(screen.getByTestId(`${VIEW}-menu-button`));
    fireEvent.click(screen.getByText("Delegate Voting Power"));
    expect(alerts.openModal).toHaveBeenCalledWith(expect.objectContaining({ type: "chooseWallet" }));
  });
});
