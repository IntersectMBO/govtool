import { render, screen, fireEvent } from "@testing-library/react";
import { describe, it, expect, vi } from "vitest";
import * as Hooks from "@hooks";
import * as Context from "@context";
import * as Utils from "@utils";
import { VotingPowerChips } from "@atoms";

describe("VotingPowerChips", () => {
  const mockUseCardano = vi.spyOn(Context, "useCardano");
  const mockUseGetDRepVotingPowerQuery = vi.spyOn(
    Hooks,
    "useGetDRepVotingPowerQuery",
  );
  const mockUseScreenDimension = vi.spyOn(Hooks, "useScreenDimension");
  const mockCorrectAdaFormat = vi.spyOn(Utils, "correctAdaFormat");
  const mockUseTranslation = vi.spyOn(Hooks, "useTranslation");
  const mockUseGetVoterInfo = vi.spyOn(Hooks, "useGetVoterInfo");

  it("renders loading spinner when data is loading", () => {
    mockUseCardano.mockReturnValue({
      isEnableLoading: 'demos',
    } as ReturnType<typeof Context.useCardano>);
    mockUseGetDRepVotingPowerQuery.mockReturnValue(
      {} as ReturnType<typeof Hooks.useGetDRepVotingPowerQuery>,
    );
    mockUseScreenDimension.mockReturnValue({
      isMobile: false,
      screenWidth: 1024,
    } as ReturnType<typeof Hooks.useScreenDimension>);
    mockUseTranslation.mockReturnValue({
      t: (key: string) => key,
    } as ReturnType<typeof Hooks.useTranslation>);
    mockUseGetVoterInfo.mockReturnValue(
      { voter: { isRegisteredAsDRep: true } } as ReturnType<typeof Hooks.useGetVoterInfo>,
    );

    render(<VotingPowerChips />);
    expect(screen.getByRole("progressbar")).toBeInTheDocument();
  });

  it("displays formatted ADA amount when data is available and not loading", () => {
    mockUseCardano.mockReturnValue({
      isEnableLoading: null,
    } as ReturnType<typeof Context.useCardano>);
    mockUseGetDRepVotingPowerQuery.mockReturnValue({
      dRepVotingPower: 1000,
    } as ReturnType<typeof Hooks.useGetDRepVotingPowerQuery>);
    mockUseScreenDimension.mockReturnValue({
      isMobile: false,
      screenWidth: 1024,
    } as ReturnType<typeof Hooks.useScreenDimension>);
    mockUseTranslation.mockReturnValue({
      t: (key: string) => key,
    } as ReturnType<typeof Hooks.useTranslation>);
    mockUseGetVoterInfo.mockReturnValue({
      voter: { isRegisteredAsDRep: true },
    } as ReturnType<typeof Hooks.useGetVoterInfo>);
    mockCorrectAdaFormat.mockReturnValue(1000);

    render(<VotingPowerChips />);
    expect(screen.getByText(/₳ 1000/)).toBeInTheDocument();
  });

  it("displays the tooltip correctly for non-mobile DRep registered users", async () => {
    mockUseCardano.mockReturnValue({
      isEnableLoading: null,
    } as ReturnType<typeof Context.useCardano>);
    mockUseGetDRepVotingPowerQuery.mockReturnValue({
      dRepVotingPower: 1000,
    } as ReturnType<typeof Hooks.useGetDRepVotingPowerQuery>);
    mockUseScreenDimension.mockReturnValue({
      isMobile: false,
      screenWidth: 800,
    } as ReturnType<typeof Hooks.useScreenDimension>);
    mockUseTranslation.mockReturnValue({
      t: (key: string) => key,
    } as ReturnType<typeof Hooks.useTranslation>);
    mockUseGetVoterInfo.mockReturnValue({
      voter: { isRegisteredAsDRep: true },
    } as ReturnType<typeof Hooks.useGetVoterInfo>);
    mockCorrectAdaFormat.mockReturnValue(1000);

    render(<VotingPowerChips />);

    const icon = screen.getByTestId("InfoOutlinedIcon");
    fireEvent.mouseOver(icon);

    const tooltip = await screen.findByText(
      "tooltips.votingPower.heading",
      {},
      { timeout: 500 },
    );
    expect(tooltip).toBeInTheDocument();
  });
});
