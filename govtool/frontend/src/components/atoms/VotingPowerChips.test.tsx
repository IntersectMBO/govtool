import { render, screen, fireEvent } from "@testing-library/react";
import { describe, it, expect, vi } from "vitest";
import * as Hooks from "@hooks";
import * as Utils from "@utils";
import { VotingPowerChips } from "@atoms";

describe("VotingPowerChips", () => {
  const mockUseScreenDimension = vi.spyOn(Hooks, "useScreenDimension");
  const mockCorrectAdaFormat = vi.spyOn(Utils, "correctAdaFormat");
  const mockUseTranslation = vi.spyOn(Hooks, "useTranslation");

  it("renders loading spinner when data is loading", () => {
    mockUseScreenDimension.mockReturnValue({
      isMobile: false,
      screenWidth: 1024,
    } as ReturnType<typeof Hooks.useScreenDimension>);
    mockUseTranslation.mockReturnValue({
      t: (key: string) => key,
    } as ReturnType<typeof Hooks.useTranslation>);

    render(<VotingPowerChips isLoading isShown />);
    expect(screen.getByRole("progressbar")).toBeInTheDocument();
  });

  it("displays formatted Ada amount when data is available and not loading", () => {
    mockUseScreenDimension.mockReturnValue({
      isMobile: false,
      screenWidth: 1024,
    } as ReturnType<typeof Hooks.useScreenDimension>);
    mockUseTranslation.mockReturnValue({
      t: (key: string) => key,
    } as ReturnType<typeof Hooks.useTranslation>);
    mockCorrectAdaFormat.mockReturnValue(1000);

    render(<VotingPowerChips isShown />);
    expect(screen.getByText(/₳ 1000/)).toBeInTheDocument();
  });

  it("displays the tooltip correctly for non-mobile DRep registered users", async () => {
    mockUseScreenDimension.mockReturnValue({
      isMobile: false,
      screenWidth: 800,
    } as ReturnType<typeof Hooks.useScreenDimension>);
    mockUseTranslation.mockReturnValue({
      t: (key: string) => key,
    } as ReturnType<typeof Hooks.useTranslation>);

    mockCorrectAdaFormat.mockReturnValue(1000);

    render(<VotingPowerChips isShown />);

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
