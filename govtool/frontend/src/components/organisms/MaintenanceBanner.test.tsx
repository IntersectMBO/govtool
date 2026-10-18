import { act, render, screen } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import "@/i18n";

import { MaintenanceBanner } from "./MaintenanceBanner";

// The hooks barrel pulls in every context provider; the countdown is all this
// component needs.
vi.mock("@hooks", async () => ({
  useCountdown: (await import("@/hooks/useCountdown")).useCountdown,
}));

const NOW = Date.parse("2026-10-16T12:00:00Z");
const minutes = (count: number) => count * 60_000;

const timer = () => screen.getByTestId("system-maintenance-timer");

describe("MaintenanceBanner", () => {
  beforeEach(() => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it("counts down to the end of maintenance", () => {
    const errors = vi.spyOn(console, "error");
    render(<MaintenanceBanner startsAt={NOW} endsAt={NOW + minutes(60)} />);

    expect(
      screen.getByText("GovTool is under maintenance"),
    ).toBeInTheDocument();
    expect(screen.getByText(/about 1 hour/)).toBeInTheDocument();
    expect(timer()).toHaveTextContent("Back in01h00m00s");

    act(() => {
      vi.advanceTimersByTime(61_000);
    });

    expect(timer()).toHaveTextContent("Back in58m59s");
    expect(errors).not.toHaveBeenCalled();
    errors.mockRestore();
  });

  it("estimates the time left for a visitor arriving late", () => {
    render(
      <MaintenanceBanner
        startsAt={NOW - minutes(37)}
        endsAt={NOW + minutes(23)}
      />,
    );

    expect(screen.getByText(/about 25 minutes/)).toBeInTheDocument();
    expect(timer()).toHaveTextContent("Back in23m00s");
  });

  it("says it will be back soon in the last five minutes", () => {
    render(<MaintenanceBanner endsAt={NOW + minutes(10)} />);

    expect(screen.queryByTestId("system-maintenance-dots")).toBeNull();

    act(() => {
      vi.advanceTimersByTime(minutes(5));
    });

    expect(
      screen.getByText("Almost there, we'll be back soon"),
    ).toBeInTheDocument();
    expect(screen.getByTestId("system-maintenance-dots")).toBeInTheDocument();
    expect(screen.queryByText(/about 5 minutes/)).toBeNull();
    expect(timer()).toHaveTextContent("Back in05m00s");
  });

  it("says it is still under maintenance once the end passes", () => {
    render(<MaintenanceBanner endsAt={NOW + minutes(1)} />);

    act(() => {
      vi.advanceTimersByTime(61_000);
    });

    expect(
      screen.getByText("GovTool is still under maintenance"),
    ).toBeInTheDocument();
    expect(screen.getByText(/We'll be back soon\./)).toBeInTheDocument();
    expect(
      screen.getByTestId("system-maintenance-finishing"),
    ).toHaveTextContent("Finishing up");
    expect(timer()).not.toHaveTextContent(/Back in/i);
  });

  it("shows maintenance that overran as still running", () => {
    render(<MaintenanceBanner endsAt={NOW - minutes(3)} />);

    expect(
      screen.getByText("GovTool is still under maintenance"),
    ).toBeInTheDocument();
    expect(screen.getByTestId("system-maintenance-finishing")).toBeVisible();
  });

  it("has no countdown without an end time", () => {
    render(<MaintenanceBanner />);

    expect(
      screen.getByText("GovTool is under maintenance"),
    ).toBeInTheDocument();
    expect(screen.getByText(/We'll be back soon\./)).toBeInTheDocument();
    expect(screen.queryByTestId("system-maintenance-timer")).toBeNull();
  });

  it("follows a notice whose end time moves", () => {
    const { rerender } = render(
      <MaintenanceBanner endsAt={NOW + minutes(10)} />,
    );

    rerender(<MaintenanceBanner endsAt={NOW + minutes(40)} />);

    expect(timer()).toHaveTextContent("Back in40m00s");
  });
});
