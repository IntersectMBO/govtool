import { act, fireEvent, render, screen } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import "@/i18n";
import type { MetadataRefreshOutcome } from "@models";

import { MetadataRetryButton } from "./MetadataRetryButton";

const retry = vi.fn<() => Promise<MetadataRefreshOutcome>>();

vi.mock("@/hooks/mutations", () => ({
  useMetadataRetryMutation: () => ({ retry, isRetrying: false }),
}));

// The atoms barrel pulls in the modal atoms and, through them, every context
// provider; the plain atoms are all this component needs.
vi.mock("@atoms", async () => ({
  Button: (await import("../atoms/Button")).Button,
  Typography: (await import("../atoms/Typography")).Typography,
}));

vi.mock("@hooks", async () => {
  const { useTranslation } = await import("react-i18next");
  const { useCountdown } = await import("@/hooks/useCountdown");
  return { useTranslation, useCountdown };
});

const failure = {
  ok: false as const,
  code: "FETCH_ERROR" as const,
  category: "NETWORK" as const,
  message: "connect ECONNREFUSED",
  reportId: "r1",
  checkedAt: "2026-09-24T00:00:00Z",
};

const anchor = { url: "https://example.com/a.json", hash: "ab".repeat(32) };

// Resolve the click's promise chain while fake timers hold the clock.
const click = async () => {
  await act(async () => {
    fireEvent.click(screen.getByTestId("metadata-retry-button"));
  });
};

describe("MetadataRetryButton", () => {
  beforeEach(() => {
    vi.useFakeTimers();
    retry.mockReset();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it("disables itself and counts down live when the window has not elapsed", async () => {
    retry.mockResolvedValue({
      refetched: false,
      retryAfterSeconds: 3,
      result: failure,
    });
    const onOutcome = vi.fn();
    render(<MetadataRetryButton anchor={anchor} onOutcome={onOutcome} />);

    await click();

    const button = screen.getByTestId("metadata-retry-button");
    expect(button).toBeDisabled();
    expect(screen.getByTestId("metadata-retry-countdown")).toHaveTextContent(
      "You can retry again in 3 seconds",
    );
    expect(onOutcome).toHaveBeenCalledWith(
      expect.objectContaining({ retryAfterSeconds: 3 }),
    );

    act(() => vi.advanceTimersByTime(1000));
    expect(screen.getByTestId("metadata-retry-countdown")).toHaveTextContent(
      "You can retry again in 2 seconds",
    );

    act(() => vi.advanceTimersByTime(1000));
    expect(screen.getByTestId("metadata-retry-countdown")).toHaveTextContent(
      "You can retry again in 1 second",
    );

    act(() => vi.advanceTimersByTime(1000));
    expect(
      screen.queryByTestId("metadata-retry-countdown"),
    ).not.toBeInTheDocument();
    expect(screen.getByTestId("metadata-retry-button")).toBeEnabled();
  });

  it("stays enabled after a real refetch and passes the outcome on", async () => {
    const outcome: MetadataRefreshOutcome = {
      refetched: true,
      result: { ...failure, reportId: "r2" },
    };
    retry.mockResolvedValue(outcome);
    const onOutcome = vi.fn();
    render(<MetadataRetryButton anchor={anchor} onOutcome={onOutcome} />);

    await click();

    expect(screen.getByTestId("metadata-retry-button")).toBeEnabled();
    expect(
      screen.queryByTestId("metadata-retry-countdown"),
    ).not.toBeInTheDocument();
    expect(onOutcome).toHaveBeenCalledWith(outcome);
  });

  it("shows an error when the retry request fails", async () => {
    retry.mockRejectedValue(new Error("/metadata/retry responded 503"));
    const onOutcome = vi.fn();
    render(<MetadataRetryButton anchor={anchor} onOutcome={onOutcome} />);

    await click();

    expect(
      screen.getByText("The retry could not be sent. Please try again later."),
    ).toBeInTheDocument();
    expect(onOutcome).not.toHaveBeenCalled();
    expect(screen.getByTestId("metadata-retry-button")).toBeEnabled();
  });
});
