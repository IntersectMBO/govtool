import { act, renderHook } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { useCountdown } from "./useCountdown";

describe("useCountdown", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it("is idle until started", () => {
    const { result } = renderHook(() => useCountdown());
    expect(result.current.remaining).toBe(0);
    expect(result.current.isRunning).toBe(false);
  });

  it("counts down once a second and stops at zero", () => {
    const { result } = renderHook(() => useCountdown());

    act(() => result.current.start(3));
    expect(result.current.remaining).toBe(3);
    expect(result.current.isRunning).toBe(true);

    act(() => vi.advanceTimersByTime(1000));
    expect(result.current.remaining).toBe(2);

    act(() => vi.advanceTimersByTime(1000));
    expect(result.current.remaining).toBe(1);

    act(() => vi.advanceTimersByTime(1000));
    expect(result.current.remaining).toBe(0);
    expect(result.current.isRunning).toBe(false);

    act(() => vi.advanceTimersByTime(5000));
    expect(result.current.remaining).toBe(0);
  });

  it("rounds fractional seconds up and ignores non-positive values", () => {
    const { result } = renderHook(() => useCountdown());

    act(() => result.current.start(1.2));
    expect(result.current.remaining).toBe(2);

    act(() => result.current.start(0));
    expect(result.current.isRunning).toBe(false);
  });

  it("restarting while running moves the deadline", () => {
    const { result } = renderHook(() => useCountdown());

    act(() => result.current.start(2));
    act(() => vi.advanceTimersByTime(1000));
    expect(result.current.remaining).toBe(1);

    act(() => result.current.start(5));
    expect(result.current.remaining).toBe(5);
    act(() => vi.advanceTimersByTime(1000));
    expect(result.current.remaining).toBe(4);
  });
});
