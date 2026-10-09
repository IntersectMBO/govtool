import { act, renderHook } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { getTransactionStatus } from "@services";
import { usePendingTransaction } from "./usePendingTransaction";
import { refetchData } from "./utils";

const alerts = {
  addSuccessAlert: vi.fn(),
  addWarningAlert: vi.fn(),
  addErrorAlert: vi.fn(),
};

vi.mock("@services", () => ({ getTransactionStatus: vi.fn() }));
vi.mock("@hooks", () => ({ useTranslation: () => ({ t: (key: string) => key }) }));
vi.mock("..", () => ({
  useModal: () => ({ openModal: vi.fn(), closeModal: vi.fn() }),
  useSnackbar: () => alerts,
}));
vi.mock("@tanstack/react-query", () => ({ useQueryClient: () => ({}) }));
vi.mock("./utils", async (importOriginal) => ({
  ...(await importOriginal<typeof import("./utils")>()),
  refetchData: vi.fn(),
}));

const mockStatus = vi.mocked(getTransactionStatus);
const mockRefetch = vi.mocked(refetchData);

const start = (transaction: Parameters<
  ReturnType<typeof usePendingTransaction>["updateTransaction"]
>[0]) => {
  const hook = renderHook(() =>
    usePendingTransaction({ isEnabled: true, stakeKey: "stake_test1" }),
  );
  act(() => hook.result.current.updateTransaction(transaction));
  return hook;
};

describe("usePendingTransaction", () => {
  beforeEach(() => {
    vi.useFakeTimers();
    localStorage.clear();
  });
  afterEach(() => {
    vi.useRealTimers();
    vi.clearAllMocks();
  });

  it("confirms a vote once its transaction is on chain, even with an empty votingProcedure", async () => {
    mockStatus.mockResolvedValue({
      transactionConfirmed: true,
      votingProcedure: [],
    } as never);
    mockRefetch.mockResolvedValue(true);

    const hook = start({
      type: "vote",
      transactionHash: "ab".repeat(32),
      resourceId: "gov_action1example",
    });
    await act(() => vi.advanceTimersByTimeAsync(0));

    expect(alerts.addSuccessAlert).toHaveBeenCalledWith("alerts.vote.success");
    expect(hook.result.current.pendingTransaction.vote).toBeNull();
  });

  it("keeps polling after confirmation and expires the transaction if the change never shows", async () => {
    mockStatus.mockResolvedValue({
      transactionConfirmed: true,
      votingProcedure: [],
    } as never);
    // The backend never reports the new delegation.
    mockRefetch.mockResolvedValue("some_other_drep");

    const hook = start({
      type: "delegate",
      transactionHash: "cd".repeat(32),
      resourceId: "expected_drep",
    });

    await act(() => vi.advanceTimersByTimeAsync(60 * 1000));
    expect(hook.result.current.pendingTransaction.delegate).not.toBeNull();
    expect(alerts.addErrorAlert).not.toHaveBeenCalled();
    const checksInFirstMinute = mockStatus.mock.calls.length;
    expect(checksInFirstMinute).toBeGreaterThan(1);

    await act(() => vi.advanceTimersByTimeAsync(3 * 60 * 1000));
    expect(alerts.addErrorAlert).toHaveBeenCalledWith("alerts.delegate.failed");
    expect(alerts.addSuccessAlert).not.toHaveBeenCalled();
    expect(hook.result.current.pendingTransaction.delegate).toBeNull();

    // Resolved: no further checks once the transaction is cleared.
    const checks = mockStatus.mock.calls.length;
    await act(() => vi.advanceTimersByTimeAsync(60 * 1000));
    expect(mockStatus.mock.calls.length).toBe(checks);
  });

  it("notes when a registration expires unconfirmed, so the voter info keeps reading", async () => {
    // Not on chain within the 3 minutes.
    mockStatus.mockResolvedValue({
      transactionConfirmed: false,
      votingProcedure: [],
    } as never);

    const hook = start({ type: "registerAsDrep", transactionHash: "ef".repeat(32) });
    await act(() => vi.advanceTimersByTimeAsync(4 * 60 * 1000));

    expect(hook.result.current.pendingTransaction.registerAsDrep).toBeNull();
    expect(
      JSON.parse(
        localStorage.getItem("voter_transaction_expired_stake_test1") ?? "null",
      ),
    ).toEqual(expect.any(Number));
  });

  it("does not note an expired transaction that does not change the voter", async () => {
    mockStatus.mockResolvedValue({
      transactionConfirmed: false,
      votingProcedure: [],
    } as never);

    start({
      type: "vote",
      transactionHash: "ab".repeat(32),
      resourceId: "gov_action1example",
    });
    await act(() => vi.advanceTimersByTimeAsync(4 * 60 * 1000));

    expect(localStorage.getItem("voter_transaction_expired_stake_test1")).toBeNull();
  });
});
