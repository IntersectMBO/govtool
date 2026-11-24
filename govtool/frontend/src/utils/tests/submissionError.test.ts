import { describe, expect, it } from "vitest";

import { MetadataValidationStatus } from "@/models/metadataValidation";

import { readSubmissionError } from "../submissionError";

describe("readSubmissionError", () => {
  it("reports a metadata check failure by its status", () => {
    expect(readSubmissionError(MetadataValidationStatus.INVALID_HASH)).toEqual({
      kind: "metadata",
      status: MetadataValidationStatus.INVALID_HASH,
      text: "INVALID_HASH",
    });
  });

  it("reads a wallet decline as cancelled", () => {
    expect(readSubmissionError("user declined sign tx")).toMatchObject({ kind: "cancelled" });
    expect(readSubmissionError({ code: 2, info: "User rejected the request" })).toMatchObject({
      kind: "cancelled",
      text: "User rejected the request",
    });
  });

  it("reads wallet sign declines in their usual wordings as cancelled", () => {
    expect(readSubmissionError("UserDeclined")).toMatchObject({ kind: "cancelled" });
    expect(readSubmissionError("User declined to sign the transaction")).toMatchObject({
      kind: "cancelled",
    });
    expect(readSubmissionError("user canceled")).toMatchObject({ kind: "cancelled" });
  });

  it("does not read a node or wallet refusal as the user cancelling", () => {
    expect(readSubmissionError("Transaction rejected by node: BadInputsUTxO")).toEqual({
      kind: "failed",
      text: "Transaction rejected by node: BadInputsUTxO",
    });
    expect(readSubmissionError({ code: 1, info: "Refused" })).toMatchObject({ kind: "failed" });
  });

  it("reads an insufficient balance from a string or an Error", () => {
    expect(readSubmissionError("InsufficientFunds")).toMatchObject({ kind: "insufficient" });
    expect(readSubmissionError(new Error("Insufficient input in transaction"))).toMatchObject({
      kind: "insufficient",
    });
  });

  it("keeps any other error's message", () => {
    expect(readSubmissionError(new Error("Could not get UTxOs"))).toEqual({
      kind: "failed",
      text: "Could not get UTxOs",
    });
    expect(readSubmissionError({ reason: "x" })).toEqual({ kind: "failed", text: '{"reason":"x"}' });
    expect(readSubmissionError(undefined)).toEqual({ kind: "failed", text: "" });
  });
});
