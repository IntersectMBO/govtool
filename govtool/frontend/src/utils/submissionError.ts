import { MetadataValidationStatus } from "@/models/metadataValidation";

export type SubmissionError =
  | { kind: "metadata"; status: MetadataValidationStatus; text: string }
  | { kind: "cancelled" | "insufficient" | "failed"; text: string };

// Only the user declining: a node or the wallet refusing a transaction
// ("rejected by node", CIP-30's "Refused") is a failure, not a cancel.
const USER_DECLINED = /user\s*(declin|reject|cancel|denied)|declined/i;

const textOf = (error: unknown): string => {
  if (error === undefined || error === null) return "";
  if (typeof error === "string") return error;
  if (error instanceof Error) return error.message;
  if (typeof error === "object" && "info" in error && typeof error.info === "string") {
    return error.info;
  }
  try {
    return JSON.stringify(error);
  } catch {
    return String(error);
  }
};

/**
 * What went wrong when a governance action was submitted: a failed metadata
 * check (thrown as its status), the wallet declining, too little ada, or
 * anything else. Wallets throw strings, CIP-30 error objects or Errors.
 */
export const readSubmissionError = (error: unknown): SubmissionError => {
  const text = textOf(error);
  if (Object.values(MetadataValidationStatus).includes(error as MetadataValidationStatus)) {
    return { kind: "metadata", status: error as MetadataValidationStatus, text };
  }
  if (USER_DECLINED.test(text)) return { kind: "cancelled", text };
  if (/insufficient/i.test(text)) return { kind: "insufficient", text };
  return { kind: "failed", text };
};
