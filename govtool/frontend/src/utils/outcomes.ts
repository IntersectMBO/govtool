import { OutcomeGovernanceAction, OutcomeStatus } from "@/models";

import { correctAdaFormatWithSuffix } from "./adaFormat";
import {
  decodeCIP129Identifier,
  encodeCIP129Identifier,
} from "./cip129identifier";

const LOVELACE = 1000000;

export type OutcomeProposalStatus =
  "Enacted" | "Ratified" | "Expired" | "Not Ratified" | "Live";

export const getOutcomeProposalStatus = (
  status: OutcomeStatus,
): OutcomeProposalStatus => {
  if (status.enacted_epoch !== null) return "Enacted";
  if (status.ratified_epoch !== null) return "Ratified";
  if (status.expired_epoch !== null) return "Expired";
  if (status.dropped_epoch !== null) return "Not Ratified";
  return "Live";
};

/**
 * `Tue Mar 04 2025 10:00:00 AM` (full) or `Tue Mar 04 2025` (short), in the
 * viewer's time zone. The playwright outcomes suite parses the full form.
 */
export const formatOutcomeTimestamp = (
  timeStamp: string,
  format: "short" | "full" = "full",
) => {
  const date = new Date(timeStamp);

  if (format === "short") {
    return date
      .toLocaleDateString("en-US", {
        weekday: "short",
        month: "short",
        day: "2-digit",
        year: "numeric",
      })
      .replace(",", "");
  }

  return date
    .toLocaleString("en-US", {
      weekday: "short",
      month: "short",
      day: "2-digit",
      year: "numeric",
      hour: "2-digit",
      minute: "2-digit",
      second: "2-digit",
      hour12: true,
    })
    .replace(",", "");
};

export const getOutcomeCIP129Id = (
  action: Pick<OutcomeGovernanceAction, "tx_hash" | "index">,
) =>
  encodeCIP129Identifier({
    txID: action.tx_hash,
    index: action.index.toString(16).padStart(2, "0"),
    bech32Prefix: "gov_action",
  });

/**
 * `txHash#index` for a CIP-129 `gov_action1…` id; anything else is returned
 * as given, so a CIP-105 id or a free-text search passes through.
 */
export const toOutcomeGovActionId = (id: string) => {
  if (!id.startsWith("gov_action")) return id;
  try {
    const { txID, index } = decodeCIP129Identifier(id);
    return `${txID}#${parseInt(index || "0", 16)}`;
  } catch {
    return id;
  }
};

/** A committee cold credential as CIP-129, or "" for a malformed hash. */
export const encodeOutcomeCommitteeColdId = (
  keyHash: string,
  hasScript?: boolean,
) => {
  if (!keyHash || keyHash.length !== 56) return "";
  return encodeCIP129Identifier({
    txID: (hasScript ? "13" : "12") + keyHash,
    bech32Prefix: "cc_cold",
  });
};

/** `₳ 1.23M` for stake, or the plain count for the committee. */
export const formatOutcomeVoteValue = (
  value: number,
  isCC: boolean,
  addAdaPrefix = true,
) => {
  if (isCC) return value;
  return `${addAdaPrefix ? "₳" : ""} ${correctAdaFormatWithSuffix(value)}`;
};

/** Whole ada, rounded up. */
export const lovelaceToRoundedUpAda = (lovelace: number | undefined) => {
  if (!lovelace) return 0;
  return Math.ceil(lovelace / LOVELACE);
};
