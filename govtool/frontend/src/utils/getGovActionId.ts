import { generatePath } from "react-router";

import { GOVERNANCE_ACTION_TYPE_PARAM, PATHS } from "@/consts/paths";

import {
  decodeCIP129Identifier,
  encodeCIP129Identifier,
} from "./cip129identifier";

export const getShortenedGovActionId = (
  txHash: string,
  index: number | string,
) => {
  if (txHash.length <= 6) {
    return `${txHash}#${index}`;
  }

  const firstPart = txHash.slice(0, 4);
  const lastPart = txHash.slice(-4);

  return txHash.startsWith("gov_action")
    ? `${firstPart}...${lastPart}`
    : `${firstPart}...${lastPart}#${index}`;
};

export const getFullGovActionId = (txHash: string, index: number | string) =>
  (txHash.startsWith("gov_action") ? txHash : `${txHash}#${index}`);

/** The CIP-129 gov_action1… id that names an action in its URL. */
export const getGovActionCIP129Id = (txHash: string, index: number | string) =>
  encodeCIP129Identifier({
    txID: txHash,
    index: Number(index).toString(16).padStart(2, "0"),
    bech32Prefix: "gov_action",
  });

/** The transaction and index a gov_action1… id names, or null if it is not one. */
export const parseGovActionCIP129Id = (id: string) => {
  if (!id.startsWith("gov_action1")) return null;
  try {
    const { txID, index, prefix } = decodeCIP129Identifier(id);
    if (prefix !== "gov_action" || txID.length !== 64) return null;
    return { txHash: txID, index: parseInt(index || "0", 16) };
  } catch {
    return null;
  }
};

/** An action's page, /actions/gov_action1…. */
export const getGovActionPath = (txHash: string, index: number | string) =>
  generatePath(PATHS.governanceActionsAction, {
    proposalId: getGovActionCIP129Id(txHash, index),
  });

/** The governance actions list filtered to one type, /actions?type=…. */
export const getGovActionsTypePath = (type: string) =>
  `${PATHS.governanceActions}?${new URLSearchParams({
    [GOVERNANCE_ACTION_TYPE_PARAM]: type,
  })}`;
