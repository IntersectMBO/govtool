import axios from "axios";

import { env } from "@/config/env";
import {
  EpochParams,
  GovernanceActionMetadata,
  GovernanceActionRecord,
  GovernanceActionProposalDiscussion,
  GovernanceActionSignatureVerificationDto,
  GovernanceActionSignatureVerificationResult,
} from "@/models";

import { GovernanceActionsAPI } from "../GovernanceActionsAPI";

// Governance action reads and author verification on the GovTool backend; the
// forum proposal lookup goes to the pdf API.

export const getGovernanceActionHistory = async (
  search: string,
  filters: string[],
  sort: string,
  page: number,
  limit: number,
) => {
  const { data } = await GovernanceActionsAPI.get<GovernanceActionRecord[]>(
    "/governance-actions",
    {
      params: {
        search,
        filters: filters.join(","),
        sort,
        page,
        limit,
      },
    },
  );
  return data;
};

/** `id` is `txHash#index`. */
export const getGovernanceActionRecord = async (id: string) => {
  const [hash, indexStr] = id.split("#");
  const { data } = await GovernanceActionsAPI.get<GovernanceActionRecord>(
    `/governance-actions/${hash}`,
    { params: { index: indexStr || "0" } },
  );
  return data;
};

export const getGovernanceActionMetadata = async (
  url: string,
  hash: string,
) => {
  const { data } = await GovernanceActionsAPI.get<GovernanceActionMetadata>(
    "/governance-actions/metadata",
    { params: { url, hash } },
  );
  return data;
};

/**
 * The discussion forum proposal submitted in `txHash`, or null when there is
 * none. Asks the pdf API directly, as the forum pages do.
 */
export const getGovernanceActionProposalDiscussion = async (txHash: string) => {
  const { data } = await axios.get<{
    data?: GovernanceActionProposalDiscussion[];
  }>("/api/proposals", {
    baseURL: String(env.VITE_PDF_API_URL ?? "").replace(/\/+$/, ""),
    params: {
      "filters[prop_submission_tx_hash][$eq]": txHash.toLowerCase(),
      "pagination[page]": 1,
      "pagination[pageSize]": 1,
    },
    timeout: 30_000,
  });
  return data.data?.[0] ?? null;
};

export const getGovernanceActionEpochParams = async (epoch?: number) => {
  const { data } = await GovernanceActionsAPI.get<EpochParams>("/misc/epoch/params", {
    params: { epoch },
  });
  return data;
};

export const postGovernanceActionVerifySignature = async (
  body: GovernanceActionSignatureVerificationDto,
) => {
  const { data } = await GovernanceActionsAPI.post<GovernanceActionSignatureVerificationResult>(
    "/misc/verify-signature",
    body,
  );
  return data;
};
