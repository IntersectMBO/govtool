import {
  EpochParams,
  GovernanceActionMetadata,
  GovernanceActionRecord,
  GovernanceActionProposalDiscussion,
  GovernanceActionSignatureVerificationDto,
  GovernanceActionSignatureVerificationResult,
} from "@/models";

import { GovernanceActionsAPI } from "../GovernanceActionsAPI";

// Governance action reads and author verification on the GovTool backend.

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

export const getGovernanceActionProposalDiscussion = async (txHash: string) => {
  const { data } = await GovernanceActionsAPI.get<{
    data: GovernanceActionProposalDiscussion | null;
  }>(`/governance-actions/proposal/${txHash}`);
  return data;
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
