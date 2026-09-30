import {
  EpochParams,
  OutcomeGovActionMetadata,
  OutcomeGovernanceAction,
  OutcomeNetworkMetrics,
  OutcomeProposalDiscussion,
  OutcomeSignatureVerificationDto,
  OutcomeSignatureVerificationResult,
} from "@/models";

import { OutcomesAPI } from "../OutcomesAPI";

// The outcomes UI's routes on VITE_OUTCOMES_API_URL (docs/api decision D143).
// Paths and query parameter order are what the playwright outcomes suite
// matches responses on, so keep them as they are.

export const getOutcomeGovernanceActions = async (
  search: string,
  filters: string[],
  sort: string,
  page: number,
  limit: number,
) => {
  const { data } = await OutcomesAPI.get<OutcomeGovernanceAction[]>(
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
export const getOutcomeGovernanceAction = async (id: string) => {
  const [hash, indexStr] = id.split("#");
  const { data } = await OutcomesAPI.get<OutcomeGovernanceAction>(
    `/governance-actions/${hash}`,
    { params: { index: indexStr || "0" } },
  );
  return data;
};

export const getOutcomeGovActionMetadata = async (
  url: string,
  hash: string,
) => {
  const { data } = await OutcomesAPI.get<OutcomeGovActionMetadata>(
    "/governance-actions/metadata",
    { params: { url, hash } },
  );
  return data;
};

export const getOutcomeProposalDiscussion = async (txHash: string) => {
  const { data } = await OutcomesAPI.get<{
    data: OutcomeProposalDiscussion | null;
  }>(`/governance-actions/proposal/${txHash}`);
  return data;
};

export const getOutcomeNetworkMetrics = async (epoch?: number) => {
  const { data } = await OutcomesAPI.get<OutcomeNetworkMetrics>(
    "/misc/network/metrics",
    { params: { epoch } },
  );
  return data;
};

export const getOutcomeEpochParams = async (epoch?: number) => {
  const { data } = await OutcomesAPI.get<EpochParams>("/misc/epoch/params", {
    params: { epoch },
  });
  return data;
};

export const postOutcomeVerifySignature = async (
  body: OutcomeSignatureVerificationDto,
) => {
  const { data } = await OutcomesAPI.post<OutcomeSignatureVerificationResult>(
    "/misc/verify-signature",
    body,
  );
  return data;
};
