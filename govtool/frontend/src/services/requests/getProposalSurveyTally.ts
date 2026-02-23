import { API } from "../API";
import { ProposalSurveyTallyResponse } from "@/models";

export const getProposalSurveyTally = async (
  proposalId: string,
  weighting: "CredentialBased" | "StakeBased" = "CredentialBased",
): Promise<ProposalSurveyTallyResponse> => {
  const encodedProposalId = encodeURIComponent(proposalId);
  const response = await API.get<ProposalSurveyTallyResponse>(
    `/proposal/survey/${encodedProposalId}/tally`,
    {
      params: { weighting },
    },
  );

  return response.data;
};
