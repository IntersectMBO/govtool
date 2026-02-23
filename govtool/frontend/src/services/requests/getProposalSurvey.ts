import { API } from "../API";
import { ProposalSurveyResponse } from "@/models";

export const getProposalSurvey = async (
  proposalId: string,
): Promise<ProposalSurveyResponse> => {
  const encodedProposalId = encodeURIComponent(proposalId);
  const response = await API.get<ProposalSurveyResponse>(
    `/proposal/survey/${encodedProposalId}`,
  );

  return response.data;
};
