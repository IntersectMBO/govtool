import { EnactedProposalDetails } from "@/models";
import { GovernanceActionType } from "@/types/governanceAction";
import { API } from "../API";

export const getEnactedProposalDetails = async (
  type: GovernanceActionType,
) => {
  const response = await API.get<EnactedProposalDetails | null>(
    "/proposal/enacted-details",
    { params: { type } },
  );

  return response.data;
};
