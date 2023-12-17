import { API } from "../API";

export const postDRepRemoveVote = async ({
  drepId,
  proposalId,
}: {
  drepId: string;
  proposalId: string;
}) => {
  const request = await API.post("/drep/removeVote", {
    drepId,
    proposalId,
  });

  return request.data;
};
