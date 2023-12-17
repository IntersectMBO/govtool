import { API } from "../API";

export const postDRepVote = async ({
  proposalId,
  drepId,
  vote,
  url,
  metadataHash,
}: {
  proposalId: string;
  drepId: string;
  vote: string;
  url: string;
  metadataHash: string;
}) => {
  const request = await API.post("/drep/vote", {
    drepId,
    vote,
    url,
    metadataHash,
    proposalId,
  });

  return request.data;
};
