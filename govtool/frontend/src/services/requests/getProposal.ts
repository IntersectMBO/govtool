import { checkIsMissingGAMetadata } from "@utils";
import { API } from "../API";

export const getProposal = async (proposalId: string, drepId?: string) => {
  const encodedHash = encodeURIComponent(proposalId);

  const { data } = await API.get(
    `/proposal/get/${encodedHash}?drepId=${drepId}`,
  );

  const isDataMissing = await checkIsMissingGAMetadata({
    hash: data?.proposal.metadataHash,
    url: data?.proposal.url,
  });

  return { ...data, isDataMissing };
};
