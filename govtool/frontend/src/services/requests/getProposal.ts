import { checkIsMissingGAMetadata } from "@utils";
import { API } from "../API";

export const getProposal = async (proposalId: string, drepId?: string) => {
  const encodedHash = encodeURIComponent(proposalId);

  const { data } = await API.get(
    `/proposal/get/${encodedHash}?drepId=${drepId}`,
  );

  const { metadata, status } = await checkIsMissingGAMetadata({
    hash: data?.proposal.metadataHash,
    url: data?.proposal.url,
  });
  // workaround for the missing data in db-sync
  return {
    ...data,
    proposal: { ...data.proposal, ...metadata },
    isDataMissing: status || false,
  };
};
