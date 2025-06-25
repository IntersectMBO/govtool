import { verifiedFetch } from "@helia/verified-fetch";
import { VotedProposal, ProposalData } from "@/models";
import { decodeCIP129Identifier } from "@/utils";

import { API } from "../API";

export const getProposal = async (
  proposalId: string,
  drepId?: string,
): Promise<VotedProposal> => {
  const isCIP129Identifier = proposalId.startsWith("gov_action");
  if (isCIP129Identifier) {
    const { txID, index } = decodeCIP129Identifier(proposalId);
    proposalId = `${txID}#${parseInt(index, 16)}`;
  }

  const encodedHash = encodeURIComponent(proposalId);

  const { data } = await API.get<VotedProposal>(
    `/proposal/get/${encodedHash}?drepId=${drepId}`,
  );
  return { ...data, proposal: await fillProposalMetadata(data.proposal) };
};

export const fillProposalMetadata = async (
  proposal: ProposalData,
): Promise<ProposalData> => {
  if (
    proposal.authors &&
    Array.isArray(proposal.authors) &&
    proposal.authors.length > 0 &&
    proposal.json &&
    typeof proposal.json === "object" &&
    proposal.json !== null
  ) {
    return proposal;
  }

  const metadata = await getMetadata(proposal.url);
  // If authors are empty, return the authors from the ipfs file
  proposal.authors =
    metadata && Array.isArray(metadata.authors)
      ? metadata.authors.map((a: { witness: null } | null) =>
          (typeof a === "object" &&
          a !== null &&
          typeof a.witness === "object" &&
          a.witness !== null
            ? {
                ...(typeof a === "object" && a !== null ? a : {}),
                ...(typeof a?.witness === "object" && a.witness !== null
                  ? a.witness
                  : {}),
              }
            : a),
        )
      : [];

  proposal.json = metadata || {};

  return proposal;
};

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const getMetadata = async (url: string): Promise<any | undefined> => {
  if (!url.startsWith("ipfs://")) {
    return undefined;
  }
  const resp = await verifiedFetch(url);

  return resp.json();
};
