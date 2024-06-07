import { ProposalData } from "@models";
import { removeDuplicatedProposals } from "..";

const uniqueProposals = [
  {
    id: "1322",
    txHash: "2bca7756ba6c998518c1bccbcdd5165e32d3c8e0bfdf930d34359c98354e85a0",
    index: 0,
    type: "InfoAction",
    details: {
      description: "Info about InfoAction 1",
      additionalInfo: "Additional information for InfoAction 1",
    },
    expiryDate: "2024-01-08T15:32:13.61165Z",
    expiryEpochNo: 1673183533,
    createdDate: "2023-12-29T23:04:41Z",
    createdEpochNo: 1672350281,
    url: "https://bit.ly/3zCH2HL",
    metadataHash:
      "1111111111111111111111111111111111111111111111111111111111111111",
    yesVotes: 0,
    noVotes: 0,
    abstainVotes: 81528377728,
    title: "Proposal 1322 Title",
    about: "This is about Proposal 1322",
    metadataStatus: null,
    metadataValid: false,
    motivation: "Motivation behind Proposal 1322",
    rationale: "Rationale for Proposal 1322",
  },
  {
    id: "1338",
    txHash: "5e37f4d48182c4d8ff8e8ee7472c066501459b7bc8aaf6ca2f93a522ae12b0ea",
    index: 0,
    type: "InfoAction",
    details: {
      description: "Info about InfoAction 2",
      additionalInfo: "Additional information for InfoAction 2",
    },
    expiryDate: "2024-01-15T15:16:04.8932Z",
    expiryEpochNo: 1673895364,
    createdDate: "2024-01-05T23:06:02Z",
    createdEpochNo: 1672953962,
    url: "https://bit.ly/3zCH2HL",
    metadataHash:
      "2222222222222222222222222222222222222222222222222222222222222222",
    yesVotes: 0,
    noVotes: 0,
    abstainVotes: 81528377728,
    title: "Proposal 1338 Title",
    about: "This is about Proposal 1338",
    metadataStatus: null,
    metadataValid: false,
    motivation: "Motivation behind Proposal 1338",
    rationale: "Rationale for Proposal 1338",
  },
  {
    id: "1335",
    txHash: "e88ddff921de8b7f6079a1c25a301c034de6b3ec8a906ad75463f0f5b3597672",
    index: 0,
    type: "InfoAction",
    details: {
      description: "Info about InfoAction 3",
      additionalInfo: "Additional information for InfoAction 3",
    },
    expiryDate: "2024-01-14T15:18:23.28155Z",
    expiryEpochNo: 1673807903,
    createdDate: "2024-01-04T23:06:36Z",
    createdEpochNo: 1672867596,
    url: "https://bit.ly/3zCH2HL",
    metadataHash:
      "3333333333333333333333333333333333333333333333333333333333333333",
    yesVotes: 3175400714,
    noVotes: 0,
    abstainVotes: 81528377728,
    title: "Proposal 1335 Title",
    about: "This is about Proposal 1335",
    metadataStatus: null,
    metadataValid: false,
    motivation: "Motivation behind Proposal 1335",
    rationale: "Rationale for Proposal 1335",
  },
];

const duplicatedProposals = [
  ...uniqueProposals,
  {
    ...uniqueProposals[0],
  },
  {
    ...uniqueProposals[1],
  },
];

describe("remove duplicated proposals", () => {
  it("returns all proposals when all are unique", () => {
    expect(removeDuplicatedProposals(uniqueProposals).length).toBe(
      uniqueProposals.length,
    );
  });

  it("removes duplicate proposals based on txHash and index", () => {
    expect(removeDuplicatedProposals(duplicatedProposals).length).toBe(
      uniqueProposals.length,
    );
  });

  it("returns empty array if input is empty", () => {
    const proposals: ProposalData[] = [];
    expect(removeDuplicatedProposals(proposals).length).toBe(0);
  });
});
