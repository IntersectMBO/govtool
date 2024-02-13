from typing import TypedDict


class Proposal(TypedDict):
    id: str
    type: str
    details: str
    expiryDate: str
    url: str
    metadataHash: str
    yesVotes: int
    noVotes: int
    abstainVotes: int

class Drep(TypedDict):
    drepId: str
    url: str
    metadataHash: str
    deposit : int


class Delegation(TypedDict):
    stakeKey: str
    dRepId: str


class AdaHolder(TypedDict):
    stakeKey: str


class Vote(TypedDict):
    proposalId: str
    drepId: str
    vote: str
    url: str
    metadataHash: str


class VoteonProposal(TypedDict):
    vote: Vote
    proposal: Proposal
