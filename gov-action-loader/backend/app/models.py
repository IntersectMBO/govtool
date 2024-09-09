from pydantic import BaseModel


class MultipleProposal(BaseModel):
    proposal_type: str
    no_of_proposals: int
 