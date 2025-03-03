import environments from "@constants/environments";
import { Page, Response } from "@playwright/test";
import { outcomeProposal } from "@types";

export default class OutcomeDetailsPage {
  readonly dRepYesVotes = this.page.getByTestId("submitted-votes-dReps-yes");
  readonly dRepNoVotes = this.page.getByTestId("submitted-votes-dReps-no");
  readonly dRepNotVoted = this.page.getByTestId(
    "submitted-votes-dReps-notVoted"
  );
  readonly dRepAbstainVotes = this.page.getByTestId(
    "submitted-votes-dReps-abstain"
  );

  readonly sPosYesVotes = this.page.getByTestId("submitted-votes-sPos-yes");
  readonly sPosNoVotes = this.page.getByTestId("submitted-votes-sPos-no");
  readonly sPosAbstainVotes = this.page.getByTestId(
    "submitted-votes-sPos-abstain"
  );

  readonly ccCommitteeYesVotes = this.page.getByTestId(
    "submitted-votes-ccCommittee-yes"
  );
  readonly ccCommitteeNoVotes = this.page.getByTestId(
    "submitted-votes-ccCommittee-no"
  );
  readonly ccCommitteeAbstainVotes = this.page.getByTestId(
    "submitted-votes-ccCommittee-abstain"
  );

  constructor(private readonly page: Page) {}

  get currentPage(): Page {
    return this.page;
  }

  async goto(proposalId: string) {
    await this.page.goto(
      `${environments.frontendUrl}/outcomes/governance_actions/${proposalId}`
    );
  }

  async getDRepTotalAbstainVoted(
    proposal: outcomeProposal,
    metricsResponses: Response
  ): Promise<number | undefined> {
    const alwaysAbstainVotingPower = await metricsResponses
      .json()
      .then((res) => res.alwaysAbstainVotingPower);
    if (
      alwaysAbstainVotingPower &&
      typeof alwaysAbstainVotingPower === "number"
    ) {
      const totalAbstainVoted =
        alwaysAbstainVotingPower + parseInt(proposal.abstain_votes);

      return totalAbstainVoted;
    } else {
      return parseInt(proposal.abstain_votes);
    }
  }
}
