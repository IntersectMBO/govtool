import environments from "@constants/environments";
import { formatWithThousandSeparator } from "@helpers/adaFormat";
import { Page, Response } from "@playwright/test";
import { outcomeProposal, VoterType } from "@types";

export default class OutcomeDetailsPage {
  readonly dRepYesVotes = this.page.getByTestId("DReps-yes-votes-submitted");
  readonly dRepNoVotes = this.page.getByTestId("DReps-no-votes-submitted");
  readonly dRepNotVoted = this.page.getByTestId(
    "submitted-votes-dReps-notVoted"
  );
  readonly dRepAbstainVotes = this.page.getByTestId(
    "submitted-votes-dReps-abstain"
  );
  readonly dRepExpandButton = this.page.getByTestId("DReps-expand-button");

  readonly sPosYesVotes = this.page.getByTestId("SPOs-yes-votes-submitted");
  readonly sPosNoVotes = this.page.getByTestId("SPOs-no-votes-submitted");
  readonly sPosAbstainVotes = this.page.getByTestId(
    "submitted-votes-sPos-abstain"
  );
  readonly sPosExpandButton = this.page.getByTestId("SPOs-expand-button");

  readonly ccCommitteeYesVotes = this.page.getByTestId(
    "Constitutional Committee-yes-votes-submitted"
  );
  readonly ccCommitteeNoVotes = this.page.getByTestId(
    "Constitutional Committee-no-votes-submitted"
  );
  readonly ccCommitteeAbstainVoteResult = this.page.getByTestId(
    "CC-voting-results-data"
  );

  readonly dRepResultData = this.page.getByTestId("DReps-voting-results-data");
  readonly sPosResultData = this.page.getByTestId("SPOs-voting-results-data");
  readonly cCResultData = this.page.getByTestId("CC-voting-results-data");

  constructor(private readonly page: Page) {}

  get currentPage(): Page {
    return this.page;
  }

  async goto(proposalId: string) {
    await this.page.goto(
      `${environments.frontendUrl}/outcomes/governance_actions/${proposalId}`
    );
  }

  async getSposAndDRepAbstainNoConfidence(metricsResponses: Response): Promise<{
    autoAbstain: string;
    noConfidence: string;
    sPosAutoAbstain: string;
    sPosNoConfidence: string;
  }> {
    const response = await metricsResponses.json();
    const LOVELACE = 1000000;
    let autoAbstain: string = "0";
    let noConfidence: string = "0";
    let sPosAutoAbstain: string = "0";
    let sPosNoConfidence: string = "0";
    if (response) {
      autoAbstain = formatWithThousandSeparator(
        Math.ceil(response.always_abstain_voting_power / LOVELACE)
      );
      noConfidence = formatWithThousandSeparator(
        Math.ceil(response.always_no_confidence_voting_power / LOVELACE)
      );

      sPosAutoAbstain = formatWithThousandSeparator(
        Math.ceil(response.spos_abstain_voting_power / LOVELACE)
      );
      sPosNoConfidence = formatWithThousandSeparator(
        Math.ceil(response.spos_no_confidence_voting_power / LOVELACE)
      );
    }
    return {
      autoAbstain,
      noConfidence,
      sPosAutoAbstain,
      sPosNoConfidence,
    };
  }
}
