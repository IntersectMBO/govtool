import environments from "@constants/environments";
import { formatWithThousandSeparator } from "@helpers/adaFormat";
import { Browser, expect, Page, Response } from "@playwright/test";
import { outcomeProposal, outcomeType } from "@types";
import OutComesPage from "./outcomesPage";
import {
  areCCVoteTotalsDisplayed,
  areDRepVoteTotalsDisplayed,
  areSPOVoteTotalsDisplayed,
} from "@helpers/featureFlag";
import { parseVotingPowerAndPercentage } from "@helpers/index";

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

  async shouldDisplayCorrectVotingResults(
    browser: Browser,
    isLoggedIn = false
  ) {
    await Promise.all(
      Object.keys(outcomeType).map(async (filterKey) => {
        const outcomePage = new OutComesPage(this.page);
        const {
          govActionDetailsPage,
          metricsResponsePromise,
          outcomeResponsePromise,
        } = await outcomePage.navigateToFilteredProposalDetail(
          browser,
          filterKey,
          isLoggedIn
        );

        const outcomeResponse = await outcomeResponsePromise;
        const proposalToCheck = (await outcomeResponse.json())[0];

        const metricsResponse = await metricsResponsePromise;

        const { autoAbstain, noConfidence, sPosAutoAbstain, sPosNoConfidence } =
          await govActionDetailsPage.getSposAndDRepAbstainNoConfidence(
            metricsResponse
          );

        const currentPageUrl = govActionDetailsPage.currentPage.url();

        // check dRep votes
        if (await areDRepVoteTotalsDisplayed(proposalToCheck)) {
          await govActionDetailsPage.dRepExpandButton.click();

          await expect(
            govActionDetailsPage.dRepResultData.getByRole("row", {
              name: "Yes",
            }),
            {
              message: `DRep "Yes" voting power checked for ${currentPageUrl}`,
            }
          ).toHaveText(
            `Yes${formatWithThousandSeparator(proposalToCheck.yes_votes, false)}`,
            {
              timeout: 60_000,
            }
          ); //BUG missing testIds

          await expect(
            govActionDetailsPage.dRepResultData.getByRole("row", {
              name: "Auto-Abstain",
            }),
            {
              message: `DRep "Auto-Abstain" voting power checked for ${currentPageUrl}`,
            }
          ).toHaveText(`Auto-Abstain${autoAbstain}`); //BUG missing testIds
          await expect(
            govActionDetailsPage.dRepResultData.getByRole("row", {
              name: "No Confidence",
            }),
            {
              message: `DRep "No Confidence" voting power checked for ${currentPageUrl}`,
            }
          ).toHaveText(`No Confidence${noConfidence}`); //BUG missing testIds
          await expect(
            govActionDetailsPage.dRepResultData.getByRole("row", {
              name: "Explicit",
            }),
            {
              message: `DRep "Explicit" voting power checked for ${currentPageUrl}`,
            }
          ).toHaveText(
            `Explicit${formatWithThousandSeparator(proposalToCheck.abstain_votes, false)}`
          );

          await expect(
            govActionDetailsPage.dRepResultData
              .getByRole("row", {
                name: "No",
              })
              .first(),
            {
              message: `DRep "No" voting power checked for ${currentPageUrl}`,
            }
          ).toHaveText(
            `No${formatWithThousandSeparator(proposalToCheck.no_votes, false)}`
          ); //BUG missing testIds
        }

        // check sPos votes
        if (await areSPOVoteTotalsDisplayed(proposalToCheck)) {
          await govActionDetailsPage.sPosExpandButton.click();
          const totalSposNoVotes =
            filterKey === "NoConfidence"
              ? proposalToCheck.pool_no_votes
              : parseInt(sPosNoConfidence.replace(/,/g, "")) * 1000000 +
                parseInt(proposalToCheck.pool_no_votes);

          const totalSposYesVotesForNoConfidence =
            parseInt(sPosNoConfidence.replace(/,/g, "")) * 1000000 +
            parseInt(proposalToCheck.pool_yes_votes);

          const totalSposYesVotes =
            filterKey === "NoConfidence"
              ? totalSposYesVotesForNoConfidence
              : proposalToCheck.pool_yes_votes;
          await expect(
            govActionDetailsPage.sPosResultData.getByRole("row", {
              name: "Yes",
            }),
            {
              message: `SPos "Yes" voting power checked for ${currentPageUrl}`,
            }
          ).toHaveText(
            `Yes${formatWithThousandSeparator(totalSposYesVotes, false)}`,
            {
              timeout: 60_000,
            }
          ); //BUG missing testIds

          await expect(
            govActionDetailsPage.sPosResultData.getByRole("row", {
              name: "Auto-Abstain",
            }),
            {
              message: `SPos "Auto-Abstain" voting power checked for ${currentPageUrl}`,
            }
          ).toHaveText(`Auto-Abstain${sPosAutoAbstain}`); //BUG missing testIds
          await expect(
            govActionDetailsPage.sPosResultData.getByRole("row", {
              name: "No Confidence",
            }),
            {
              message: `SPos "No Confidence" voting power checked for ${currentPageUrl}`,
            }
          ).toHaveText(`No Confidence${sPosNoConfidence}`); //BUG missing testIds
          await expect(
            govActionDetailsPage.sPosResultData.getByRole("row", {
              name: "Explicit",
            }),
            {
              message: `SPos "Explicit" voting power checked for ${currentPageUrl}`,
            }
          ).toHaveText(
            `Explicit${formatWithThousandSeparator(proposalToCheck.pool_abstain_votes, false)}`
          ); //BUG missing testIds
          await expect(
            govActionDetailsPage.sPosResultData
              .getByRole("row", {
                name: "No",
              })
              .first(),
            {
              message: `SPos "No" voting power checked for ${currentPageUrl}`,
            }
          ).toHaveText(
            `No${formatWithThousandSeparator(totalSposNoVotes, false)}`
          ); //BUG missing testIds
        }

        // check ccCommittee votes
        if (areCCVoteTotalsDisplayed(proposalToCheck)) {
          const ccYesVoteSubmittedText =
            await govActionDetailsPage.ccCommitteeYesVotes.textContent();

          const { percentage: yesPercentage } = parseVotingPowerAndPercentage(
            ccYesVoteSubmittedText
          );

          await expect(govActionDetailsPage.ccCommitteeYesVotes, {
            message: `CC "Yes" vote count checked for ${currentPageUrl}`,
          }).toHaveText(`${proposalToCheck.cc_yes_votes} - ${yesPercentage}`);
          await expect(
            govActionDetailsPage.cCResultData.getByRole("row", {
              name: "Abstain Votes",
            }),
            {
              message: `CC "Abstain" vote count checked for ${currentPageUrl}`,
            }
          ).toHaveText(`Abstain Votes${proposalToCheck.pool_abstain_votes}`); //BUG missing testIds

          const noPercentage = 100 - parseFloat(yesPercentage.replace("%", ""));
          await expect(govActionDetailsPage.ccCommitteeNoVotes, {
            message: `CC "No" vote count checked for ${currentPageUrl}`,
          }).toHaveText(
            `${proposalToCheck.cc_no_votes} - ${noPercentage.toFixed(2)}%`
          );
        }
      })
    );
  }

  async verifyInvalidOutcomeMetadata({
    outcomeResponse,
    type,
    url,
    hash,
  }: {
    outcomeResponse: outcomeProposal;
    type: string;
    url: string;
    hash: string;
  }) {
    await this.page.route(/.*\/governance-actions\/[a-f0-9]{64}\?.*/, (route) =>
      route.fulfill({ body: JSON.stringify([outcomeResponse]) })
    );

    const outcomePage = new OutComesPage(this.page);
    await outcomePage.goto();
    await outcomePage.viewFirstOutcomes();
    const outcomeTitle = await outcomePage.title.textContent();

    await expect(
      outcomePage.title,
      outcomeTitle.toLowerCase() !== type.toLowerCase() &&
        `The URL "${url}" and hash "${hash}" do not match the expected properties for type "${type}".`
    ).toHaveText(type, {
      ignoreCase: true,
      timeout: 60_000,
    });
    await expect(outcomePage.metadataErrorLearnMoreBtn).toBeVisible();
  }
}
