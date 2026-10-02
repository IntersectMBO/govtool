import environments from "@constants/environments";
import { Browser, expect, Page } from "@playwright/test";
import { actionRecordProposal, actionRecordType } from "@types";
import GovernanceActionHistoryPage from "./governanceActionHistoryPage";

export default class GovernanceActionHistoryDetailsPage {
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
      `${environments.frontendUrl}/governance_actions/history/${proposalId}`
    );
  }

  async shouldDisplayCorrectVotingResults(
    browser: Browser,
    isLoggedIn = false
  ) {
    // Visit serially: every detail page may need a provider read.
    for (const filterKey of Object.keys(actionRecordType)) {
      const actionRecordPage = new GovernanceActionHistoryPage(this.page);
      const { govActionDetailsPage, actionRecordResponsePromise } =
        await actionRecordPage.navigateToFilteredProposalDetail(
          browser,
          filterKey,
          isLoggedIn
        );
      if (!govActionDetailsPage) continue;
      const page = govActionDetailsPage.currentPage;
      try {
        const response = await actionRecordResponsePromise;
        expect(response.ok()).toBeTruthy();
        const proposal = await response.json();
        expect(Array.isArray(proposal.vote_aggregates)).toBeTruthy();
        for (const [role, prefix, title] of [
          ["drep", "DReps", "DReps"],
          ["spo", "SPOs", "SPOs"],
          ["cc", "CC", "Constitutional Committee"],
        ]) {
          const section = page.getByTestId(`${prefix}-voting-results-data`);
          await expect(section).toBeVisible();
          const aggregate = proposal.vote_aggregates.find(
            (a: { role: string }) => a.role === role
          );
          if (!aggregate) {
            // Unsupported and inapplicable roles have distinct explicit messages.
            await expect(section.getByRole("status")).toBeVisible();
            await expect(section.getByRole("progressbar")).toHaveCount(0);
            continue;
          }
          const format = (value: string) =>
            aggregate.representation === "percent"
              ? `${(Number(value) * 100).toFixed(2)}%`
              : aggregate.representation === "count"
                ? BigInt(value).toLocaleString("en-US")
                : `₳ ${((BigInt(value) + 999999n) / 1000000n).toLocaleString("en-US")}`;
          // Exact integer arithmetic: mainnet lovelace exceeds Number's safe range.
          const digits = Math.max(
            ...[aggregate.yes, aggregate.abstain, aggregate.totalEligible].map(
              (value: string) => value.split(".")[1]?.length ?? 0
            )
          );
          const scaled = (value: string) => {
            const [whole, fraction = ""] = value.split(".");
            return BigInt(whole + fraction.padEnd(digits, "0"));
          };
          const denominator =
            scaled(aggregate.totalEligible) - scaled(aggregate.abstain);
          if (denominator > 0n) {
            // Hundredths of a percent, rounded half up, as the UI rounds.
            const hundredths =
              (scaled(aggregate.yes) * 10000n + denominator / 2n) /
              denominator;
            const yesPercent = Number(hundredths) / 100;
            await expect(
              section.getByTestId(`${title}-yes-votes-submitted`)
            ).toHaveText(
              `${format(aggregate.yes)} - ${yesPercent.toFixed(2)}%`
            );
          } else {
            await expect(section.getByRole("progressbar")).toHaveCount(0);
          }
          await section.getByTestId(`${title}-expand-button`).click();
          for (const [field, suffix] of [
            ["yes", "yes-votes"],
            ["no", "no-votes"],
            ["notVoted", "not-voted-votes"],
            ["abstain", "abstain-votes"],
          ]) {
            await expect(section.getByTestId(`${title}-${suffix}`)).toHaveText(
              format(aggregate[field])
            );
          }
        }
      } finally {
        await page.close();
      }
    }
  }

  async verifyInvalidGovernanceActionMetadata({
    actionRecordResponse,
    type,
    url,
    hash,
  }: {
    actionRecordResponse: actionRecordProposal;
    type: string;
    url: string;
    hash: string;
  }) {
    let governanceActionPromise = this.page.route(
      "**/governance-actions/*",
      async (route) => {
        if (route.request().url().includes("/governance-actions/metadata")) {
          await route.continue();
        } else {
          await route.fulfill({ body: JSON.stringify(actionRecordResponse) });
        }
      }
    );
    const actionRecordPage = new GovernanceActionHistoryPage(this.page);
    await actionRecordPage.goto();
    await actionRecordPage.viewFirstGovernanceActionHistory();
    await governanceActionPromise;
    const actionRecordTitle = await actionRecordPage.title.textContent();

    await expect(
      actionRecordPage.title,
      actionRecordTitle.toLowerCase() !== type.toLowerCase() &&
        `The URL "${url}" and hash "${hash}" do not match the expected properties for type "${type}".`
    ).toHaveText(type, {
      ignoreCase: true,
      timeout: 60_000,
    });
    await expect(actionRecordPage.metadataErrorLearnMoreBtn).toBeVisible();
  }
}
