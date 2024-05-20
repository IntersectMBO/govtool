import { Page, expect } from "@playwright/test";
import { IDRep } from "@types";
import environments from "lib/constants/environments";
import { withTxConfirmation } from "lib/transaction.decorator";

export default class DRepDirectoryPage {
  readonly otherOptionsBtn = this.page.getByText("Other options");
  readonly nextStepBtn = this.page.getByTestId("next-step-button");
  readonly dRepInput = this.page.getByRole("textbox");
  readonly searchInput = this.page.getByTestId("search-input");
  readonly filterBtn = this.page.getByTestId("filters-button");
  readonly sortBtn = this.page.getByTestId("sort-button");
  readonly showMoreBtn = this.page.getByTestId("show-more-button");

  readonly automaticDelegationOptionsDropdown = this.page.getByRole("button", {
    name: "Automated Voting Options arrow",
  }); // BUG: testId -> delegation-options-dropdown

  readonly delegateToDRepCard = this.page.getByTestId("delegate-to-drep-card");
  readonly signalNoConfidenceCard = this.page
    .getByRole("region")
    .locator("div")
    .filter({ hasText: "Signal No Confidence on Every" })
    .nth(2); // BUG: testId -> signal-no-confidence-card
  readonly abstainDelegationCard = this.page.getByText(
    "Abstain from Every VoteSelect this to vote ABSTAIN to every vote.Voting Power₳"
  ); // BUG: testId -> abstain-delegation-card

  readonly delegationErrorModal = this.page.getByTestId(
    "delegation-transaction-error-modal"
  );

  readonly delegateBtns = this.page.locator(
    '[data-testid$="-delegate-button"]'
  );

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(
      `${environments.frontendUrl}/connected/dRep_directory`
    );
  }

  @withTxConfirmation
  async delegateToDRep(dRepId: string) {
    await this.searchInput.fill(dRepId);
    const delegateBtn = this.page.getByTestId(`${dRepId}-delegate-button`);
    await expect(delegateBtn).toBeVisible();
    await this.page.getByTestId(`${dRepId}-delegate-button`).click();
    await this.searchInput.clear();
  }

  async resetDRepForm() {
    if (await this.delegationErrorModal.isVisible()) {
      await this.page.getByTestId("confirm-modal-button").click();
    }
    await this.dRepInput.clear();
  }
  async filterDReps(filterOptions: string[]) {
    for (const option of filterOptions) {
      await this.page.getByTestId(`${option}-checkbox`).click();
    }
  }

  async unFilterDReps(filterOptions: string[]) {
    for (const option of filterOptions) {
      await this.page.getByTestId(`${option}-checkbox`).click();
    }
  }

  async validateFilters(filters: string[], filterOptions: string[]) {
    const excludedFilters = filterOptions.filter(
      (filter) => !filters.includes(filter)
    );

    for (const filter of excludedFilters) {
      await expect(
        this.page.getByText(filter, { exact: true }),
        `Expected "${filter}" to be excluded, but it's included`
      ).toHaveCount(1);
    }

    for (const filter of filters) {
      expect(
        (await this.page.getByText(filter, { exact: true }).all(),
        `Expected to find "${filter}"`).length
      ).toBeGreaterThanOrEqual(0);
    }
  }

  async sortDRep(option: string) {}

  async sortAndValidate(
    option: string,
    validationFn: (p1: IDRep, p2: IDRep) => boolean
  ) {
    const responsePromise = this.page.waitForResponse((response) =>
      response.url().includes(`&sort=${option}`)
    );

    await this.page.getByTestId(`${option}-radio`).click();
    const response = await responsePromise;

    const dRepList: IDRep[] = (await response.json()).elements;

    // API validation
    for (let i = 0; i <= dRepList.length - 2; i++) {
      const isValid = validationFn(dRepList[i], dRepList[i + 1]);
      expect(isValid, "API Sorting validation failed").toBe(true);
    }

    // Frontend validation
    const dRepListFE = await this.getAllListedDRepIds();

    for (let i = 0; i <= dRepListFE.length - 1; i++) {
      expect(dRepListFE[i], "Frontend validation failed").toHaveText(
        dRepList[i].view
      );
    }
  }
  getDRepCard(dRepId: string) {
    return this.page.getByRole("list").getByTestId(`${dRepId}-copy-id-button`);
  }

  async getAllListedDRepIds() {
    await this.page.waitForTimeout(2_000);

    return await this.page
      .getByRole("list")
      .locator('[data-testid$="-copy-id-button"]')
      .all();
  }
}
