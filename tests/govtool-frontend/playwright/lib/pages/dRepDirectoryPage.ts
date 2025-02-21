import { convertDRepToCIP129 } from "@helpers/dRep";
import { functionWaitedAssert, waitedLoop } from "@helpers/waitedLoop";
import { Locator, Page, expect } from "@playwright/test";
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
  readonly abstainInfoButton = this.page.getByTestId(
    "abstain-from-every-vote-info-button"
  );

  readonly signalNoConfidenceInfoButton = this.page.getByTestId(
    "signal-no-confidence-on-every-vote-info-button"
  );

  readonly automaticDelegationOptionsDropdown = this.page.getByTestId(
    "automated-voting-options-accordion"
  );

  readonly delegateToDRepCard = this.page.getByTestId("delegate-to-drep-card");
  readonly signalNoConfidenceCard = this.page.getByTestId(
    "no-confidence-delegation-card"
  );
  readonly abstainDelegationCard = this.page.getByTestId(
    "abstain-delegation-card"
  );

  readonly delegationErrorModal = this.page.getByTestId(
    "delegate-transaction-error-modal"
  );

  readonly delegateBtns = this.page.locator(
    '[data-testid$="-delegate-button"]'
  );

  readonly NoDRepText = this.page.getByText("No DReps found");

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(
      `${environments.frontendUrl}/connected/drep_directory`
    );
  }

  @withTxConfirmation
  async delegateToDRep(dRepId: string) {
    await this.searchInput.fill(dRepId);
    const delegateBtn = this.page.getByTestId(`${dRepId}-delegate-button`);
    await expect(delegateBtn).toBeVisible();
    await this.page.getByTestId(`${dRepId}-delegate-button`).click();
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
    await functionWaitedAssert(async () => {
      const excludedFilters = filterOptions.filter(
        (filter) => !filters.includes(filter)
      );

      const dRepList = await this.getAllListedDReps();

      for (const filter of excludedFilters) {
        await expect(this.page.getByTestId(`${filter}-checkbox`)).toHaveCount(
          1
        );
      }

      for (const dRep of dRepList) {
        const hasFilter = await this._validateTypeFiltersInDRep(dRep, filters);
        const actualFilter = await dRep
          .locator('[data-testid$="-pill"]')
          .textContent();
        if (!hasFilter) {
          const errorMessage = `${actualFilter} pill does not match with any of the ${filters}`;
          throw new Error(errorMessage);
        }
        expect(hasFilter).toBe(true);
      }
    });
  }

  async _validateTypeFiltersInDRep(
    dRepCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    const dRepType = await dRepCard
      .locator('[data-testid$="-pill"]')
      .textContent();

    return filters.includes(dRepType);
  }

  async sortDRep(option: string) {}

  async getDRepsResponseFromApi(option: string): Promise<IDRep[]> {
    const responsePromise = this.page.waitForResponse((response) =>
      response.url().includes(`&sort=${option}`)
    );

    await this.page.getByTestId(`${option}-radio`).click();
    const response = await responsePromise;

    return (await response.json()).elements;
  }

  async sortAndValidate(
    option: string,
    validationFn: (p1: IDRep, p2: IDRep) => boolean
  ) {
    const dRepList = await this.getDRepsResponseFromApi(option);

    // API validation
    for (let i = 0; i <= dRepList.length - 2; i++) {
      const isValid = validationFn(dRepList[i], dRepList[i + 1]);
      expect(isValid).toBe(true);
    }
    // Frontend validation
    const cip105DRepListFE = await this.getAllListedCIP105DRepIds();
    const cip129DRepListFE = await this.getAllListedCIP129DRepIds();

    const cip129DRepListApi = dRepList.map((dRep) =>
      convertDRepToCIP129(dRep.drepId, dRep.isScriptBased)
    );

    for (let i = 0; i <= cip105DRepListFE.length - 1; i++) {
      await expect(cip129DRepListFE[i]).toHaveText(cip129DRepListApi[i]);
      await expect(cip105DRepListFE[i]).toHaveText(
        `(CIP-105) ${dRepList[i].view}`
      );
    }
  }
  getDRepCard(dRepId: string) {
    return this.page.getByTestId(`${dRepId}-drep-card`);
  }

  async getAllListedCIP105DRepIds() {
    const dRepCards = await this.getAllListedDReps();

    return dRepCards.map((dRep) =>
      dRep.locator('[data-testid$="-copy-id-button"]').last()
    );
  }

  async getAllListedCIP129DRepIds() {
    const dRepCards = await this.getAllListedDReps();

    return dRepCards.map((dRep) =>
      dRep.locator('[data-testid$="-copy-id-button"]').first()
    );
  }

  async getAllListedDReps() {
    await expect(this.searchInput).toBeVisible({ timeout: 10_000 });

    await waitedLoop(async () => {
      return (
        (await this.page.locator('[data-testid$="-drep-card"]').count()) > 0
      );
    });

    return this.page.locator('[data-testid$="-drep-card"]').all();
  }

  async verifyDRepInList(dRepId: string) {
    await this.goto();

    await this.searchInput.fill(dRepId);

    await expect(this.page.getByText("No DReps found")).toBeVisible({
      timeout: 20_000,
    });
  }
}
