import environments from "@constants/environments";
import { outcomeStatusType } from "@constants/index";
import { toCamelCase } from "@helpers/string";
import { functionWaitedAssert, waitedLoop } from "@helpers/waitedLoop";
import { Browser, expect, Locator, Page } from "@playwright/test";
import { outcomeMetadata, outcomeProposal, outcomeType } from "@types";
import OutcomeDetailsPage from "./outcomeDetailsPage";
import { isMobile } from "@helpers/mobile";
import extractExpiryDateFromText from "@helpers/extractExpiryDateFromText";
import { createNewPageWithWallet, injectLogger } from "@helpers/page";
import { createTempUserAuth } from "@datafactory/createAuth";
import { user01Wallet } from "@constants/staticWallets";
import { user01AuthFile } from "@constants/auth";

const status = ["Expired", "Ratified", "Enacted", "Live"];

enum SortOption {
  SoonToExpire = "Soon to expire",
  NewestFirst = "Newest first",
  OldestFirst = "Oldest first",
  HighestAmountYesVote = "Highest amount of yes votes",
}

export default class OutComesPage {
  // Buttons
  readonly filterBtn = this.page.getByTestId("filters-button");
  readonly sortBtn = this.page.getByTestId("sort-button");
  readonly showMoreBtn = this.page.getByTestId("show-more-button");
  readonly metadataErrorLearnMoreBtn = this.page.getByTestId(
    "metadata-error-learn-more"
  );

  //inputs
  readonly searchInput = this.page.getByTestId("search-input");

  readonly title = this.page.getByTestId("single-action-title");

  constructor(private readonly page: Page) {}

  async goto(params: { filter?: string; sort?: string } = {}): Promise<void> {
    const { filter, sort = "newestFirst" } = params;
    const url = new URL(`${environments.frontendUrl}/outcomes`);
    url.searchParams.append("sort", sort);
    if (filter) {
      url.searchParams.append("type", filter);
    }
    await this.page.goto(url.toString());
  }

  async getAllListedCIP105GovernanceIds(): Promise<string[]> {
    const dRepCards = await this.getAllOutcomes();
    const dRepIds = [];

    for (const dRep of dRepCards) {
      const dRepIdTextContent = await dRep
        .locator('[data-testid$="-CIP-105-id"]')
        .textContent();
      dRepIds.push(dRepIdTextContent.replace(/^.*ID/, ""));
    }

    return dRepIds;
  }

  async viewFirstOutcomes(): Promise<OutcomeDetailsPage> {
    await this.page.locator('[data-testid$="-view-details"]').first().click();
    return new OutcomeDetailsPage(this.page);
  }

  async getAllOutcomes(): Promise<Locator[]> {
    await waitedLoop(async () => {
      return (
        (await this.page.locator('[data-testid$="-outcome-card"]').count()) >
          0 ||
        (await this.page.getByText("No governance actions found").isVisible())
      );
    });
    return await this.page.locator('[data-testid$="-outcome-card"]').all();
  }

  async clickCheckboxByNames(names: string[]) {
    const formattedNames = names.map((name) =>
      name === "Info Action" ? "Info" : name
    );
    for (const name of formattedNames) {
      const testId = name.toLowerCase().replace(/ /g, "-");
      await this.page.getByTestId(`${testId}-checkbox`).click();
    }
  }

  async filterProposalByNames(names: string[]) {
    await this.clickCheckboxByNames(names);
  }

  async unFilterProposalByNames(names: string[]) {
    await this.clickCheckboxByNames(names);
  }

  async applyAndValidateFilters(
    filters: string[],
    validateFunction: (proposalCard: any, filters: string[]) => Promise<boolean>
  ) {
    await this.page.waitForTimeout(4_000); // wait for the proposals to load
    // single filter
    for (const filter of filters) {
      await this.filterProposalByNames([filter]);
      await this.validateFilters([filter], validateFunction);
      await this.unFilterProposalByNames([filter]);
    }

    // multiple filter
    const multipleFilters = [...filters];
    while (multipleFilters.length > 1) {
      await this.filterProposalByNames(multipleFilters);
      await this.validateFilters(multipleFilters, validateFunction);
      await this.unFilterProposalByNames(multipleFilters);
      multipleFilters.pop();
    }
  }

  async validateFilters(
    filters: string[],
    validateFunction: (proposalCard: any, filters: string[]) => Promise<boolean>
  ) {
    await functionWaitedAssert(
      async () => {
        const proposalCards = await this.getAllOutcomes();
        for (const proposalCard of proposalCards) {
          if (await proposalCard.isVisible()) {
            const type = await proposalCard
              .locator('[data-testid$="-type"]')
              .textContent();
            const outcomeType = type.replace(/^.*Type/, "");
            const hasFilter = await validateFunction(proposalCard, filters);
            if (!hasFilter) {
              const errorMessage = `An outcome type ${outcomeType} does not contain on ${filters}`;
              throw errorMessage;
            }
            expect(hasFilter).toBe(true);
          }
        }
      },
      {
        name: "validateFilters",
      }
    );
  }

  getSortType(sortOption: string) {
    let sortType = sortOption;
    if (sortOption === "Highest amount of yes votes") {
      sortType = "Highest yes votes";
    }
    return toCamelCase(sortType);
  }

  getSortTestId(sortOption: string) {
    const sortType = this.getSortType(sortOption);
    return sortType.toLowerCase().replace(/[\s.]/g, "") + "-radio";
  }

  async sortAndValidate(
    sortOption: string,
    validationFn: (p1: outcomeProposal, p2: outcomeProposal) => boolean,
    filterKey?: string
  ) {
    const sortType = this.getSortType(sortOption);
    const responsePromise = this.page.waitForResponse((response) =>
      response
        .url()
        .includes(
          filterKey
            ? `&filters=${filterKey}&sort=${sortType}`
            : `&sort=${sortType}`
        )
    );

    await this.page.getByTestId(this.getSortTestId(sortOption)).click();

    const response = await responsePromise;
    const data = await response.json();
    let outcomeProposalList: outcomeProposal[] = data.length != 0 ? data : null;

    // API validation
    if (outcomeProposalList.length <= 1) return;

    for (let i = 0; i <= outcomeProposalList.length - 2; i++) {
      const isValid = validationFn(
        outcomeProposalList[i],
        outcomeProposalList[i + 1]
      );
      expect(isValid).toBe(true);
    }

    await expect(
      this.page.getByRole("progressbar").getByRole("img")
    ).toBeHidden({ timeout: 20_000 });

    await functionWaitedAssert(
      async () => {
        const outcomeCards = await this.getAllOutcomes();
        for (const [index, outcomeCard] of outcomeCards.entries()) {
          const outcomeProposalFromAPI = outcomeProposalList[index];
          const proposalTypeFromUI = await outcomeCard
            .locator('[data-testid$="-type"]')
            .textContent();
          const proposalTypeFromApi = outcomeType[outcomeProposalFromAPI.type];

          const cip105IdFromUI = await outcomeCard
            .locator('[data-testid$="-CIP-105-id"]')
            .textContent();
          const cip105IdFromApi = `${outcomeProposalFromAPI.tx_hash}#${outcomeProposalFromAPI.index}`;

          expect(proposalTypeFromUI.replace(/^.*Type/, "")).toContain(
            proposalTypeFromApi
          );

          expect(cip105IdFromUI.replace(/^.*ID/, "")).toContain(
            cip105IdFromApi
          );
        }
      },
      {
        name: `frontend sort validation of ${sortOption} and filter ${filterKey}`,
      }
    );
  }

  async _validateFiltersInOutcomeCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    const type = await proposalCard
      .locator('[data-testid$="-type"]')
      .textContent();
    const outcomeType = type.replace(/^.*Type/, "");
    return filters.includes(outcomeType);
  }

  async _validateStatusFiltersInOutcomeCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    const status = await proposalCard
      .locator('[data-testid$="-status"]')
      .textContent();
    const outcomeStatus = outcomeStatusType.filter((statusType) => {
      if (statusType === "Live") {
        return "In Progress";
      }
      return status.includes(statusType);
    });
    return outcomeStatus.some((status) => filters.includes(status));
  }

  async shouldAccessPage() {
    await this.page.goto("/");

    if (isMobile(this.page)) {
      await this.page.getByTestId("open-drawer-button").click();
    }
    await this.page.getByTestId("governance-actions-outcomes-link").click();

    await expect(this.page.getByText(/outcomes/i)).toHaveCount(2);
  }

  async filterOutcomes() {
    await this.filterBtn.click();
    const filterOptionNames = Object.values(outcomeType);

    // proposal type filter
    await this.applyAndValidateFilters(
      filterOptionNames,
      this._validateFiltersInOutcomeCard
    );

    // proposal status filter
    await this.applyAndValidateFilters(
      status,
      this._validateStatusFiltersInOutcomeCard
    );
  }

  async sortOutcomes() {
    await this.sortBtn.click();

    await this.sortAndValidate(
      SortOption.NewestFirst,
      (p1, p2) => p1.expiry_date >= p2.time
    );

    await this.sortAndValidate(
      SortOption.OldestFirst,
      (p1, p2) => p1.expiry_date <= p2.expiry_date
    );

    await this.sortAndValidate(
      SortOption.HighestAmountYesVote,
      (p1, p2) => parseInt(p1.yes_votes) >= parseInt(p2.yes_votes)
    );
  }

  async filterAndSortOutcomes() {
    const filterOptionKeys = Object.keys(outcomeType);
    const filterOptionNames = Object.values(outcomeType);

    const choice = Math.floor(Math.random() * filterOptionKeys.length);
    await this.goto({ filter: filterOptionKeys[choice] });
    await this.sortBtn.click();

    await this.sortAndValidate(
      SortOption.OldestFirst,
      (p1, p2) => p1.expiry_date <= p2.expiry_date
    );

    await this.validateFilters(
      [filterOptionNames[choice]],
      this._validateFiltersInOutcomeCard
    );
  }

  async verifyAllOutcomesAreExpired() {
    const proposalCards = await this.getAllOutcomes();

    for (const proposalCard of proposalCards) {
      const expiryDateEl = proposalCard.locator(
        '[data-testid$="-Expired-date"]'
      );
      const expiryDateTxt = await expiryDateEl.innerText();
      const expiryDate = extractExpiryDateFromText(expiryDateTxt);
      const today = new Date();
      expect(today >= expiryDate).toBeTruthy();
    }
  }

  async VerifyLoadMoreOutcomes() {
    const responsePromise = this.page.waitForResponse((response) =>
      response
        .url()
        .includes(`governance-actions?search=&filters=&sort=newestFirst&page=2`)
    );
    await this.goto();

    let governanceActionIdsBefore: String[];
    let governanceActionIdsAfter: String[];

    await functionWaitedAssert(
      async () => {
        governanceActionIdsBefore =
          await this.getAllListedCIP105GovernanceIds();
        await this.showMoreBtn.click();
      },
      { message: "Show more button not visible" }
    );

    const response = await responsePromise;
    const governanceActionListAfter = await response.json();

    await functionWaitedAssert(
      async () => {
        governanceActionIdsAfter = await this.getAllListedCIP105GovernanceIds();
        expect(governanceActionIdsAfter.length).toBeGreaterThan(
          governanceActionIdsBefore.length
        );
      },
      { message: "Outcomes not loaded after clicking show more" }
    );

    if (governanceActionListAfter.length >= governanceActionIdsBefore.length) {
      await expect(this.showMoreBtn).toBeVisible();
      expect(true).toBeTruthy();
    } else {
      await expect(this.showMoreBtn).not.toBeVisible();
    }
  }

  async fetchOutcomeIdFromNetwork(governanceActionId: string) {
    let updatedGovernanceActionId = governanceActionId;
    await this.page.route(
      "**/governance-actions?search=&filters=&sort=**",
      async (route) => {
        const response = await route.fetch();
        const data: outcomeProposal[] = await response.json();
        if (!governanceActionId) {
          if (data.length > 0) {
            const randomIndexForId = Math.floor(Math.random() * data.length);
            updatedGovernanceActionId =
              data[randomIndexForId].tx_hash +
              "#" +
              data[randomIndexForId].index;
          }
        }
        await route.fulfill({
          status: 200,
          contentType: "application/json",
          body: JSON.stringify(data),
        });
      }
    );

    const responsePromise = this.page.waitForResponse(
      "**/governance-actions?search=&filters=&sort=**"
    );
    await this.goto();
    await responsePromise;
    return updatedGovernanceActionId;
  }

  async fetchOutcomeTitleFromNetwork(governanceActionTitle: string) {
    let updatedGovernanceActionTitle = governanceActionTitle;
    await this.page.route(
      "**/governance-actions/metadata?**",
      async (route) => {
        try {
          const response = await route.fetch();
          if (response.status() !== 200) {
            await route.continue();
            return;
          }
          const data: outcomeMetadata = await response.json();
          if (!governanceActionTitle && data.data.title != null) {
            updatedGovernanceActionTitle = data.data.title;
          }
          await route.fulfill({
            status: 200,
            contentType: "application/json",
            body: JSON.stringify(data),
          });
        } catch (error) {
          return;
        }
      }
    );
    await this.goto();
    const metadataResponsePromise = this.page.waitForResponse(
      "**/governance-actions/metadata?**"
    );
    await metadataResponsePromise;
    return updatedGovernanceActionTitle;
  }

  async searchOutcomesById(governanceActionId: string) {
    await this.searchInput.fill(governanceActionId);
    await expect(
      this.page.getByRole("progressbar").getByRole("img")
    ).toBeVisible();

    await functionWaitedAssert(
      async () => {
        const idSearchOutcomeCards = await this.getAllOutcomes();
        expect(idSearchOutcomeCards.length, {
          message:
            idSearchOutcomeCards.length == 0 && "No governance actions found",
        }).toBeGreaterThan(0);
        for (const outcomeCard of idSearchOutcomeCards) {
          const id = await outcomeCard
            .locator('[data-testid$="-CIP-105-id"]')
            .textContent();
          expect(id.replace(/^.*ID/, "")).toContain(governanceActionId);
        }
      },
      { name: "search by id" }
    );
  }

  async searchOutcomesByTitle(governanceActionTitle: string) {
    await this.searchInput.fill(governanceActionTitle);
    await expect(
      this.page.getByRole("progressbar").getByRole("img")
    ).toBeVisible();

    await functionWaitedAssert(
      async () => {
        const titleSearchOutcomeCards = await this.getAllOutcomes();
        expect(titleSearchOutcomeCards.length, {
          message:
            titleSearchOutcomeCards.length == 0 &&
            "No governance actions found",
        }).toBeGreaterThan(0);
        for (const outcomeCard of titleSearchOutcomeCards) {
          const title = await outcomeCard
            .locator('[data-testid$="-card-title"]')
            .textContent();
          expect(title.toLowerCase()).toContain(
            governanceActionTitle.toLowerCase()
          );
        }
      },
      { name: "search by title" }
    );
  }

  async shouldCopyGovernanceActionId(governanceActionId: string) {
    await this.searchInput.fill(governanceActionId);

    await this.page
      .getByTestId(`${governanceActionId}-CIP-105-id`)
      .getByTestId("copy-button")
      .click();
    await expect(this.page.getByText("Copied to clipboard")).toBeVisible({
      timeout: 60_000,
    });
    const copiedTextDRepDirectory = await this.page.evaluate(() =>
      navigator.clipboard.readText()
    );
    expect(copiedTextDRepDirectory).toEqual(governanceActionId);
  }

  async navigateToFilteredProposalDetail(
    browser: Browser,
    filterKey: string,
    isLoggedIn: boolean
  ) {
    let page: Page;
    if (!isLoggedIn) {
      page = await browser.newPage();
    } else {
      page = await createNewPageWithWallet(browser, {
        storageState: user01AuthFile,
        wallet: user01Wallet,
      });
    }
    injectLogger(page);

    const outcomeListResponsePromise = page.waitForResponse(
      (response) =>
        response
          .url()
          .includes(`governance-actions?search=&filters=${filterKey}`),
      { timeout: 60_000 }
    );

    const metricsResponsePromise = page.waitForResponse(
      (response) => response.url().includes(`/misc/network/metrics?epoch`),
      { timeout: 60_000 }
    );

    const outcomePage = new OutComesPage(page);
    await outcomePage.goto({ filter: filterKey });

    const outcomeListResponse = await outcomeListResponsePromise;
    const proposals = await outcomeListResponse.json();

    expect(
      proposals.length,
      proposals.length == 0 && "No proposals found!"
    ).toBeGreaterThan(0);

    const { index: governanceActionIndex, tx_hash: governanceTransactionHash } =
      proposals[0];

    const outcomeResponsePromise = page.waitForResponse(
      (response) =>
        response
          .url()
          .includes(
            `governance-actions/${governanceTransactionHash}?index=${governanceActionIndex}`
          ),
      { timeout: 60_000 }
    );

    const govActionDetailsPage = await outcomePage.viewFirstOutcomes();
    return {
      govActionDetailsPage,
      outcomeResponsePromise,
      metricsResponsePromise,
    };
  }
}
