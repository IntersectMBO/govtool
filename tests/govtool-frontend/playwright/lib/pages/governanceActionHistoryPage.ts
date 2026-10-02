import environments from "@constants/environments";
import { actionRecordStatusType } from "@constants/index";
import { toCamelCase } from "@helpers/string";
import { functionWaitedAssert, waitedLoop } from "@helpers/waitedLoop";
import { Browser, expect, Locator, Page } from "@playwright/test";
import { actionRecordMetadata, actionRecordProposal, actionRecordType } from "@types";
import GovernanceActionHistoryDetailsPage from "./governanceActionHistoryDetailsPage";
import { isMobile } from "@helpers/mobile";
import extractExpiryDateFromText from "@helpers/extractExpiryDateFromText";
import { createNewPageWithWallet, injectLogger } from "@helpers/page";
import { testWallet } from "lib/wallet/testWallets";

const status = ["Expired", "Ratified", "Enacted", "Live"];

enum SortOption {
  SoonToExpire = "Soon to expire",
  NewestFirst = "Newest first",
  OldestFirst = "Oldest first",
  HighestAmountYesVote = "Highest amount of yes votes",
}

// Card reads run inside retry loops: a filter or sort change can replace
// the cards between counting and reading them, and an unbounded read then
// waits for a card that is gone until the test times out.
const CARD_READ_TIMEOUT = 10_000;

export default class GovernanceActionHistoryPage {
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

  async goto(
    params: { filter?: string; sort?: string; status?: string } = {}
  ): Promise<void> {
    const { filter, sort = "newestFirst", status } = params;
    const url = new URL(`${environments.frontendUrl}/governance_actions/history`);
    url.searchParams.append("sort", sort);
    if (filter) {
      url.searchParams.append("type", filter);
    }
    if (status) {
      url.searchParams.append("status", status);
    }
    await this.page.goto(url.toString());
  }

  async getAllListedCIP105GovernanceIds(): Promise<string[]> {
    const dRepCards = await this.getAllGovernanceActionHistory();
    const dRepIds = [];

    for (const dRep of dRepCards) {
      const dRepIdTextContent = await dRep
        .locator('[data-testid$="-CIP-105-id"]')
        .textContent({ timeout: CARD_READ_TIMEOUT });
      dRepIds.push(dRepIdTextContent.replace(/^.*ID/, ""));
    }

    return dRepIds;
  }

  async viewFirstGovernanceActionHistory(): Promise<GovernanceActionHistoryDetailsPage> {
    await this.page.locator('[data-testid$="-view-details"]').first().click();
    return new GovernanceActionHistoryDetailsPage(this.page);
  }

  async getAllGovernanceActionHistory(): Promise<Locator[]> {
    await waitedLoop(async () => {
      return (
        (await this.page.locator('[data-testid$="-governance-action-card"]').count()) >
          0 ||
        (await this.page.getByText("No governance actions found").isVisible())
      );
    });
    return await this.page.locator('[data-testid$="-governance-action-card"]').all();
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
        const proposalCards = await this.getAllGovernanceActionHistory();
        for (const proposalCard of proposalCards) {
          if (await proposalCard.isVisible()) {
            const type = await proposalCard
              .locator('[data-testid$="-type"]')
              .textContent({ timeout: CARD_READ_TIMEOUT });
            const actionRecordType = type.replace(/^.*Type/, "");
            const hasFilter = await validateFunction(proposalCard, filters);
            if (!hasFilter) {
              const errorMessage = `An actionRecord type ${actionRecordType} does not contain on ${filters}`;
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
    validationFn: (p1: actionRecordProposal, p2: actionRecordProposal) => boolean,
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
    const actionRecordProposalList: actionRecordProposal[] = await response.json();

    // API validation
    if (actionRecordProposalList.length <= 1) return;

    for (let i = 0; i <= actionRecordProposalList.length - 2; i++) {
      const isValid = validationFn(
        actionRecordProposalList[i],
        actionRecordProposalList[i + 1]
      );
      expect(isValid).toBe(true);
    }

    await expect(
      this.page.getByRole("progressbar").getByRole("img")
    ).toBeHidden({ timeout: 20_000 });

    await functionWaitedAssert(
      async () => {
        const actionRecordCards = await this.getAllGovernanceActionHistory();
        for (const [index, actionRecordCard] of actionRecordCards.entries()) {
          const actionRecordProposalFromAPI = actionRecordProposalList[index];
          const proposalTypeFromUI = await actionRecordCard
            .locator('[data-testid$="-type"]')
            .textContent({ timeout: CARD_READ_TIMEOUT });
          const proposalTypeFromApi = actionRecordType[actionRecordProposalFromAPI.type];

          const cip105IdFromUI = await actionRecordCard
            .locator('[data-testid$="-CIP-105-id"]')
            .textContent({ timeout: CARD_READ_TIMEOUT });
          const cip105IdFromApi = `${actionRecordProposalFromAPI.tx_hash}#${actionRecordProposalFromAPI.index}`;

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

  async _validateFiltersInGovernanceActionHistoryCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    const type = await proposalCard
      .locator('[data-testid$="-type"]')
      .textContent({ timeout: CARD_READ_TIMEOUT });
    const actionRecordType = type.replace(/^.*Type/, "");
    return filters.includes(actionRecordType);
  }

  async _validateStatusFiltersInGovernanceActionHistoryCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    const status = await proposalCard
      .locator('[data-testid$="-status"]')
      .textContent({ timeout: CARD_READ_TIMEOUT });
    const actionRecordStatus = actionRecordStatusType.filter((statusType) => {
      if (statusType === "Live") {
        return "In Progress";
      }
      return status.includes(statusType);
    });
    return actionRecordStatus.some((status) => filters.includes(status));
  }

  async shouldAccessPage(isLoggedIn = false) {
    await this.page.goto("/");

    if (isMobile(this.page)) {
      await this.page.getByTestId("open-drawer-button").click();
    } else {
      if (!isLoggedIn) {
        await this.page.getByTestId("governance-actions").click();
      }
    }
    await this.page.getByTestId("governance-actions-governance-actions-link").click();

    if (!isMobile(this.page) && !isLoggedIn) {
      await this.page.getByTestId("governance-actions").click();
    }

    await expect(this.page.getByText(/Governance action history/i)).toHaveCount(2);
  }

  async filterGovernanceActionHistory() {
    await this.filterBtn.click();
    const filterOptionNames = Object.values(actionRecordType);

    // proposal type filter
    await this.applyAndValidateFilters(
      filterOptionNames,
      this._validateFiltersInGovernanceActionHistoryCard
    );

    // proposal status filter
    await this.applyAndValidateFilters(
      status,
      this._validateStatusFiltersInGovernanceActionHistoryCard
    );
  }

  async sortGovernanceActionHistory() {
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

  async filterAndSortGovernanceActionHistory() {
    const filterOptionKeys = Object.keys(actionRecordType);
    const filterOptionNames = Object.values(actionRecordType);

    const choice = Math.floor(Math.random() * filterOptionKeys.length);
    await this.goto({ filter: filterOptionKeys[choice] });
    await this.sortBtn.click();

    await this.sortAndValidate(
      SortOption.OldestFirst,
      (p1, p2) => p1.expiry_date <= p2.expiry_date
    );

    await this.validateFilters(
      [filterOptionNames[choice]],
      this._validateFiltersInGovernanceActionHistoryCard
    );
  }

  // Cards show the date that ended the action (Enacted, Not Ratified,
  // Expired) or, while live, its expiry, so only the expired filter can
  // promise an "Expired" date on every card.
  async verifyAllGovernanceActionHistoryAreExpired() {
    await this.goto({ status: "expired" });
    const proposalCards = await this.getAllGovernanceActionHistory();

    for (const proposalCard of proposalCards) {
      const expiryDateEl = proposalCard.locator(
        '[data-testid$="-Expired-date"]'
      );
      await expect(expiryDateEl).toBeVisible();
      // e.g. "Expired: Thu Oct 01, 2026 (Epoch 2883)"
      const match = (await expiryDateEl.innerText()).match(
        /(\w{3}) (\d{1,2}), (\d{4})/
      );
      expect(match, "expired date is not readable").not.toBeNull();
      const expiryDate = new Date(`${match[1]} ${match[2]}, ${match[3]}`);
      expect(new Date() >= expiryDate).toBeTruthy();
    }
  }

  async VerifyLoadMoreGovernanceActionHistory() {
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
      { message: "GovernanceActionHistory not loaded after clicking show more" }
    );

    if (governanceActionListAfter.length >= governanceActionIdsBefore.length) {
      await expect(this.showMoreBtn).toBeVisible();
      expect(true).toBeTruthy();
    } else {
      await expect(this.showMoreBtn).not.toBeVisible();
    }
  }

  async fetchGovernanceActionIdAndTitleFromNetwork(
    governanceActionId?: string,
    governanceActionTitle?: string
  ): Promise<{ governanceActionId: string; governanceActionTitle: string }> {
    let updatedGovernanceActionId = governanceActionId;
    let updatedGovernanceActionTitle = governanceActionTitle;

    await this.page.route(
      "**/governance-actions?search=&filters=&sort=**",
      async (route) => {
        const response = await route.fetch();
        const data: actionRecordProposal[] = await response.json();

        if (!updatedGovernanceActionId && data.length > 0) {
          const randomIndex = Math.floor(Math.random() * data.length);
          updatedGovernanceActionId = `${data[randomIndex].tx_hash}#${data[randomIndex].index}`;
        }

        if (!updatedGovernanceActionTitle) {
          const itemWithTitle = data.find((item) => item.title != null);
          if (itemWithTitle) {
            updatedGovernanceActionTitle = itemWithTitle.title;
          }
        }

        await route.fulfill({
          status: 200,
          contentType: "application/json",
          body: JSON.stringify(data),
        });
      }
    );

    await this.page.route(
      "**/governance-actions/metadata?**",
      async (route) => {
        try {
          const response = await route.fetch();
          if (response.status() !== 200) {
            await route.continue();
            return;
          }

          const data: actionRecordMetadata = await response.json();
          if (!updatedGovernanceActionTitle && data.data.title) {
            updatedGovernanceActionTitle = data.data.title;
          }

          await route.fulfill({
            status: 200,
            contentType: "application/json",
            body: JSON.stringify(data),
          });
        } catch (error) {
          // Just return without handling the error
          return;
        }
      }
    );

    const actionsResponsePromise = this.page.waitForResponse(
      "**/governance-actions?search=&filters=&sort=**"
    );

    await this.goto();
    await actionsResponsePromise;

    const needMetadataForTitle = !updatedGovernanceActionTitle;
    const metadataResponsePromise = needMetadataForTitle
      ? this.page.waitForResponse("**/governance-actions/metadata?**")
      : Promise.resolve(null);
    if (needMetadataForTitle) {
      await metadataResponsePromise;
    }

    return {
      governanceActionId: updatedGovernanceActionId,
      governanceActionTitle: updatedGovernanceActionTitle,
    };
  }

  async searchGovernanceActionHistoryById(governanceActionId: string) {
    await this.searchInput.fill(governanceActionId);

    try {
      await expect(
        this.page.getByRole("progressbar").getByRole("img")
      ).toBeVisible();
    } catch (error) {
      // Handle the case where the progress bar is not visible
      console.warn("Progress bar not visible, proceeding with search.");
    }
    await functionWaitedAssert(
      async () => {
        const idSearchGovernanceActionHistoryCards = await this.getAllGovernanceActionHistory();
        expect(idSearchGovernanceActionHistoryCards.length, {
          message:
            idSearchGovernanceActionHistoryCards.length == 0 && "No governance actions found",
        }).toBeGreaterThan(0);
        for (const actionRecordCard of idSearchGovernanceActionHistoryCards) {
          const id = await actionRecordCard
            .locator('[data-testid$="-CIP-105-id"]')
            .textContent({ timeout: CARD_READ_TIMEOUT });
          expect(id.replace(/^.*ID/, "")).toContain(governanceActionId);
        }
      },
      { name: "search by id" }
    );
  }

  async searchGovernanceActionHistoryByTitle(governanceActionTitle: string) {
    await this.searchInput.fill(governanceActionTitle);
    try {
      await expect(
        this.page.getByRole("progressbar").getByRole("img")
      ).toBeVisible();
    } catch (error) {
      // Handle the case where the progress bar is not visible
      console.warn("Progress bar not visible, proceeding with search.");
    }

    await functionWaitedAssert(
      async () => {
        const titleSearchGovernanceActionHistoryCards = await this.getAllGovernanceActionHistory();
        expect(titleSearchGovernanceActionHistoryCards.length, {
          message:
            titleSearchGovernanceActionHistoryCards.length == 0 &&
            "No governance actions found",
        }).toBeGreaterThan(0);
        for (const actionRecordCard of titleSearchGovernanceActionHistoryCards) {
          const title = await actionRecordCard
            .locator('[data-testid$="-card-title"]')
            .textContent({ timeout: CARD_READ_TIMEOUT });
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
        wallet: await testWallet("user01"),
      });
    }
    injectLogger(page);

    const actionRecordListResponsePromise = page.waitForResponse(
      (response) =>
        response
          .url()
          .includes(`governance-actions?search=&filters=${filterKey}`),
      { timeout: 60_000 }
    );

    const actionRecordPage = new GovernanceActionHistoryPage(page);
    await actionRecordPage.goto({ filter: filterKey });

    const actionRecordListResponse = await actionRecordListResponsePromise;
    const proposals = await actionRecordListResponse.json();

    if (proposals.length === 0) {
      expect(true, "No proposals found!").toBeTruthy();
      return {
        govActionDetailsPage: null,
        actionRecordResponsePromise: null,
      };
    }

    expect(
      proposals.length,
      proposals.length == 0 && "No proposals found!"
    ).toBeGreaterThan(0);

    const { index: governanceActionIndex, tx_hash: governanceTransactionHash } =
      proposals[0];

    const actionRecordResponsePromise = page.waitForResponse(
      (response) =>
        response
          .url()
          .includes(
            `governance-actions/${governanceTransactionHash}?index=${governanceActionIndex}`
          ),
      { timeout: 120_000 }
    );

    const govActionDetailsPage = await actionRecordPage.viewFirstGovernanceActionHistory();
    return {
      govActionDetailsPage,
      actionRecordResponsePromise,
    };
  }
}
