import { expect, Locator, Page } from "@playwright/test";
import { ProposalCreateRequest, ProposalType, ProposedGovAction } from "@types";
import environments from "lib/constants/environments";
import ProposalDiscussionDetailsPage from "./proposalDiscussionDetailsPage";
import { functionWaitedAssert, waitedLoop } from "@helpers/waitedLoop";

export default class ProposalDiscussionPage {
  // Buttons
  readonly proposalCreateBtn = this.page.getByTestId(
    "propose-a-governance-action-button"
  );
  readonly continueBtn = this.page.getByTestId("continue-button");
  readonly filterBtn = this.page.getByTestId("filter-button");
  readonly sortBtn = this.page.getByTestId("sort-button");
  readonly searchInput = this.page.getByTestId("search-input");
  readonly showAllBtn = this.page.getByTestId("show-all-button").first(); //this.page.getByTestId("show-all-button");
  readonly verifyIdentityBtn = this.page.getByTestId("verify-identity-button");
  readonly addLinkBtn = this.page.getByTestId("add-link-button");
  readonly infoRadio = this.page.getByTestId("info action-radio-wrapper");
  readonly treasuryRadio = this.page.getByTestId(
    "treasury requests-radio-wrapper"
  );
  readonly activeProposalWrapper = this.page.getByTestId(
    "active-proposal-radio-wrapper"
  );

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/proposal_discussion`);
    // wait for the proposal cards to load
    await this.page.waitForTimeout(2_000);
  }

  async viewFirstProposal(): Promise<ProposalDiscussionDetailsPage> {
    await this.page
      .locator('[data-testid^="proposal-"][data-testid$="-view-details"]')
      .first()
      .click();
    return new ProposalDiscussionDetailsPage(this.page);
  }

  async getAllProposals() {
    await waitedLoop(async () => {
      return (
        (await this.page
          .locator('[data-testid^="proposal-"][data-testid$="-card"]')
          .count()) > 0
      );
    });

    return this.page
      .locator('[data-testid^="proposal-"][data-testid$="-card"]')
      .all();
  }

  async setUsername(name: string) {
    await this.page.getByTestId("username-input").fill(name);

    const proceedBtn = this.page.getByTestId("proceed-button");
    await proceedBtn.click();
    await proceedBtn.click();

    await this.page.getByTestId("close-button").click();
  }

  private async fillForm(data: ProposalCreateRequest) {
    await this.page.getByTestId("governance-action-type").click();
    await this.page.getByTestId("info-button").click();
    await this.page.getByTestId("title-input").fill(data.prop_name);
    await this.page.getByTestId("abstract-input").fill(data.prop_abstract);
    await this.page.getByTestId("motivation-input").fill(data.prop_motivation);
    await this.page.getByTestId("rationale-input").fill(data.prop_rationale);

    for (let index = 0; index < data.proposal_links.length; index++) {
      await this.addLinkBtn.click();

      await this.page
        .getByTestId(`link-${index}-url-input`)
        .fill(data.proposal_links[index].prop_link);
      await this.page
        .getByTestId(`link-${index}-text-input`)
        .fill(data.proposal_links[index].prop_link_text);
    }
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

  async clickRadioButtonsByNames(names: string[]) {
    for (const name of names) {
      const testId = Object.values(ProposalType).includes(name as ProposalType)
        ? name.toLowerCase()
        : name.toLowerCase().replace(/ /g, "-");
      await this.page.getByTestId(`${testId}-radio`).click();
    }
  }

  async filterProposalByNames(names: string[]) {
    await this.clickRadioButtonsByNames(names);
  }

  async unFilterProposalByNames(names: string[]) {
    await this.clickRadioButtonsByNames(names);
  }

  async validateFilters(
    filters: string[],
    validateFunction: (proposalCard: any, filters: string[]) => Promise<boolean>
  ) {
    await functionWaitedAssert(async () => {
      const proposalCards = await this.getAllProposals();

      for (const proposalCard of proposalCards) {
        if (await proposalCard.isVisible()) {
          const type = await proposalCard
            .getByTestId("governance-action-type")
            .textContent();
          const hasFilter = await validateFunction(proposalCard, filters);
          if (!hasFilter) {
            const errorMessage = `A governance action type ${type} does not contain on ${filters}`;
            throw errorMessage;
          }
          expect(hasFilter).toBe(true);
        }
      }
    });
  }

  async sortAndValidate(
    option: "asc" | "desc",
    validationFn: (p1: ProposedGovAction, p2: ProposedGovAction) => boolean
  ) {
    const responsePromise = this.page.waitForResponse((response) =>
      response
        .url()
        .includes(`&sort[createdAt]=${option}&populate[0]=proposal_links`)
    );

    await this.sortBtn.click();
    const response = await responsePromise;

    let proposals: ProposedGovAction[] = (await response.json()).data;

    // API validation
    for (let i = 0; i <= proposals.length - 2; i++) {
      const isValid = validationFn(proposals[i], proposals[i + 1]);
      expect(isValid).toBe(true);
    }
  }

  async _validateTypeFiltersInProposalCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    const govActionType = await proposalCard
      .getByTestId("governance-action-type")
      .textContent();

    return filters.includes(govActionType);
  }

  async _validateStatusFiltersInProposalCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    let govActionType = await proposalCard
      .locator('[data-testid^="proposal-"][data-testid$="-status"]')
      .textContent();
    if (govActionType === "Active") {
      govActionType = "Active proposal";
    }

    return filters.includes(govActionType);
  }
}
