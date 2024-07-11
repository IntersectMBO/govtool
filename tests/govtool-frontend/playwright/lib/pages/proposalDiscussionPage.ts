import { faker } from "@faker-js/faker";
import { generateWalletAddress } from "@helpers/cardano";
import { extractProposalIdFromUrl } from "@helpers/string";
import { expect, Locator, Page } from "@playwright/test";
import { ProposalCreateRequest, ProposedGovAction } from "@types";
import environments from "lib/constants/environments";
import ProposalDiscussionDetailsPage from "./proposalDiscussionDetailsPage";

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
  readonly verifyIdentityBtn = this.page.getByRole("button", {
    name: "Verify your identity",
  }); // BUG
  readonly addLinkBtn = this.page.getByTestId("add-link-button");
  readonly infoRadio = this.page.getByTestId("Info-radio-wrapper");
  readonly treasuryRadio = this.page.getByTestId("Treasury-radio-wrapper");

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/proposal_discussion`);
    await this.page.waitForTimeout(2_000);
  }

  async closeUsernamePrompt() {
    await this.page.waitForTimeout(5_000);
    await this.page
      .locator("div")
      .filter({ hasText: /^Hey, setup your username$/ })
      .getByRole("button")
      .click();
  }

  async viewFirstProposal(): Promise<ProposalDiscussionDetailsPage> {
    await this.page
      .locator('[data-testid^="proposal-"][data-testid$="-view-details"]')
      .first()
      .click();
    return new ProposalDiscussionDetailsPage(this.page);
  }

  async getAllProposals() {
    await this.page.waitForTimeout(4_000); // waits for proposals to render

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

  async createProposal(): Promise<number> {
    const receivingAddr = generateWalletAddress();
    const proposalRequest: ProposalCreateRequest = {
      proposal_links: [
        {
          prop_link: faker.internet.url(),
          prop_link_text: faker.internet.displayName(),
        },
      ],
      gov_action_type_id: 1,
      prop_name: faker.company.name(),
      prop_abstract: faker.lorem.paragraph(2),
      prop_motivation: faker.lorem.paragraph(2),
      prop_rationale: faker.lorem.paragraph(2),
      prop_receiving_address: receivingAddr,
      prop_amount: faker.number.int({ min: 100, max: 1000 }).toString(),
      is_draft: false,
    };
    await this.proposalCreateBtn.click();
    await this.continueBtn.click();

    await this.fillForm(proposalRequest);

    await this.continueBtn.click();
    await this.page.getByTestId("submit-button").click();

    // Wait for redirection to `proposal-discussion-details` page
    await this.page.waitForTimeout(2_000);

    const currentPageUrl = this.page.url();
    return extractProposalIdFromUrl(currentPageUrl);
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

  async filterProposalByNames(names: string[]) {
    for (const name of names) {
      await this.page.getByLabel(name).click(); // test id is not in proper format for all filter type
    }
  }

  async unFilterProposalByNames(names: string[]) {
    for (const name of names) {
      await this.page.getByLabel(name).click(); // test id is not in proper format for all filter type
    }
  }

  async validateFilters(filters: string[]) {
    const proposalCards = await this.getAllProposals();

    for (const proposalCard of proposalCards) {
      const hasFilter = await this._validateFiltersInProposalCard(
        proposalCard,
        filters
      );
      expect(hasFilter).toBe(true);
    }
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

  async _validateFiltersInProposalCard(
    proposalCard: Locator,
    filters: string[]
  ): Promise<boolean> {
    const proposalTypeTextContent = await proposalCard
      .locator('[data-testid$="-type"]')
      .textContent();
    const govActionType = proposalTypeTextContent.split(":")[1];

    return filters.includes(govActionType);
  }
}
