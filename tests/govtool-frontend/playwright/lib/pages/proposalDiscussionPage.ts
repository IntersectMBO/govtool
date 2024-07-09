import { faker } from "@faker-js/faker";
import { generateWalletAddress } from "@helpers/cardano";
import { extractProposalIdFromUrl } from "@helpers/string";
import { expect, Locator, Page } from "@playwright/test";
import environments from "lib/constants/environments";
import ProposalDiscussionDetailsPage from "./proposalDiscussionDetailsPage";
import { ProposalCreateRequest, ProposedGovAction } from "@types";
import { range } from "cypress/types/lodash";

export default class ProposalDiscussionPage {
  // Buttons
  readonly proposalCreateBtn = this.page.getByRole("button", {
    name: "Propose a Governance Action",
  });
  readonly continueBtn = this.page.getByRole("button", { name: "Continue" }); // #BUG test-id missing
  readonly filterBtn = this.page.locator("#filters-button"); // this.page.getByTestId("filters-button");
  readonly shareBtn = this.page
    .locator(".MuiCardHeader-action > .MuiButtonBase-root")
    .first(); //this.page.getByTestId("share-button");
  readonly sortBtn = this.page.locator("button:nth-child(2)").first(); //this.page.getByTestId("sort-button");
  readonly searchInput = this.page.getByPlaceholder("Search..."); // this.page.getByTestId("search-input");
  readonly showAllBtn = this.page
    .getByRole("button", { name: "Show all" })
    .first(); //this.page.getByTestId("show-all-button");
  readonly showLessBtn = this.page.getByRole("button", { name: "Show less" });
  readonly infoRadio = this.page.getByLabel("Info");
  readonly treasuryRadio = this.page.getByLabel("Treasury");
  readonly verifyIdentityBtn = this.page.getByRole("button", {
    name: "Verify your identity",
  });
  readonly addLinkBtn = this.page.getByRole("button", { name: "Add link" });

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/proposal_discussion`);
    await this.page.waitForTimeout(2_000);
  }

  async closeUsernamePrompt() {
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
    // BUG  Select all elements with data-testid attribute
    const elements = await this.page.$$("[data-testid]");

    // Regex pattern to match IDs starting with "proposal" and ending with numbers
    const pattern = /^proposal-\d+$/;

    // Extract the data-testid attributes
    const proposalCards: Locator[] = [];
    for (const element of elements) {
      const dataTestId = await element.getAttribute("data-testid");
      if (pattern.test(dataTestId)) {
        proposalCards.push(this.page.getByTestId(dataTestId));
      }
    }

    return proposalCards;
    // BUG return this.page.locator('[data-testid$="-card"]').all();
  }

  async setUsername(name: string) {
    await this.page.getByLabel("Username *").fill(name);

    const proceedBtn = this.page.getByRole("button", {
      name: "Proceed with this username",
    });
    await proceedBtn.click();
    await proceedBtn.click();

    await this.page.getByRole("button", { name: "Close" }).click();
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
    await this.page.getByRole("button", { name: "Submit" }).click();

    // Wait for redirection to `proposal-discussion-details` page
    await this.page.waitForTimeout(2_000);

    const currentPageUrl = this.page.url();
    return extractProposalIdFromUrl(currentPageUrl);
  }

  private async fillForm(data: ProposalCreateRequest) {
    await this.page.getByLabel("Governance Action Type *").click();
    await this.page.getByRole("option", { name: "Info" }).click();
    await this.page.getByLabel("Title *").fill(data.prop_name);
    await this.page.getByPlaceholder("Summary...").fill(data.prop_abstract);
    await this.page.getByLabel("Motivation *").fill(data.prop_motivation);
    await this.page.getByLabel("Rationale *").fill(data.prop_rationale);

    for (const link of data.proposal_links) {
      await this.addLinkBtn.click();

      await this.page
        .getByPlaceholder("https://website.com")
        .fill(link.prop_link);
      await this.page.getByPlaceholder("Text").fill(link.prop_link_text);
    }
  }

  async filterProposalByNames(names: string[]) {
    for (const name of names) {
      await this.page.getByLabel(name).click();
    }
  }

  async unFilterProposalByNames(names: string[]) {
    for (const name of names) {
      await this.page.getByLabel(name).click();
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
