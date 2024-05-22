import { faker } from "@faker-js/faker";
import { expectWithInfo } from "@helpers/exceptionHandler";
import { Logger } from "@helpers/logger";
import { downloadMetadata } from "@helpers/metadata";
import { invalid } from "@mock/index";
import { Download, Page, expect } from "@playwright/test";
import metadataBucketService from "@services/metadataBucketService";
import { IProposalForm, ProposalType } from "@types";
import environments from "lib/constants/environments";
const formErrors = {
  proposalTitle: ["max-80-characters-error", "this-field-is-required-error"],
  abstract: "this-field-is-required-error",
  motivation: "this-field-is-required-error",
  Rationale: "this-field-is-required-error",
  receivingAddress: "invalid-bech32-address-error",
  amount: ["only-number-is-allowed-error", "this-field-is-required-error"],
  link: "invalid-url-error",
};

export default class ProposalSubmissionPage {
  // modals
  readonly registrationSuccessModal = this.page.getByTestId(
    "create-governance-action-submitted-modal"
  );
  readonly registrationErrorModal = this.page.getByTestId(
    "create-governance-action-error-modal"
  );

  // buttons
  readonly registerBtn = this.page.getByTestId("register-button");
  readonly skipBtn = this.page.getByTestId("skip-button");
  readonly confirmBtn = this.page.getByTestId("confirm-modal-button");

  readonly continueBtn = this.page.getByTestId("continue-button");
  readonly addLinkBtn = this.page.getByRole("button", { name: "+ Add link" }); // BUG testid= add-link-button
  readonly infoRadioButton = this.page.getByTestId("Info-radio");
  readonly treasuryRadioButton = this.page.getByTestId("Treasury-radio");

  // input fields
  readonly titleInput = this.page.getByPlaceholder("A name for this Action"); // BUG testid = title-input
  readonly abstractInput = this.page.getByPlaceholder("Summary"); // BUG testid = abstract-input
  readonly motivationInput = this.page.getByPlaceholder(
    "Problem this GA will solve"
  ); // BUG testid = motivation-input
  readonly rationaleInput = this.page.getByPlaceholder(
    "Content of Governance Action"
  ); // BUG testid = rationale-input
  readonly linkInput = this.page.getByPlaceholder("https://website.com/"); // BUG testid = link-input
  readonly receivingAddressInput = this.page.getByPlaceholder(
    "The address to receive funds"
  );
  readonly amountInput = this.page.getByPlaceholder("e.g.");

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(
      `${environments.frontendUrl}/create_governance_action`
    );
    await this.continueBtn.click();
  }

  async register(governanceProposal: IProposalForm) {
    await this.fillupForm(governanceProposal);

    await this.continueBtn.click();
    await this.continueBtn.click();
    await this.page.getByRole("checkbox").click();
    await this.continueBtn.click();

    this.page
      .getByRole("button", { name: `${governanceProposal.type}.jsonld` })
      .click(); // BUG test id = metadata-download-button

    const dRepMetadata = await this.downloadVoteMetadata();
    const url = await metadataBucketService.uploadMetadata(
      dRepMetadata.name,
      dRepMetadata.data
    );
    await this.page.getByPlaceholder("URL").fill(url);
    await this.continueBtn.click();
  }

  async downloadVoteMetadata() {
    const download: Download = await this.page.waitForEvent("download");
    return downloadMetadata(download);
  }

  async fillupForm(governanceProposal: IProposalForm) {
    await this.titleInput.fill(governanceProposal.title);
    await this.abstractInput.fill(governanceProposal.abstract);
    await this.motivationInput.fill(governanceProposal.motivation);
    await this.rationaleInput.fill(governanceProposal.rationale);

    if (governanceProposal.type === "Treasury") {
      await this.receivingAddressInput.fill(
        governanceProposal.receivingAddress
      );
      await this.amountInput.fill(governanceProposal.amount);
    }

    if (governanceProposal.extraContentLinks != null) {
      for (let i = 0; i < governanceProposal.extraContentLinks.length; i++) {
        if (i > 0) {
          this.page
            .getByRole("button", {
              name: "+ Add link",
            })
            .click(); // BUG
        }
        await this.linkInput
          .nth(i)
          .fill(governanceProposal.extraContentLinks[i]);
      }
    }
  }

  async validateForm(governanceProposal: IProposalForm) {
    await this.fillupForm(governanceProposal);

    for (const err of formErrors.proposalTitle) {
      await expect(this.page.getByTestId(err)).toBeHidden();
    }

    expect(await this.abstractInput.textContent()).toEqual(
      governanceProposal.abstract
    );

    expect(await this.rationaleInput.textContent()).toEqual(
      governanceProposal.rationale
    );

    expect(await this.motivationInput.textContent()).toEqual(
      governanceProposal.motivation
    );

    if (governanceProposal.type === "Treasury") {
      await expect(
        this.page.getByTestId(formErrors.receivingAddress)
      ).toBeHidden();

      for (const err of formErrors.amount) {
        await expect(this.page.getByTestId(err)).toBeHidden();
      }
    }

    await expect(this.page.getByTestId(formErrors.link)).toBeHidden();

    await expect(this.continueBtn).toBeEnabled();
  }

  async inValidateForm(governanceProposal: IProposalForm) {
    await this.fillupForm(governanceProposal);

    function convertTestIdToText(testId: string) {
      let text = testId.replace("-error", "");
      text = text.replace(/-/g, " ");
      return text[0].toUpperCase() + text.substring(1);
    }

    // Helper function to generate regex pattern from form errors
    function generateRegexPattern(errors: string[]) {
      return new RegExp(errors.map(convertTestIdToText).join("|"));
    }

    // Helper function to get errors based on regex pattern
    async function getErrorsByPattern(page: Page, regexPattern: RegExp) {
      return await page
        .locator('[data-testid$="-error"]')
        .filter({ hasText: regexPattern })
        .all();
    }

    const proposalTitlePattern = generateRegexPattern(formErrors.proposalTitle);
    const proposalTitleErrors = await getErrorsByPattern(
      this.page,
      proposalTitlePattern
    );

    expectWithInfo(
      async () => expect(proposalTitleErrors.length).toEqual(1),
      `valid title: ${governanceProposal.title}`
    );
    if (governanceProposal.type === "Treasury") {
      const receiverAddressErrors = await getErrorsByPattern(
        this.page,
        new RegExp(convertTestIdToText(formErrors.receivingAddress))
      );

      expectWithInfo(
        async () => expect(receiverAddressErrors.length).toEqual(1),
        `valid address: ${governanceProposal.receivingAddress}`
      );

      const amountPattern = generateRegexPattern(formErrors.amount);
      const amountErrors = await getErrorsByPattern(this.page, amountPattern);

      expectWithInfo(
        async () => expect(amountErrors.length).toEqual(1),
        `valid amount: ${governanceProposal.amount}`
      );
    }

    expectWithInfo(
      async () =>
        expect(await this.abstractInput.textContent()).not.toEqual(
          governanceProposal.abstract
        ),
      `valid abstract: ${governanceProposal.abstract}`
    );

    expectWithInfo(
      async () =>
        expect(await this.abstractInput.textContent()).not.toEqual(
          governanceProposal.motivation
        ),
      `valid motivation: ${governanceProposal.motivation}`
    );

    expectWithInfo(
      async () =>
        expect(await this.abstractInput.textContent()).not.toEqual(
          governanceProposal.rationale
        ),
      `valid rationale: ${governanceProposal.rationale}`
    );

    expectWithInfo(
      async () =>
        await expect(this.page.getByTestId(formErrors.link)).toBeVisible(),
      `valid link: ${governanceProposal.extraContentLinks[0]}`
    );

    await expect(this.continueBtn).toBeDisabled();
  }

  generateValidProposalFormFields(
    proposalType: ProposalType,
    receivingAddress?: string
  ) {
    const proposal: IProposalForm = {
      title: faker.lorem.sentence(6),
      abstract: faker.lorem.paragraph(2),
      motivation: faker.lorem.paragraphs(2),
      rationale: faker.lorem.paragraphs(2),

      extraContentLinks: [faker.internet.url()],
      type: proposalType,
    };
    if (proposalType === ProposalType.treasury) {
      (proposal.receivingAddress = receivingAddress),
        (proposal.amount = faker.number
          .int({ min: 100, max: 1000 })
          .toString());
    }
    return proposal;
  }

  generateInValidProposalFormFields(proposalType: ProposalType) {
    const proposal: IProposalForm = {
      title: invalid.proposalTitle(),
      abstract: invalid.paragraph(),
      motivation: invalid.paragraph(),
      rationale: invalid.paragraph(),

      extraContentLinks: [invalid.url()],
      type: proposalType,
    };
    if (proposalType === ProposalType.treasury) {
      (proposal.receivingAddress = faker.location.streetAddress()),
        (proposal.amount = invalid.amount());
    }
    return proposal;
  }
}
