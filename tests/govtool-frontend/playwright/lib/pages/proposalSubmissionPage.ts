import environments from "@constants/environments";
import { proposal04Wallet } from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { isBootStrapingPhase } from "@helpers/cardano";
import { ShelleyWallet } from "@helpers/crypto";
import { expectWithInfo } from "@helpers/exceptionHandler";
import { downloadMetadata } from "@helpers/metadata";
import { extractProposalIdFromUrl } from "@helpers/string";
import { invalid } from "@mock/index";
import { Download, Locator, Page, expect } from "@playwright/test";
import metadataBucketService from "@services/metadataBucketService";
import { ProposalCreateRequest, ProposalLink, ProposalType } from "@types";

const formErrors = {
  proposalTitle: "title-input-error",
  abstract: "abstract-helper-error",
  motivation: "motivation-helper-error",
  rationale: "rationale-helper-error",
  receivingAddress: "receiving-address-text-error",
  amount: "amount-text-error",
  link: "link-0-url-input-error",
};

export default class ProposalSubmissionPage {
  // modals
  readonly registrationSuccessModal =
    this.page.getByTestId("ga-submitted-modal");
  readonly registrationErrorModal = this.page.getByTestId(
    "create-governance-action-error-modal"
  );

  // buttons
  readonly proposalCreateBtn = this.page.getByTestId(
    "propose-a-governance-action-button"
  );
  readonly registerBtn = this.page.getByTestId("register-button");
  readonly skipBtn = this.page.getByTestId("skip-button");
  readonly confirmBtn = this.page.getByTestId("confirm-modal-button");

  readonly continueBtn = this.page.getByTestId("continue-button");
  readonly addLinkBtn = this.page.getByTestId("add-link-button");
  readonly infoBtn = this.page.getByTestId("info-button");
  readonly treasuryBtn = this.page.getByTestId("treasury-button");
  readonly editSubmissionButton = this.page.getByTestId(
    "edit-submission-button"
  );
  readonly verifyIdentityBtn = this.page.getByTestId("verify-identity-button");
  readonly governanceActionType = this.page.getByLabel(
    "Governance Action Type *"
  );
  readonly saveDraftBtn = this.page.getByTestId("save-draft-button");
  readonly submitBtn = this.page.getByTestId("submit-button");
  readonly createNewProposalBtn = this.page.getByTestId(
    "create-new-proposal-button"
  );

  // input fields
  readonly titleInput = this.page.getByTestId("title-input");
  readonly abstractInput = this.page.getByTestId("abstract-input");
  readonly metadataUrlInput = this.page.getByTestId("url-input");
  readonly motivationInput = this.page.getByTestId("motivation-input");
  readonly rationaleInput = this.page.getByTestId("rationale-input");
  readonly receivingAddressInput = this.page.getByTestId(
    "receiving-address-input"
  );
  readonly amountInput = this.page.getByTestId("amount-input");
  readonly closeDraftSuccessModalBtn = this.page.getByTestId("close-button");

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/proposal_discussion`);

    await this.page.waitForTimeout(2_000); // wait until page load properly

    await this.verifyIdentityBtn.click();
    await this.proposalCreateBtn.click();

    await this.continueBtn.click();
  }

  async fillUpValidMetadata() {
    this.page.getByTestId("download-button").click();

    const dRepMetadata = await this.downloadVoteMetadata();
    const url = await metadataBucketService.uploadMetadata(
      dRepMetadata.name,
      dRepMetadata.data
    );
    await this.metadataUrlInput.fill(url);
    await this.submitBtn.click();
  }

  async downloadVoteMetadata() {
    const download: Download = await this.page.waitForEvent("download");
    return downloadMetadata(download);
  }

  async fillupFormWithTypeSelected(governanceProposal: ProposalCreateRequest) {
    await this.fillCommonFields(governanceProposal);

    if (governanceProposal.gov_action_type_id === 1) {
      await this.fillTreasuryFields(governanceProposal);
    }

    if (governanceProposal.proposal_links != null) {
      await this.fillProposalLinks(governanceProposal.proposal_links);
    }
  }

  async fillupForm(governanceProposal: ProposalCreateRequest) {
    await this.governanceActionType.click();

    if (governanceProposal.gov_action_type_id === 0) {
      await this.infoBtn.click();
    } else {
      await this.treasuryBtn.click();
    }
    await this.fillupFormWithTypeSelected(governanceProposal);
  }

  async fillCommonFields(governanceProposal: ProposalCreateRequest) {
    await this.titleInput.fill(governanceProposal.prop_name);
    await this.abstractInput.fill(governanceProposal.prop_abstract);
    await this.motivationInput.fill(governanceProposal.prop_motivation);
    await this.rationaleInput.fill(governanceProposal.prop_rationale);
  }

  async fillTreasuryFields(governanceProposal: ProposalCreateRequest) {
    await this.receivingAddressInput.fill(
      governanceProposal.prop_receiving_address
    );
    await this.amountInput.fill(governanceProposal.prop_amount);
  }

  async fillProposalLinks(proposal_links: Array<ProposalLink>) {
    for (let i = 0; i < proposal_links.length; i++) {
      if (i > 0) {
        await this.addLinkBtn.click();
      }
      await this.page
        .getByTestId(`link-${i}-url-input`)
        .fill(proposal_links[i].prop_link);
      await this.page
        .getByTestId(`link-${i}-text-input`)
        .fill(proposal_links[i].prop_link_text);
    }
  }

  async getAllDrafts() {
    await expect(
      this.page.locator('[data-testid^="draft-"][data-testid$="-card"]')
    ).toBeVisible({ timeout: 10_000 }); // slow rendering

    return await this.page
      .locator('[data-testid^="draft-"][data-testid$="-card"]')
      .all();
  }

  async getFirstDraft() {
    await expect(
      this.page.locator('[data-testid^="draft-"][data-testid$="-card"]')
    ).toBeVisible({ timeout: 10_000 }); // slow rendering

    return this.page
      .locator('[data-testid^="draft-"][data-testid$="-card"]')
      .first();
  }

  async viewFirstDraft() {
    await expect(
      this.page.locator('[data-testid^="draft-"][data-testid$="-card"]')
    ).toBeVisible({ timeout: 10_000 }); // slow rendering

    return await this.page
      .locator('[data-testid^="draft-"][data-testid$="-start-editing"]')
      .first()
      .click();
  }

  async validateForm(governanceProposal: ProposalCreateRequest) {
    await this.fillupFormWithTypeSelected(governanceProposal);

    for (const err of formErrors.proposalTitle) {
      await expect(this.page.getByTestId(err)).toBeHidden();
    }

    expect(await this.abstractInput.textContent()).toEqual(
      governanceProposal.prop_abstract
    );

    expect(await this.rationaleInput.textContent()).toEqual(
      governanceProposal.prop_rationale
    );

    expect(await this.motivationInput.textContent()).toEqual(
      governanceProposal.prop_motivation
    );

    if (governanceProposal.gov_action_type_id === 1) {
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

  async assertFieldValidation(
    input: Locator,
    errorField: string,
    value: string,
    logMessage: string
  ) {
    if (value === " ") {
      await expect(this.page.getByTestId(errorField)).toBeVisible();
    } else {
      expectWithInfo(
        async () => expect(await input.textContent()).not.toEqual(value),
        `${logMessage}: ${value}`
      );
    }
  }

  async inValidateForm(governanceProposal: ProposalCreateRequest) {
    await this.fillupFormWithTypeSelected(governanceProposal);
    await expect(this.page.getByTestId(formErrors.proposalTitle)).toBeVisible();

    await this.assertFieldValidation(
      this.abstractInput,
      formErrors.abstract,
      governanceProposal.prop_abstract,
      "valid abstract"
    );
    await this.assertFieldValidation(
      this.motivationInput,
      formErrors.motivation,
      governanceProposal.prop_motivation,
      "valid motivation"
    );
    await this.assertFieldValidation(
      this.rationaleInput,
      formErrors.rationale,
      governanceProposal.prop_rationale,
      "valid rationale"
    );

    await expect(this.page.getByTestId(formErrors.link)).toBeVisible();

    if (governanceProposal.gov_action_type_id === 1) {
      await expect(
        this.page.getByTestId(formErrors.receivingAddress)
      ).toBeVisible();

      await expect(this.page.getByTestId(formErrors.amount)).toBeVisible();
    }

    await expect(this.continueBtn).toBeDisabled();
  }

  generateValidProposalFormFields(
    proposalType: ProposalType,
    is_draft?: boolean,
    receivingAddress?: string
  ) {
    const proposal: ProposalCreateRequest = {
      prop_name: faker.lorem.sentence(6),
      prop_abstract: faker.lorem.words(5),
      prop_motivation: faker.lorem.words(5),
      prop_rationale: faker.lorem.words(5),

      proposal_links: [
        {
          prop_link: faker.internet.url(),
          prop_link_text: faker.internet.domainWord(),
        },
      ],
      gov_action_type_id: proposalType === ProposalType.info ? 0 : 1,
      is_draft: !!is_draft,
    };

    if (proposalType === ProposalType.treasury) {
      (proposal.prop_receiving_address = receivingAddress),
        (proposal.prop_amount = faker.number
          .int({ min: 100, max: 1000 })
          .toString());
    }
    return proposal;
  }

  generateInValidProposalFormFields(proposalType: ProposalType) {
    const proposal: ProposalCreateRequest = {
      prop_name: invalid.proposalTitle(),
      prop_abstract: invalid.paragraph(2510),
      prop_motivation: invalid.paragraph(12020),
      prop_rationale: invalid.paragraph(12020),

      proposal_links: [
        {
          prop_link: invalid.url(),
          prop_link_text: invalid.name(),
        },
      ],
      gov_action_type_id: proposalType === ProposalType.info ? 0 : 1,
      is_draft: false,
    };

    if (proposalType === ProposalType.treasury) {
      (proposal.prop_receiving_address = faker.location.streetAddress()),
        (proposal.prop_amount = invalid.amount());
    }
    return proposal;
  }

  async createProposal(): Promise<number> {
    await this.addLinkBtn.click();
    const receivingAddr = (await ShelleyWallet.generate()).rewardAddressBech32(
      0
    );

    const proposalRequest: ProposalCreateRequest =
      this.generateValidProposalFormFields(
        (await isBootStrapingPhase())
          ? ProposalType.info
          : ProposalType.treasury,
        false,
        receivingAddr
      );
    await this.fillupForm(proposalRequest);
    await this.continueBtn.click();
    await this.submitBtn.click();

    // Wait for redirection to `proposal-discussion-details` page
    await this.page.waitForTimeout(2_000);

    const currentPageUrl = this.page.url();
    return extractProposalIdFromUrl(currentPageUrl);
  }

  async createDraft(proposalType: ProposalType) {
    await this.goto();
    await this.addLinkBtn.click();

    const proposalFormValue = this.generateValidProposalFormFields(
      proposalType,
      true,
      ShelleyWallet.fromJson(proposal04Wallet).rewardAddressBech32(0)
    );
    await this.fillupForm(proposalFormValue);

    await this.saveDraftBtn.click();
    await this.closeDraftSuccessModalBtn.click();

    await this.proposalCreateBtn.click();
    return { proposalFormValue };
  }
}
