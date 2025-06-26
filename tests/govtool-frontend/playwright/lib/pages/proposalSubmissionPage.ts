import environments from "@constants/environments";
import { guardrailsScript, guardrailsScriptHash } from "@constants/index";
import { proposal04Wallet } from "@constants/staticWallets";
import { faker } from "@faker-js/faker";
import { isBootStrapingPhase } from "@helpers/cardano";
import { ShelleyWallet } from "@helpers/crypto";
import { expectWithInfo } from "@helpers/exceptionHandler";
import {
  downloadMetadata,
  uploadScriptAndGenerateUrl,
} from "@helpers/metadata";
import { extractProposalIdFromUrl } from "@helpers/string";
import { invalid, valid } from "@mock/index";
import { Download, Locator, Page, expect } from "@playwright/test";
import metadataBucketService from "@services/metadataBucketService";
import {
  ProposalCreateRequest,
  ProposalLink,
  ProposalType,
  StaticWallet,
} from "@types";

const formErrors = {
  proposalTitle: "title-input-error",
  abstract: "abstract-helper-error",
  motivation: "motivation-helper-error",
  rationale: "rationale-helper-error",
  receivingAddress: "receiving-address-0-text-error",
  amount: "amount-0-text-error",
  constitutionalUrl: "prop-constitution-url-text-error",
  guardrailsScriptUrl: "prop-guardrails-script-url-input-error",
  link: "link-0-url-input-error",
  majorError: "major-error",
  minorError: "minor-error",
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
  readonly addWithdrawalAddressBtn = this.page.getByTestId(
    "add-withdrawal-link-button"
  );
  readonly infoBtn = this.page.getByTestId("info action-button");
  readonly treasuryBtn = this.page.getByTestId("treasury requests-button");
  readonly updateTheConstitutionBtn = this.page.getByTestId(
    "updates to the constitution-button"
  );
  readonly motionOfNoConfidenceBtn = this.page.getByTestId(
    "motion of no confidence-button"
  );
  readonly hardForkBtn = this.page.getByTestId("hard fork-button");
  readonly editSubmissionButton = this.page.getByTestId(
    "edit-submission-button"
  );
  readonly verifyIdentityBtn = this.page
    .getByTestId("verify-user-link")
    .first();
  readonly governanceActionType = this.page.getByLabel(
    "Governance Action Type *"
  );
  readonly saveDraftBtn = this.page.getByTestId("save-draft-button");
  readonly submitBtn = this.page.getByTestId("submit-button");
  readonly createNewProposalBtn = this.page.getByTestId(
    "create-new-proposal-button"
  );
  readonly guardrailsScriptCheckbox = this.page.getByTestId(
    "chb-prop-have-guardrails-script"
  );

  // input fields
  readonly titleInput = this.page.getByTestId("title-input");
  readonly abstractInput = this.page.getByTestId("abstract-input");
  readonly metadataUrlInput = this.page.getByTestId("url-input");
  readonly motivationInput = this.page.getByTestId("motivation-input");
  readonly rationaleInput = this.page.getByTestId("rationale-input");
  readonly receivingAddressInput = this.page.getByTestId(
    "receiving-address-0-text-input"
  );
  readonly amountInput = this.page.getByTestId("amount-0-text-input");
  readonly constitutionUrlInput = this.page.getByTestId(
    "prop_constitution_url"
  );
  readonly guardrailsScriptUrlInput = this.page.getByTestId(
    "prop-guardrails-script-url-input"
  );
  readonly guardrailsScriptHashInput = this.page.getByTestId(
    "prop-guardrails-script-hash-input"
  );
  readonly closeDraftSuccessModalBtn = this.page.getByTestId("close-button");
  readonly linkTextInput = this.page.getByTestId("link-0-text-input");
  readonly linkUrlInput = this.page.getByTestId("link-0-url-input");
  readonly previousGAHashInput = this.page.getByTestId(
    "previous-ga-hash-input"
  );
  readonly previousGAIdInput = this.page.getByTestId("previous-ga-id-input");
  readonly majorInput = this.page.getByTestId("major-input");
  readonly minorInput = this.page.getByTestId("minor-input");

  // content
  readonly governanceActionTypeContent = this.page.getByTestId(
    "governance-action-type-content"
  );
  readonly titleContent = this.page.getByTestId("title-content");
  readonly abstractContent = this.page.getByTestId("abstract-content");
  readonly motivationContent = this.page.getByTestId("motivation-content");
  readonly rationaleContent = this.page.getByTestId("rationale-content");
  readonly receivingAddressContent = this.page.getByTestId(
    "receiving-address-0-content"
  );
  readonly amountContent = this.page.getByTestId("amount-0-content");
  readonly constitutionUrlContent = this.page.getByTestId(
    "new-constitution-url-content"
  );
  readonly guardrailsScriptUrlContent = this.page.getByTestId(
    "guardrails-script-url-content"
  );
  readonly guardrailsScriptHashContent = this.page.getByTestId(
    "guardrails-script-hash-content"
  );
  readonly linkTextContent = this.page.getByTestId("link-0-text-content");
  readonly linkUrlContent = this.page.getByTestId("link-0-url-content");
  readonly majorVersionContent = this.page.getByTestId("major-version-content");
  readonly minorVersionContent = this.page.getByTestId("minor-version-content");

  constructor(private readonly page: Page) {}

  get currentPage(): Page {
    return this.page;
  }

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/proposal_discussion`);

    await this.verifyIdentityBtn.click();
    await this.proposalCreateBtn.click();

    await this.continueBtn.click();
  }

  async fillUpValidMetadata() {
    this.page.getByTestId("download-button").click();

    const dRepMetadata = await this.downloadVoteMetadata();
    const url = await metadataBucketService.uploadMetadata(
      faker.lorem.word({ length: { min: 5, max: 10 } }),
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

    if (governanceProposal.gov_action_type_id === 2) {
      await this.fillUpdateTheConstitutionFields(governanceProposal);
    }

    if (governanceProposal.proposal_links != null) {
      await this.fillProposalLinks(governanceProposal.proposal_links);
    }

    if (governanceProposal.gov_action_type_id == 4) {
      await this.fillHardForkFields(governanceProposal);
    }
  }

  async fillupForm(governanceProposal: ProposalCreateRequest) {
    console.log(governanceProposal.gov_action_type_id);
    await this.governanceActionType.click();

    if (governanceProposal.gov_action_type_id === 0) {
      await this.infoBtn.click();
    } else if (governanceProposal.gov_action_type_id === 1) {
      await this.treasuryBtn.click();
    } else if (governanceProposal.gov_action_type_id === 2) {
      await this.updateTheConstitutionBtn.click();
      if (governanceProposal.has_guardrails) {
        await this.guardrailsScriptCheckbox.click();
      }
    } else if (governanceProposal.gov_action_type_id === 3) {
      await this.motionOfNoConfidenceBtn.click();
    } else {
      await this.hardForkBtn.click();
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

  async fillUpdateTheConstitutionFields(
    governanceProposal: ProposalCreateRequest
  ) {
    await this.constitutionUrlInput.fill(
      governanceProposal.prop_constitution_url
    );

    if (governanceProposal.has_guardrails) {
      await this.guardrailsScriptUrlInput.fill(
        governanceProposal.prop_guardrails_script_url
      );
      await this.guardrailsScriptHashInput.fill(
        governanceProposal.prop_guardrails_script_hash
      );
    }
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

  async fillHardForkFields(hardForkProposal: ProposalCreateRequest) {
    await this.minorInput.fill(hardForkProposal.prop_min_version.toString());
    await this.majorInput.fill(hardForkProposal.prop_major_version.toString());
  }

  async getAllDrafts() {
    await expect(
      this.page.locator('[data-testid^="draft-"][data-testid$="-card"]')
    ).toBeVisible({ timeout: 60_000 }); // slow rendering

    return await this.page
      .locator('[data-testid^="draft-"][data-testid$="-card"]')
      .all();
  }

  async getFirstDraft() {
    await expect(
      this.page.locator('[data-testid^="draft-"][data-testid$="-card"]').first()
    ).toBeVisible({ timeout: 60_000 }); // slow rendering

    return this.page
      .locator('[data-testid^="draft-"][data-testid$="-card"]')
      .first();
  }

  async viewFirstDraft() {
    await expect(
      this.page.locator('[data-testid^="draft-"][data-testid$="-card"]')
    ).toBeVisible({ timeout: 60_000 }); // slow rendering

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

    const abstractInputContent = await this.abstractInput.textContent();
    const rationaleInputContent = await this.rationaleInput.textContent();
    const motivationInputContent = await this.motivationInput.textContent();
    const isLinkErrorVisible = await this.page
      .getByTestId(formErrors.link)
      .isVisible();

    expect(await this.abstractInput.textContent(), {
      message:
        abstractInputContent !== governanceProposal.prop_abstract &&
        `${governanceProposal.prop_abstract} is not equal to ${await this.abstractInput.textContent()}`,
    }).toEqual(governanceProposal.prop_abstract);

    expect(await this.rationaleInput.textContent(), {
      message:
        rationaleInputContent !== governanceProposal.prop_rationale &&
        `${governanceProposal.prop_rationale} is not equal to ${await this.rationaleInput.textContent()}`,
    }).toEqual(governanceProposal.prop_rationale);

    expect(await this.motivationInput.textContent(), {
      message:
        motivationInputContent !== governanceProposal.prop_motivation &&
        `${governanceProposal.prop_motivation} is not equal to ${await this.motivationInput.textContent()}`,
    }).toEqual(governanceProposal.prop_motivation);

    if (governanceProposal.gov_action_type_id === 1) {
      const isReceivingAddressErrorVisible = await this.page
        .getByTestId(formErrors.receivingAddress)
        .isVisible();
      const isAmountErrorVisible = await this.page
        .getByTestId(formErrors.amount)
        .isVisible();

      await expect(this.page.getByTestId(formErrors.receivingAddress), {
        message:
          isReceivingAddressErrorVisible &&
          `${governanceProposal.prop_receiving_address} is an invalid receiving address`,
      }).toBeHidden();

      for (const err of formErrors.amount) {
        await expect(this.page.getByTestId(err), {
          message:
            isAmountErrorVisible &&
            `${governanceProposal.prop_amount} is an invalid amount`,
        }).toBeHidden();
      }
    }

    if (governanceProposal.gov_action_type_id === 2) {
      const isConstitutionalUrlErrorVisible = await this.page
        .getByTestId(formErrors.constitutionalUrl)
        .isVisible();
      const isGuardrailsScriptUrlErrorVisible = await this.page
        .getByTestId(formErrors.guardrailsScriptUrl)
        .isVisible();

      await expect(this.page.getByTestId(formErrors.constitutionalUrl), {
        message:
          isConstitutionalUrlErrorVisible &&
          `${governanceProposal.prop_constitution_url} is an invalid constitution url`,
      }).toBeHidden();

      await expect(this.page.getByTestId(formErrors.guardrailsScriptUrl), {
        message:
          isGuardrailsScriptUrlErrorVisible &&
          `${governanceProposal.prop_guardrails_script_url} is an invalid guardrails script url`,
      }).toBeHidden();
    }

    if (governanceProposal.gov_action_type_id === 4) {
      const isMajorErrorVisible = await this.page
        .getByTestId(formErrors.majorError)
        .isVisible();
      const isMinorErrorVisible = await this.page
        .getByTestId(formErrors.minorError)
        .isVisible();

      await expect(this.page.getByTestId(formErrors.majorError), {
        message: isMajorErrorVisible
          ? "Major version error should be hidden"
          : "Major version error is correctly hidden",
      }).toBeHidden();

      await expect(this.page.getByTestId(formErrors.minorError), {
        message: isMinorErrorVisible
          ? "Minor version error should be hidden"
          : "Minor version error is correctly hidden",
      }).toBeHidden();
    }

    await expect(this.page.getByTestId(formErrors.link), {
      message:
        isLinkErrorVisible &&
        `${governanceProposal.proposal_links[0].prop_link} is an invalid link`,
    }).toBeHidden();

    await expect(this.continueBtn).toBeEnabled();
  }

  async assertFieldValidation(
    input: Locator,
    errorField: string,
    value: string,
    logMessage: string
  ) {
    const isErrorFieldVisible = await this.page
      .getByTestId(errorField)
      .isVisible();
    if (value === " ") {
      await expect(this.page.getByTestId(errorField), {
        message:
          !isErrorFieldVisible && `Empty value is assumed as ${logMessage}`,
      }).toBeVisible();
    } else {
      expectWithInfo(
        async () => expect(await input.textContent()).not.toEqual(value),
        `${logMessage}: ${value}`
      );
    }
  }

  async inValidateForm(governanceProposal: ProposalCreateRequest) {
    await this.fillupFormWithTypeSelected(governanceProposal);
    const isProposalTitleErrorVisible = await this.page
      .getByTestId(formErrors.proposalTitle)
      .isVisible();
    const isLinkErrorVisible = await this.page
      .getByTestId(formErrors.link)
      .isVisible();

    await expect(this.page.getByTestId(formErrors.proposalTitle), {
      message:
        !isProposalTitleErrorVisible &&
        `${governanceProposal.prop_name} is a valid proposal title`,
    }).toBeVisible();

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

    await expect(this.page.getByTestId(formErrors.link), {
      message:
        !isLinkErrorVisible &&
        `${governanceProposal.proposal_links[0].prop_link} is a valid link`,
    }).toBeVisible();

    if (governanceProposal.gov_action_type_id === 1) {
      const isReceivingAddressErrorVisible = await this.page
        .getByTestId(formErrors.receivingAddress)
        .isVisible();
      const isAmountErrorVisible = await this.page
        .getByTestId(formErrors.amount)
        .isVisible();

      await expect(this.page.getByTestId(formErrors.receivingAddress), {
        message:
          !isReceivingAddressErrorVisible &&
          `${governanceProposal.prop_receiving_address} is a valid receiving address`,
      }).toBeVisible();

      await expect(this.page.getByTestId(formErrors.amount), {
        message:
          !isAmountErrorVisible &&
          `${governanceProposal.prop_amount} is a valid amount`,
      }).toBeVisible();
    }

    if (governanceProposal.gov_action_type_id === 2) {
      const isConstitutionalUrlErrorVisible = await this.page
        .getByTestId(formErrors.constitutionalUrl)
        .isVisible();
      const isGuardrailsScriptUrlErrorVisible = await this.page
        .getByTestId(formErrors.guardrailsScriptUrl)
        .isVisible();
      await expect(this.page.getByTestId(formErrors.constitutionalUrl), {
        message:
          !isConstitutionalUrlErrorVisible &&
          `${governanceProposal.prop_constitution_url} is a valid constitution url`,
      }).toBeVisible();

      await expect(this.page.getByTestId(formErrors.guardrailsScriptUrl), {
        message:
          !isGuardrailsScriptUrlErrorVisible &&
          `${governanceProposal.prop_guardrails_script_url} is a valid guardrails script url`,
      }).toBeVisible();
    }

    if (governanceProposal.gov_action_type_id === 4) {
      await expect(this.page.getByTestId(formErrors.majorError)).toBeVisible();
      await expect(this.page.getByTestId(formErrors.minorError)).toBeVisible();
    }

    await expect(this.continueBtn).toBeDisabled();
  }

  async generateValidProposalFormFields({
    proposalType,
    is_draft,
    receivingAddress,
    hasGuardrails = true,
    forValidation = false,
  }: {
    proposalType: ProposalType;
    is_draft?: boolean;
    receivingAddress?: string;
    hasGuardrails?: boolean;
    forValidation?: boolean;
  }) {
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
      gov_action_type_id: Object.values(ProposalType).indexOf(proposalType),
      has_guardrails: hasGuardrails,
      is_draft: !!is_draft,
    };

    if (proposalType === ProposalType.treasury) {
      (proposal.prop_receiving_address = receivingAddress),
        (proposal.prop_amount = faker.number
          .int({ min: 100, max: 1000 })
          .toString());
    }
    if (proposalType === ProposalType.updatesToTheConstitution) {
      proposal.prop_constitution_url = forValidation
        ? valid.url()
        : environments.metadataBucketUrl + "/data.jsonId";

      if (hasGuardrails) {
        if (!forValidation) {
          const url = await uploadScriptAndGenerateUrl(guardrailsScript);
          proposal.prop_guardrails_script_url = url;
          proposal.prop_guardrails_script_hash = guardrailsScriptHash;
        } else {
          proposal.prop_guardrails_script_url = valid.url();
          proposal.prop_guardrails_script_hash = faker.string.alphanumeric(64);
        }
      }
    }
    if (proposalType == ProposalType.hardFork) {
      proposal.prop_min_version = faker.number
        .float({ min: 0, max: 100 })
        .toString();
      proposal.prop_major_version = faker.number
        .float({ min: 0, max: 100 })
        .toString();
    }

    return proposal;
  }

  generateInValidProposalFormFields(
    proposalType: ProposalType,
    hasGuardrails: boolean = true
  ) {
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
      gov_action_type_id: Object.values(ProposalType).indexOf(proposalType),
      has_guardrails: hasGuardrails,
      is_draft: false,
    };

    if (proposalType === ProposalType.treasury) {
      (proposal.prop_receiving_address = faker.location.streetAddress()),
        (proposal.prop_amount = invalid.amount());
    }

    if (proposalType === ProposalType.updatesToTheConstitution) {
      proposal.prop_constitution_url = invalid.constitutionUrl();
      proposal.prop_guardrails_script_url = invalid.url();
      proposal.prop_guardrails_script_hash = faker.string.alphanumeric(64);
    }

    if (proposalType === ProposalType.hardFork) {
      proposal.prop_min_version = invalid.amount();
      proposal.prop_major_version = invalid.amount();
    }

    return proposal;
  }

  async createProposal(
    receivingAddress: string,
    proposalType: ProposalType = Object.values(ProposalType)[
      Math.floor(Math.random() * Object.values(ProposalType).length)
    ]
  ): Promise<number> {
    await this.addLinkBtn.click();
    const proposalRequest: ProposalCreateRequest =
      await this.generateValidProposalFormFields({
        proposalType: (await isBootStrapingPhase())
          ? ProposalType.info
          : proposalType,
        receivingAddress,
      });
    await this.fillupForm(proposalRequest);
    await this.continueBtn.click();
    await this.submitBtn.click();

    await expect(this.page.getByTestId("submit-as-GA-button")).toBeVisible();
    const currentPageUrl = this.page.url();
    return extractProposalIdFromUrl(currentPageUrl);
  }

  async createDraft(proposalType: ProposalType) {
    await this.goto();
    await this.addLinkBtn.click();

    const proposalFormValue = await this.generateValidProposalFormFields({
      proposalType: proposalType,
      is_draft: true,
      receivingAddress: ShelleyWallet.fromJson(
        proposal04Wallet
      ).rewardAddressBech32(environments.networkId),
    });
    await this.fillupForm(proposalFormValue);

    await this.saveDraftBtn.click();
    await this.closeDraftSuccessModalBtn.click();

    await this.proposalCreateBtn.click();
    return { proposalFormValue };
  }
}
