import environments from "@constants/environments";
import { faker } from "@faker-js/faker";
import { formatWithThousandSeparator } from "@helpers/adaFormat";
import { extractProposalIdFromUrl, generateParagraph } from "@helpers/string";
import { invalid, valid } from "@mock/index";
import { Locator, Page, expect } from "@playwright/test";
import {
  AdministrationAndAuditingProps,
  BudgetCostingProps,
  BudgetDiscussionEnum,
  BudgetProposalContactInformationProps,
  BudgetProposalDetailsProps,
  BudgetProposalOwnershipProps,
  BudgetProposalProblemStatementAndBenefitProps,
  BudgetProposalProps,
  BudgetProposalStageEnum,
  CommitteeAlignmentEnum,
  CompanyEnum,
  LocationEnum,
  PreferredCurrencyEnum,
  ProposalContractingEnum,
  ProposalLink,
  RoadmapNameEnum,
} from "@types";

const formErrors = {
  proposalTitle: "title-input-error",
  abstract: "abstract-helper-error",
  motivation: "motivation-helper-error",
  rationale: "rationale-helper-error",
  receivingAddress: "receiving-address-0-text-error",
  adaAmount: "ada-amount-error",
  usdToAdaConversionError: "usd-to-ada-converson-error",
  preferredCurrencyError: "preferred-currency-error",
  constitutionalUrl: "prop-constitution-url-text-error",
  guardrailsScriptUrl: "prop-guardrails-script-url-input-error",
  link: "link-0-url-input-error",
};

export default class BudgetDiscussionSubmissionPage {
  // buttons
  readonly skipBtn = this.page.getByTestId("skip-button");
  readonly createBudgetProposalBtn = this.page.getByTestId(
    "propose-a-budget-discussion-button"
  );
  readonly closeDraftBtn = this.page.getByTestId("close-button");
  readonly cancelBtn = this.page.getByTestId("cancel-button");

  readonly continueBtn = this.page.getByTestId("continue-button");
  readonly addLinkBtn = this.page.getByTestId("add-link-button");
  readonly verifyIdentityBtn = this.page.getByTestId("verify-identity-button");
  readonly saveDraftBtn = this.page.getByTestId("draft-button");
  readonly submitBtn = this.page.getByTestId("submit-button");
  readonly countryOfIncorporationBtn = this.page.getByTestId(
    "country-of-incorporation"
  );

  readonly agreeCheckbox = this.page.getByLabel(
    "I agree to the information in"
  ); //BUG missing test Ids
  readonly submitCheckbox = this.page.getByTestId("submit-checkbox");

  // input
  readonly linkTextInput = this.page.getByTestId("link-0-text-input");
  readonly linkUrlInput = this.page.getByTestId("link-0-url-input");

  // proposal-ownership
  readonly companyNameInput = this.page.getByLabel("Company Name *"); //BUG missing test Ids
  readonly companyDomainNameInput = this.page.getByLabel(
    "Company Domain Name *"
  ); //BUG missing test Ids
  readonly groupNameInput = this.page.getByLabel("Group Name *"); //BUG missing test Ids
  readonly groupTypeInput = this.page.getByLabel("Type of Group *"); //BUG missing test Ids
  readonly keyInformationOfGroupInput = this.page.getByLabel(
    "Key Information to Identify"
  ); //BUG missing test Ids
  readonly contactDetailsInput = this.page.getByLabel(
    "Please provide your preferred"
  ); //BUG missing test Ids

  // problem-statements
  readonly problemStatementInput = this.page.getByTestId(
    "problem-statement-input"
  );
  readonly proposalBenefitInput = this.page.getByTestId(
    "proposal-benefit-input"
  );
  readonly suplimentaryEndorsementInput = this.page.getByTestId(
    "supplementary-endorsement-input"
  );
  readonly productRoadmapDescriptionInput = this.page.getByLabel(
    "Please explain how your"
  ); // BUG missing test Ids

  // proposal-details
  readonly proposalNameInput = this.page.getByLabel(
    "What is your proposed name to"
  ); //BUG missing testId
  readonly proposalDescriptionInput = this.page.getByTestId(
    "proposal-description-input"
  );
  readonly proposalKeyDependenciesInput = this.page.getByTestId(
    "key-dependencies-input"
  );
  readonly proposalMaintainAndSupportInput = this.page.getByLabel(
    "How will this proposal be"
  ); //BUG missing testId
  readonly milestonesInput = this.page.getByTestId(
    "key-proposal-deliverables-input"
  );
  readonly teamSizeAndDurationInput = this.page.getByTestId(
    "resourcing-duration-estimates-input"
  );
  readonly previousExperienceInput = this.page.getByLabel(
    "Please provide previous"
  ); //BUG missing testId
  readonly otherDescriptionInput = this.page.getByLabel(
    "Please describe what you have"
  );

  // costing
  readonly adaAmountInput = this.page.getByLabel("ADA Amount *"); //BUG missing test Ids
  readonly usaToAdaCnversionRateInput = this.page.getByLabel(
    "USD to ADA Conversion Rate *"
  ); //BUG missing test Ids
  readonly preferredCurrencyInput = this.page.getByLabel(
    "Amount in preferred currency *"
  );
  readonly costBreakdownInput = this.page.getByTestId("cost-breakdown-input");
  readonly venderDetailsInput = this.page.getByLabel("Please provide further"); //BUG missing test Ids

  // select
  readonly beneficiaryCountrySelect = this.page.getByTestId(
    "beneficiary-country-of-residence"
  );
  readonly beneficiaryNationalitySelect = this.page.getByTestId(
    "beneficiary-nationality"
  );

  readonly companyTypeSelect = this.page.getByTestId("beneficiary-type");
  readonly publicChampionSelect = this.page.getByTestId(
    "proposal-public-champion"
  );

  readonly roadmapNameSelect = this.page.getByTestId("roadmap-name");
  readonly budgetDiscussionTypeSelect = this.page.getByTestId(
    "budget-discussion-type-name"
  );
  readonly committeeAlignmentTypeSelect = this.page.getByTestId(
    "committee-alignment-type"
  );

  readonly contractingTypeSelect = this.page.getByTestId("contract-type-name");
  readonly preferredCurrencySelect =
    this.page.getByTestId("preferred-currency");

  readonly intersectNamedAdministratorSelect = this.page.getByTestId(
    "intersect-named-administrator"
  );

  // content
  readonly linkTextContent = this.page.getByTestId("link-0-text-content");
  readonly linkUrlContent = this.page.getByTestId("link-0-url-content");

  // contact information
  readonly beneficiaryFullNameContent = this.page.getByTestId(
    "beneficiary-full-name-content"
  );
  readonly beneficiaryCountryOfResidenceContent = this.page.getByTestId(
    "beneficiary-country-of-residence-content"
  );

  // proposal ownership
  readonly companyTypeContent = this.page.getByTestId(
    "submited-on-behalf-content"
  );
  readonly groupNameContent = this.page.getByTestId("group-name-content");
  readonly groupTypeContent = this.page.getByTestId("group-type-content");
  readonly groupKeyIdentityContent = this.page.getByTestId(
    "group-identity-information-content"
  );
  readonly companyNameContent = this.page.getByTestId("company-name-content");
  readonly companyDomainNameContent = this.page.getByTestId(
    "company-domain-name-content"
  );
  readonly countryOfIncorporationContent = this.page.getByTestId(
    "country-of-incorporation-content"
  );
  readonly providePreferredContent = this.page.getByTestId(
    "provide-preferred-content"
  );
  readonly socialHandlesContent = this.page.getByTestId(
    "social-handles-content"
  );

  // problem statements and benefits
  readonly problemStatementContent = this.page.getByTestId(
    "problem-statement-content"
  );
  readonly proposalBenefitsContent = this.page.getByTestId(
    "proposal-benefit-content"
  );
  readonly roadmapContent = this.page.getByTestId("roadmap-content");
  readonly budgetCategoryContent = this.page.getByTestId(
    "budget-category-content"
  );
  readonly committeeContent = this.page.getByTestId("committee-content");
  readonly endorsementContent = this.page.getByTestId("endorsement-content");

  // proposal details
  readonly proposalNameContent = this.page.getByTestId("proposal-name-content");
  readonly proposalDescriptionContent = this.page.getByTestId(
    "proposal-description-content"
  );
  readonly proposalKeyDependenciesContent = this.page.getByTestId(
    "key-dependencies-content"
  );
  readonly proposalMaintainAndSupportContent = this.page.getByTestId(
    "maintain-and-support-content"
  );
  readonly proposalDeliverablesContent = this.page.getByTestId(
    "key-proposal-deliverables-content"
  );
  readonly resourcingDurationEstimatesContent = this.page.getByTestId(
    "resourcing-duration-estimates-content"
  );
  readonly experienceContent = this.page.getByTestId("experience-content");
  readonly contractingContent = this.page.getByTestId(
    "contracting-type-name-content"
  );

  // costing
  readonly adaAmountContent = this.page.getByTestId("ada-amount-content");
  readonly adaToUsdConversionRateContent = this.page.getByTestId(
    "usd-to-ada-conversion-rate-content"
  ); // BUG typo
  readonly preferredCurrencyContent = this.page.getByTestId(
    "preferred-currency-content"
  );
  readonly preferredCurrencyAmountContent = this.page.getByTestId(
    "amount-in-preferred-currency-content"
  );
  readonly costBreakdownContent = this.page.getByTestId(
    "cost-breakdown-content"
  );

  // administration and auditing
  readonly intersectAdministrationContent = this.page.getByTestId(
    "intersect-named-administrator-content"
  );

  constructor(private readonly page: Page) {}

  get currentPage(): Page {
    return this.page;
  }

  async goto() {
    await this.page.goto(`${environments.frontendUrl}/budget_discussion`);

    await this.verifyIdentityBtn.click();
    await this.createBudgetProposalBtn.click();

    await this.continueBtn.click();
  }

  async fillupProposalOwnershipForm(
    proposalOwnership: BudgetProposalOwnershipProps,
    isNaviagted = true
  ) {
    await this.companyTypeSelect.click();
    await this.page
      .getByRole("option", { name: proposalOwnership.companyType })
      .click(); //BUG missing testId

    await this.contactDetailsInput.fill(proposalOwnership.contactDetails);

    if (proposalOwnership.companyType === "Group") {
      await this.groupNameInput.fill(proposalOwnership.groupName);
      await this.groupTypeInput.fill(proposalOwnership.groupType);
      await this.keyInformationOfGroupInput.fill(
        proposalOwnership.groupKeyIdentity
      );
    }
    if (proposalOwnership.companyType === "Company") {
      await this.companyNameInput.fill(proposalOwnership.companyName);
      await this.companyDomainNameInput.fill(
        proposalOwnership.companyDomainName
      );
      await this.countryOfIncorporationBtn.click();
      await this.page
        .getByTestId(
          `${proposalOwnership.countryOfIncorportation.toLowerCase().replace(/ /g, "-")}-country-of-incorporation-button`
        )
        .click();
    }

    if (isNaviagted) {
      await this.agreeCheckbox.click();
      await this.continueBtn.click();
    }
  }

  async fillupProblemStatementAndBenefitsForm(
    problemStatementAndBenefits: BudgetProposalProblemStatementAndBenefitProps,
    isNaviagated = true
  ) {
    await this.problemStatementInput.fill(
      problemStatementAndBenefits.problemStatement
    );
    await this.proposalBenefitInput.fill(
      problemStatementAndBenefits.proposalBenefits
    );
    await this.suplimentaryEndorsementInput.fill(
      problemStatementAndBenefits.suplimentaryEndorsement
    );

    await this.roadmapNameSelect.click();
    await this.page
      .getByTestId(
        `${problemStatementAndBenefits.roadmapName.toLowerCase()}-button`
      )
      .click();

    if (
      problemStatementAndBenefits.roadmapName ===
      "It supports the product roadmap"
    ) {
      await this.productRoadmapDescriptionInput.fill(
        problemStatementAndBenefits.productRoadmapDescription
      );
    }

    await this.budgetDiscussionTypeSelect.click();
    await this.page
      .getByTestId(
        `${problemStatementAndBenefits.budgetDiscussionType.toLowerCase()}-button`
      )
      .click();

    await this.committeeAlignmentTypeSelect.click();
    await this.page
      .getByTestId(
        `${problemStatementAndBenefits.committeeAlignmentType.toLowerCase()}-button`
      )
      .click();

    if (isNaviagated) {
      await this.continueBtn.click();
    }
  }

  async fillupProposalDetailsFormWithoutContractingType(
    proposalDetails: BudgetProposalDetailsProps
  ) {
    await this.proposalNameInput.fill(proposalDetails.proposalName);
    await this.proposalDescriptionInput.fill(
      proposalDetails.proposalDescription
    );
    await this.proposalKeyDependenciesInput.fill(
      proposalDetails.proposalKeyDependencies
    );
    await this.proposalMaintainAndSupportInput.fill(
      proposalDetails.proposalMaintainAndSupport
    );
    await this.milestonesInput.fill(proposalDetails.milestones);
    await this.teamSizeAndDurationInput.fill(
      proposalDetails.teamSizeAndDuration
    );
    await this.previousExperienceInput.fill(proposalDetails.previousExperience);
  }

  async fillupProposalDetailsForm(proposalDetails: BudgetProposalDetailsProps) {
    await this.fillupProposalDetailsFormWithoutContractingType(proposalDetails);
    await this.contractingTypeSelect.click();
    await this.page
      .getByTestId(`${proposalDetails.contracting.toLowerCase()}-button`)
      .click();
    if (proposalDetails.contracting === "Other") {
      await this.otherDescriptionInput.fill(proposalDetails.otherDescription);
    }

    await this.continueBtn.click();
  }

  async fillupCostingFormWithoutPreferredCurrency(costing: BudgetCostingProps) {
    await this.adaAmountInput.fill(costing.adaAmount.toString());
    await this.usaToAdaCnversionRateInput.fill(
      costing.usdToAdaConversionRate.toString()
    );
    await this.costBreakdownInput.fill(costing.costBreakdown);
    await this.preferredCurrencyInput.fill(
      costing.AmountInPreferredCurrency.toString()
    );
  }

  async fillupCostingForm(costing: BudgetCostingProps) {
    await this.fillupCostingFormWithoutPreferredCurrency(costing);
    await this.preferredCurrencySelect.click();
    await this.page
      .getByTestId(`${costing.preferredCurrency.toLowerCase()}-button`)
      .click();

    await this.continueBtn.click();
  }

  async fillupFurtherInformation(proposal_links: Array<ProposalLink>) {
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
    await this.continueBtn.click();
  }

  // async fillCostingSectionExceptAmountInputs() {
  //   const costing = this.generateValidCosting();
  //   await this.preferredCurrencySelect.click();
  //   await this.page
  //     .getByTestId(`${costing.preferredCurrency.toLowerCase()}-button`)
  //     .click();
  //   await this.preferredCurrencyInput.fill(
  //     costing.AmountInPreferredCurrency.toString()
  //   );
  //   await this.costBreakdownInput.fill(costing.costBreakdown.toString());
  // }

  async fillupForm(
    budgetProposal: BudgetProposalProps,
    stage: BudgetProposalStageEnum = BudgetProposalStageEnum.Review
  ) {
    await this.fillupProposalOwnershipForm(budgetProposal.proposalOwnership);

    if (stage > BudgetProposalStageEnum.ProposalOwnership) {
      await this.fillupProblemStatementAndBenefitsForm(
        budgetProposal.problemStatementAndBenefits
      );
    }

    if (stage > BudgetProposalStageEnum.ProblemStatementAndBenefits) {
      await this.fillupProposalDetailsForm(budgetProposal.proposalDetails);
    }
    if (stage > BudgetProposalStageEnum.ProposalDetails) {
      await this.fillupCostingForm(budgetProposal.costing);
    }
    if (stage > BudgetProposalStageEnum.Costing) {
      await this.fillupFurtherInformation(budgetProposal.furtherInformation);
    }
    if (stage > BudgetProposalStageEnum.FurtherInformation) {
      await this.intersectNamedAdministratorSelect.click();

      await this.page
        .getByTestId(
          `${budgetProposal.administrationAndAuditing.intersectAdministration}-button`
        )
        .click();
      if (!budgetProposal.administrationAndAuditing.intersectAdministration) {
        await this.venderDetailsInput.fill(
          budgetProposal.administrationAndAuditing.venderDetails
        );
      }
      await this.continueBtn.click();
      if (stage > BudgetProposalStageEnum.AdministrationAndAuditing) {
        await this.submitCheckbox.click();
        await this.continueBtn.click();
      }
    }
  }

  async getAllDrafts() {
    await expect(
      this.page
        .locator('[data-testid^="draft-"][data-testid$="-proposal"]')
        .first()
    ).toBeVisible({ timeout: 60_000 }); // slow rendering

    return await this.page
      .locator('[data-testid^="draft-"][data-testid$="-proposal"]')
      .all();
  }

  async getFirstDraft() {
    await expect(
      this.page
        .locator('[data-testid^="draft-"][data-testid$="-proposal"]')
        .first()
    ).toBeVisible({ timeout: 60_000 }); // slow rendering

    return this.page
      .locator('[data-testid^="draft-"][data-testid$="-proposal"]')
      .first();
  }

  async viewLastDraft() {
    await expect(
      this.page
        .locator('[data-testid^="draft-"][data-testid$="-proposal"]')
        .last()
    ).toBeVisible({ timeout: 60_000 }); // slow rendering

    return await this.page.getByTestId("draft-start-editing").last().click();
  }

  generateValidBudgetProposalContactInformation(): BudgetProposalContactInformationProps {
    return {
      beneficiaryFullName: faker.person.fullName(),
      beneficiaryEmail: faker.internet.email(),
      beneficiaryCountry: faker.helpers.arrayElement(
        Object.values(LocationEnum)
      ),
      beneficiaryNationality: faker.helpers.arrayElement(
        Object.values(LocationEnum)
      ),
      submissionLeadFullName: faker.person.fullName(),
      submissionLeadEmail: faker.internet.email(),
    };
  }

  generateValidBudgetProposalProblemStatementAndBenefits(): BudgetProposalProblemStatementAndBenefitProps {
    return {
      problemStatement: faker.lorem.paragraph(2),
      proposalBenefits: faker.lorem.paragraph(2),
      roadmapName: faker.helpers.arrayElement(Object.values(RoadmapNameEnum)),
      budgetDiscussionType: faker.helpers.arrayElement(
        Object.values(BudgetDiscussionEnum).filter(
          (type) => type !== "No Category"
        )
      ),
      productRoadmapDescription: faker.lorem.paragraph(2),
      committeeAlignmentType: faker.helpers.arrayElement(
        Object.values(CommitteeAlignmentEnum)
      ),
      suplimentaryEndorsement: faker.lorem.paragraph(2),
    };
  }

  generateValidProposalOwnerShip(): BudgetProposalOwnershipProps {
    return {
      companyType: faker.helpers.arrayElement(Object.values(CompanyEnum)),
      contactDetails: faker.internet.email(),
      groupName: faker.company.name(),
      groupType: faker.company.buzzVerb(),
      groupKeyIdentity: faker.lorem.paragraph(2),
      companyName: faker.company.name(),
      companyDomainName: faker.internet.domainName(),
      countryOfIncorportation: faker.helpers.arrayElement(
        Object.values(LocationEnum)
      ),
    };
  }
  generateInvalidProposalOwnerShip(): BudgetProposalOwnershipProps {
    return {
      groupName: invalid.paragraph(50),
      groupType: invalid.paragraph(50),
      groupKeyIdentity: invalid.paragraph(50),
      companyName: invalid.paragraph(50),
      companyDomainName: invalid.paragraph(50),
      companyType: faker.helpers.arrayElement(Object.values(CompanyEnum)),
      contactDetails: faker.lorem.paragraph(2),
      countryOfIncorportation: faker.helpers.arrayElement(
        Object.values(LocationEnum)
      ),
    };
  }

  generateValidBudgetProposalDetails(): BudgetProposalDetailsProps {
    return {
      proposalName: faker.lorem.words(3),
      proposalDescription: faker.lorem.paragraph(2),
      proposalKeyDependencies: faker.lorem.paragraph(1),
      proposalMaintainAndSupport: faker.lorem.paragraph(2),
      milestones: faker.lorem.lines(2),
      teamSizeAndDuration: faker.lorem.paragraph(2),
      previousExperience: faker.lorem.paragraph(2),
      contracting: faker.helpers.arrayElement(
        Object.values(ProposalContractingEnum)
      ),
      otherDescription: faker.lorem.paragraph(2),
    };
  }

  generateValidCosting(): BudgetCostingProps {
    return {
      adaAmount: faker.number.int({ min: 100, max: 10000 }),
      usdToAdaConversionRate: faker.number.int({ min: 1, max: 10 }),
      preferredCurrency: faker.helpers.arrayElement(
        Object.values(PreferredCurrencyEnum)
      ),
      AmountInPreferredCurrency: faker.number.int({ min: 100, max: 10000 }),
      costBreakdown: faker.lorem.paragraph(2),
    };
  }

  generateInValidCosting(): BudgetCostingProps {
    return {
      adaAmount: faker.lorem.paragraph(2),
      usdToAdaConversionRate: faker.lorem.paragraph(2),
      preferredCurrency: faker.helpers.arrayElement(
        Object.values(PreferredCurrencyEnum)
      ),
      AmountInPreferredCurrency: faker.lorem.paragraph(2),
      costBreakdown: invalid.paragraph(150001),
    };
  }

  generateValidFurtherInformation(): Array<ProposalLink> {
    return [
      {
        prop_link: faker.internet.url(),
        prop_link_text: faker.lorem.words(2),
      },
      {
        prop_link: faker.internet.url(),
        prop_link_text: faker.lorem.words(2),
      },
    ];
  }

  generateAdministrationAndAuditing(): AdministrationAndAuditingProps {
    return {
      intersectAdministration: faker.datatype.boolean(),
      venderDetails: faker.lorem.paragraph(2),
    };
  }

  generateValidBudgetProposalInformation(): BudgetProposalProps {
    return {
      proposalOwnership: this.generateValidProposalOwnerShip(),
      problemStatementAndBenefits:
        this.generateValidBudgetProposalProblemStatementAndBenefits(),
      proposalDetails: this.generateValidBudgetProposalDetails(),
      costing: this.generateValidCosting(),
      furtherInformation: this.generateValidFurtherInformation(),
      administrationAndAuditing: this.generateAdministrationAndAuditing(),
    };
  }

  async createDraftBudgetProposal(fillAllDetails = false) {
    const budgetProposal = this.generateValidBudgetProposalInformation();

    if (fillAllDetails) {
      await this.fillupForm(budgetProposal);
    } else {
      await this.fillupProposalOwnershipForm(budgetProposal.proposalOwnership);
    }

    await this.saveDraftBtn.click();
    await this.closeDraftBtn.click();
    await this.cancelBtn.click();
    await this.createBudgetProposalBtn.click();

    return fillAllDetails ? budgetProposal : budgetProposal.proposalOwnership;
  }

  async createBudgetProposal(): Promise<{
    proposalId: number;
    proposalDetails: BudgetProposalProps;
  }> {
    const budgetProposalRequest: BudgetProposalProps =
      this.generateValidBudgetProposalInformation();

    await this.fillupForm(budgetProposalRequest);
    await this.submitBtn.click();

    // assert to check if the proposal is created and navigated to details page
    await expect(this.page.getByTestId("review-version")).toBeVisible({
      timeout: 60_000,
    });

    const currentPageUrl = this.page.url();
    return {
      proposalId: extractProposalIdFromUrl(currentPageUrl),
      proposalDetails: budgetProposalRequest,
    };
  }

  async validateReviewBudgetProposal(
    proposalInformations: BudgetProposalProps
  ) {
    // proposal ownership
    await expect(this.companyTypeContent).toHaveText(
      proposalInformations.proposalOwnership.companyType
    );
    await expect(this.socialHandlesContent).toHaveText(
      proposalInformations.proposalOwnership.contactDetails
    );

    if (
      proposalInformations.proposalOwnership.companyType === CompanyEnum.Company
    ) {
      await expect(this.companyNameContent).toHaveText(
        proposalInformations.proposalOwnership.companyName
      );
      await expect(this.companyDomainNameContent).toHaveText(
        proposalInformations.proposalOwnership.companyDomainName
      );
      await expect(this.countryOfIncorporationContent).toHaveText(
        proposalInformations.proposalOwnership.countryOfIncorportation
      );
    }
    if (
      proposalInformations.proposalOwnership.companyType === CompanyEnum.Group
    ) {
      await expect(this.groupNameContent).toHaveText(
        proposalInformations.proposalOwnership.groupName
      );
      await expect(this.groupTypeContent).toHaveText(
        proposalInformations.proposalOwnership.groupType
      );
      await expect(this.groupKeyIdentityContent).toHaveText(
        proposalInformations.proposalOwnership.groupKeyIdentity
      );
    }

    // problem statement and proposal benefits
    await expect(this.problemStatementContent).toHaveText(
      proposalInformations.problemStatementAndBenefits.problemStatement
    );
    await expect(this.proposalBenefitsContent).toHaveText(
      proposalInformations.problemStatementAndBenefits.proposalBenefits
    );
    await expect(this.roadmapContent).toHaveText(
      proposalInformations.problemStatementAndBenefits.roadmapName
    );
    await expect(this.budgetCategoryContent).toHaveText(
      proposalInformations.problemStatementAndBenefits.budgetDiscussionType
    );
    await expect(this.committeeContent).toHaveText(
      proposalInformations.problemStatementAndBenefits.committeeAlignmentType
    );
    await expect(this.endorsementContent).toHaveText(
      proposalInformations.problemStatementAndBenefits.suplimentaryEndorsement
    );

    // proposal details
    await expect(this.proposalNameContent).toHaveText(
      proposalInformations.proposalDetails.proposalName
    );
    await expect(this.proposalDescriptionContent).toHaveText(
      proposalInformations.proposalDetails.proposalDescription
    );
    await expect(this.proposalKeyDependenciesContent).toHaveText(
      proposalInformations.proposalDetails.proposalKeyDependencies
    );
    await expect(this.proposalMaintainAndSupportContent).toHaveText(
      proposalInformations.proposalDetails.proposalMaintainAndSupport
    );
    await expect(this.proposalDeliverablesContent).toHaveText(
      proposalInformations.proposalDetails.milestones
    );
    await expect(this.resourcingDurationEstimatesContent).toHaveText(
      proposalInformations.proposalDetails.teamSizeAndDuration
    );
    await expect(this.experienceContent).toHaveText(
      proposalInformations.proposalDetails.previousExperience
    );
    await expect(this.contractingContent).toHaveText(
      proposalInformations.proposalDetails.contracting
    );

    // costing
    await expect(this.adaAmountContent).toHaveText(
      `₳ ${formatWithThousandSeparator(proposalInformations.costing.adaAmount)}`
    );
    await expect(this.adaToUsdConversionRateContent).toHaveText(
      proposalInformations.costing.usdToAdaConversionRate.toString()
    );

    const preferredCurrencyShortForm = Object.keys(PreferredCurrencyEnum).find(
      (key) =>
        PreferredCurrencyEnum[key as keyof typeof PreferredCurrencyEnum] ===
        proposalInformations.costing.preferredCurrency
    );

    await expect(this.preferredCurrencyContent).toHaveText(
      preferredCurrencyShortForm
    );
    await expect(this.preferredCurrencyAmountContent).toHaveText(
      formatWithThousandSeparator(
        proposalInformations.costing.AmountInPreferredCurrency
      )
    );
    await expect(this.costBreakdownContent).toHaveText(
      proposalInformations.costing.costBreakdown
    );

    // further information
    for (let i = 0; i < proposalInformations.furtherInformation.length; i++) {
      //BUG missing testId
      await expect(
        this.currentPage.getByRole("link", {
          name: proposalInformations.furtherInformation[i].prop_link_text,
          exact: true,
        })
      ).toBeVisible();
    }

    // administration and auditing
    await expect(this.intersectAdministrationContent).toHaveText(
      proposalInformations.administrationAndAuditing.intersectAdministration ===
        true
        ? "Yes"
        : "No"
    );
  }

  async assertInputTextEquality(
    actualLocator: Locator,
    expected: string,
    isValid: boolean = true
  ) {
    const actual = await actualLocator.inputValue();

    if (isValid) {
      expect(actual, {
        message: actual !== expected && `${actual} is not equal to ${expected}`,
      }).toEqual(expected);
    } else {
      expect(actual, {
        message: actual === expected && `${actual} is equal to ${expected}`,
      }).not.toEqual(expected);
    }
  }

  async validateProposalOwnershipSection(
    proposalOwnership: BudgetProposalOwnershipProps,
    isValid: boolean = true
  ) {
    const companyTypeSelectContent = await this.companyTypeSelect.textContent();

    if (isValid) {
      if (proposalOwnership.companyType === "Company") {
        const countryOfIncorporationBtnContent =
          await this.countryOfIncorporationBtn.textContent();
        await this.assertInputTextEquality(
          this.companyDomainNameInput,
          proposalOwnership.companyDomainName
        );
        await this.assertInputTextEquality(
          this.companyNameInput,
          proposalOwnership.companyName
        );

        expect(countryOfIncorporationBtnContent, {
          message:
            countryOfIncorporationBtnContent !==
              proposalOwnership.countryOfIncorportation &&
            `${countryOfIncorporationBtnContent} is not equal to ${proposalOwnership.countryOfIncorportation}`,
        }).toEqual(proposalOwnership.countryOfIncorportation);
      }
      if (proposalOwnership.companyType === "Group") {
        await this.assertInputTextEquality(
          this.groupNameInput,
          proposalOwnership.groupName
        );
        await this.assertInputTextEquality(
          this.groupTypeInput,
          proposalOwnership.groupType
        );
        await this.assertInputTextEquality(
          this.keyInformationOfGroupInput,
          proposalOwnership.groupKeyIdentity
        );
      }

      expect(companyTypeSelectContent, {
        message:
          companyTypeSelectContent !== proposalOwnership.companyType &&
          `${companyTypeSelectContent} is not equal to ${proposalOwnership.companyType}`,
      }).toEqual(proposalOwnership.companyType);

      await this.assertInputTextEquality(
        this.contactDetailsInput,
        proposalOwnership.contactDetails
      );

      await expect(this.continueBtn).toBeEnabled();
    } else {
      await expect(this.continueBtn).toBeDisabled();
    }
  }

  async validateProblemStatementsAndProposalBenefitsSection(
    isValid: boolean = true
  ) {
    const isFirst = Math.random() > 0.5;

    const problemStatement = generateParagraph(isValid, isFirst);
    const proposalBenefit = generateParagraph(isValid, !isFirst);
    const suplimentaryEndorsement = isValid
      ? faker.lorem.paragraph(2)
      : faker.lorem.paragraph(15001);

    await this.problemStatementInput.fill(problemStatement);
    await this.suplimentaryEndorsementInput.fill(suplimentaryEndorsement);
    await this.proposalBenefitInput.fill(proposalBenefit);

    if (problemStatement.length !== 0) {
      await this.assertInputTextEquality(
        this.problemStatementInput,
        problemStatement,
        isValid
      );
    }

    await this.assertInputTextEquality(
      this.suplimentaryEndorsementInput,
      suplimentaryEndorsement,
      isValid
    );

    if (proposalBenefit.length !== 0) {
      await this.assertInputTextEquality(
        this.proposalBenefitInput,
        proposalBenefit,
        isValid
      );
    }
    if (isValid) {
      await expect(this.continueBtn).toBeEnabled();
    } else {
      await expect(this.continueBtn).toBeDisabled();
    }
  }

  async validateProposalDetailsSection(isValid: boolean = true) {
    const isFirst = Math.random() > 0.5;
    const isSecond = Math.random() > 0.5;

    const proposalName = isValid ? faker.lorem.paragraph(2) : "";
    const proposalDescription = generateParagraph(isValid, isFirst);
    const proposalKeyDependencies = generateParagraph(
      isValid,
      isFirst && isSecond
    );
    const proposalMaintainAndSupport = isValid ? faker.lorem.paragraph(2) : "";
    const milestones = generateParagraph(isValid, !isFirst);
    const teamSizeAndDuration = generateParagraph(
      isValid,
      !isFirst && isSecond
    );
    const previousExperience = isValid ? faker.lorem.paragraph(2) : "";

    const fieldValues = {
      proposalName,
      proposalDescription,
      proposalKeyDependencies,
      proposalMaintainAndSupport,
      milestones,
      teamSizeAndDuration,
      previousExperience,
      contracting: faker.helpers.arrayElement(
        Object.values(ProposalContractingEnum)
      ),
      otherDescription: faker.lorem.paragraph(2),
    };

    await this.fillupProposalDetailsFormWithoutContractingType(fieldValues);

    if (proposalDescription.length !== 0) {
      await this.assertInputTextEquality(
        this.proposalDescriptionInput,
        proposalDescription,
        isValid
      );
    }
    if (proposalKeyDependencies.length !== 0) {
      await this.assertInputTextEquality(
        this.proposalKeyDependenciesInput,
        proposalKeyDependencies,
        isValid
      );
    }

    if (milestones.length !== 0) {
      await this.assertInputTextEquality(
        this.milestonesInput,
        milestones,
        isValid
      );
    }
    if (teamSizeAndDuration.length !== 0) {
      await this.assertInputTextEquality(
        this.teamSizeAndDurationInput,
        teamSizeAndDuration,
        isValid
      );
    }
    if (isValid) {
      await expect(this.continueBtn).toBeEnabled();
    } else {
      await expect(this.continueBtn).toBeDisabled();
    }
  }

  async validateCostingSection(
    costingValue: BudgetCostingProps,
    isValid: boolean = true
  ) {
    await this.fillupCostingFormWithoutPreferredCurrency(costingValue);
    const adaErrorElement = this.page.getByTestId(formErrors.adaAmount);
    const usdToAdaConversionRateErrorElement = this.page.getByTestId(
      formErrors.usdToAdaConversionError
    );
    const preferredCurrencyErrorElement = this.page.getByTestId(
      formErrors.preferredCurrencyError
    );
    const isAdaAmountErrorVisible = await adaErrorElement.isVisible();
    const isUsdToAdaConversionRateErrorVisible =
      await usdToAdaConversionRateErrorElement.isVisible();
    const isPreferredCurrencyErrorVisible =
      await preferredCurrencyErrorElement.isVisible();

    if (isValid) {
      await expect(adaErrorElement, {
        message:
          isAdaAmountErrorVisible &&
          `${costingValue.adaAmount} is an invalid amount`,
      }).toBeHidden();
      await expect(usdToAdaConversionRateErrorElement, {
        message:
          isUsdToAdaConversionRateErrorVisible &&
          `${costingValue.usdToAdaConversionRate} is an invalid amount`,
      }).toBeHidden();
      await expect(preferredCurrencyErrorElement, {
        message:
          isPreferredCurrencyErrorVisible &&
          `${costingValue.preferredCurrency} is an invalid amount`,
      }).toBeHidden();
      await expect(this.continueBtn).toBeEnabled();
    } else {
      await expect(adaErrorElement, {
        message:
          !isAdaAmountErrorVisible &&
          `${costingValue.adaAmount} is a valid amount`,
      }).toBeVisible();
      await expect(usdToAdaConversionRateErrorElement, {
        message:
          !isUsdToAdaConversionRateErrorVisible &&
          `${costingValue.usdToAdaConversionRate} is a valid amount`,
      }).toBeVisible();
      await expect(preferredCurrencyErrorElement, {
        message:
          !isPreferredCurrencyErrorVisible &&
          `${costingValue.preferredCurrency} is a valid amount`,
      }).toBeVisible();
      await expect(this.continueBtn).toBeDisabled();
    }
  }

  async validateFurtherInformationSection(isValid: boolean = true) {
    const url = isValid ? valid.url() : invalid.url();
    const linkText = isValid
      ? faker.internet.displayName()
      : faker.lorem.paragraph(40);

    await this.linkTextInput.fill(linkText);
    await this.linkUrlInput.fill(url);

    const errorElement = this.page.getByTestId(formErrors.link);
    const isErrorVisible = await errorElement.isVisible();

    if (isValid) {
      await expect(errorElement, {
        message: isErrorVisible && `${url} is an invalid URL`,
      }).toBeHidden();
      await expect(this.continueBtn).toBeEnabled();
    } else {
      await expect(errorElement, {
        message: !isErrorVisible && `${url} is a valid URL`,
      }).toBeVisible();
      await expect(this.continueBtn).toBeDisabled();
    }
  }
}
