import environments from "@constants/environments";
import { fa, faker } from "@faker-js/faker";
import { extractProposalIdFromUrl } from "@helpers/string";
import { Page, expect } from "@playwright/test";
import {
  AdministrationAndAuditingProps,
  BudgetCostingProps,
  BudgetDiscussionEnum,
  BudgetProposalContactInformationProps,
  BudgetProposalDetailsProps,
  BudgetProposalOwnershipProps,
  BudgetProposalProblemStatementAndBenefitProps,
  BudgetProposalProps,
  CommitteeAlignmentEnum,
  CompanyEnum,
  LocationEnum,
  PreferredCurrencyEnum,
  ProposalChampionEnum,
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
  amount: "amount-0-text-error",
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
  readonly submitCheckbox = this.page.getByLabel("I consent to the public"); //BUG missing test Ids

  // input
  readonly linkTextInput = this.page.getByTestId("link-0-text-input");
  readonly linkUrlInput = this.page.getByTestId("link-0-url-input");

  // contact-information
  readonly beneficiaryFullNameInput = this.page.getByLabel(
    "Beneficiary Full Name *"
  ); //BUG missing test Ids
  readonly beneficiaryEmailInput = this.page.getByLabel("Beneficiary e-mail *"); //BUG missing test Ids
  readonly submissionLeadFullNameInput = this.page.getByLabel(
    "Submission Lead Full Name *"
  ); //BUG missing test Ids
  readonly submissionLeadEmailInput = this.page.getByLabel(
    "Submission Lead Email *"
  ); //BUG missing test Ids

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
  readonly preferredCurrencyAmountInput = this.page.getByLabel(
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
    "itersect-named-administrator"
  );

  // content
  readonly linkTextContent = this.page.getByTestId("link-0-text-content");
  readonly linkUrlContent = this.page.getByTestId("link-0-url-content");

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

  async fillupContactInformationForm(
    contactInformation: BudgetProposalContactInformationProps
  ) {
    await this.beneficiaryFullNameInput.fill(
      contactInformation.beneficiaryFullName
    );
    await this.beneficiaryEmailInput.fill(contactInformation.beneficiaryEmail);
    await this.beneficiaryCountrySelect.click();
    await this.page
      .getByTestId(
        `${contactInformation.beneficiaryCountry.toLowerCase()}-button`
      )
      .click();
    await this.beneficiaryNationalitySelect.click();
    await this.page
      .getByTestId(
        `${contactInformation.beneficiaryNationality.toLowerCase()}-button`
      )
      .click();

    await this.submissionLeadFullNameInput.fill(
      contactInformation.submissionLeadFullName
    );
    await this.submissionLeadEmailInput.fill(
      contactInformation.submissionLeadEmail
    );

    await this.continueBtn.click();
  }

  async fillupProposalOwnershipForm(
    proposalOwnership: BudgetProposalOwnershipProps
  ) {
    await this.companyTypeSelect.click();
    await this.page
      .getByRole("option", { name: proposalOwnership.companyType })
      .click(); //BUG missing testId

    await this.publicChampionSelect.click();
    await this.page
      .getByRole("option", { name: proposalOwnership.publicChampion })
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
          `${proposalOwnership.countryOfIncorportation.toLowerCase()}-country-of-incorporation-button`
        )
        .click();
    }
    await this.agreeCheckbox.click();

    await this.continueBtn.click();
  }

  async fillupProblemStatementAndBenefitsForm(
    problemStatementAndBenefits: BudgetProposalProblemStatementAndBenefitProps
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

    await this.continueBtn.click();
  }

  async fillupProposalDetailsForm(proposalDetails: BudgetProposalDetailsProps) {
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

    await this.contractingTypeSelect.click();
    await this.page
      .getByTestId(`${proposalDetails.contracting.toLowerCase()}-button`)
      .click();
    if (proposalDetails.contracting === "Other") {
      await this.otherDescriptionInput.fill(proposalDetails.otherDescription);
    }
    await this.continueBtn.click();
  }

  async fillupCostingForm(costing: BudgetCostingProps) {
    await this.adaAmountInput.fill(costing.adaAmount.toString());
    await this.usaToAdaCnversionRateInput.fill(
      costing.usaToAdaCnversionRate.toString()
    );
    await this.preferredCurrencySelect.click();
    await this.page
      .getByTestId(`${costing.preferredCurrency.toLowerCase()}-button`)
      .click();
    await this.preferredCurrencyAmountInput.fill(
      costing.AmountInPreferredCurrency.toString()
    );
    await this.costBreakdownInput.fill(costing.costBreakdown);

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

  async fillupForm(budgetProposal: BudgetProposalProps, stage = 8) {
    await this.fillupContactInformationForm(budgetProposal.contactInformation);

    if (stage > 2) {
      await this.fillupProposalOwnershipForm(budgetProposal.proposalOwnership);
    }

    if (stage > 3) {
      await this.fillupProblemStatementAndBenefitsForm(
        budgetProposal.problemStatementAndBenefits
      );
    }

    if (stage > 4) {
      await this.fillupProposalDetailsForm(budgetProposal.proposalDetails);
    }
    if (stage > 5) {
      await this.fillupCostingForm(budgetProposal.costing);
    }
    if (stage > 6) {
      await this.fillupFurtherInformation(budgetProposal.furtherInformation);
    }
    if (stage > 7) {
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

      await this.submitCheckbox.click();
      await this.continueBtn.click();
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
      beneficiaryCountry: faker.helpers
        .arrayElement(Object.values(LocationEnum))
        .replace(/ /g, "-"),
      beneficiaryNationality: faker.helpers
        .arrayElement(Object.values(LocationEnum))
        .replace(/ /g, "-"),
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
      publicChampion: faker.helpers.arrayElement(
        Object.values(ProposalChampionEnum)
      ),
      contactDetails: faker.internet.email(),
      groupName: faker.company.name(),
      groupType: faker.company.buzzVerb(),
      groupKeyIdentity: faker.lorem.paragraph(2),
      companyName: faker.company.name(),
      companyDomainName: faker.internet.domainName(),
      countryOfIncorportation: faker.helpers
        .arrayElement(Object.values(LocationEnum))
        .replace(/ /g, "-"),
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
      usaToAdaCnversionRate: faker.number.int({ min: 1, max: 100 }),
      preferredCurrency: faker.helpers.arrayElement(
        Object.values(PreferredCurrencyEnum)
      ),
      AmountInPreferredCurrency: faker.number.int({ min: 1, max: 100 }),
      costBreakdown: faker.lorem.paragraph(2),
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
      contactInformation: this.generateValidBudgetProposalContactInformation(),
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
      await this.fillupContactInformationForm(
        budgetProposal.contactInformation
      );
    }

    await this.saveDraftBtn.click();
    await this.closeDraftBtn.click();
    await this.cancelBtn.click();
    await this.createBudgetProposalBtn.click();

    return fillAllDetails ? budgetProposal : budgetProposal.contactInformation;
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
}
