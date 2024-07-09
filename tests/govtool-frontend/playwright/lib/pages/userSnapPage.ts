import { faker } from "@faker-js/faker";
import { Page, expect } from "@playwright/test";
export default class UserSnapPage {
  // buttons
  readonly reportABugBtn = this.page.getByRole("button", {
    name: "Report an issue Something",
  });
  readonly ideaOrNewFeatureBtn = this.page.getByRole("button", {
    name: "Idea or new feature Let us",
  });
  readonly feedbackBtn = this.page.getByTestId("feedback-footer-button");
  readonly submitBtn = this.page.getByRole("button", { name: "Submit" });
  readonly recordBtn = this.page.getByLabel("Record");
  readonly takeScreenshotBtn = this.page.getByLabel("Take screenshot");
  readonly addAttachmentBtn = this.page.getByText("Drag & drop or Browse");

  //inputs
  readonly feedbackInput = this.page.getByPlaceholder("Your feedback");
  readonly emailInput = this.page.getByPlaceholder("someone@something.com");
  readonly ideaOrNewFeatureInput = this.page.getByPlaceholder(
    "Example: New navigation"
  );
  readonly summarizeIdeaInput = this.page.getByLabel(
    "Please summarize your idea or"
  );
  readonly additionalDetailsInput = this.page.getByLabel(
    "Any additional details"
  );

  // modal
  readonly userSnapModal = this.page.getByLabel("Usersnap widget");

  constructor(private readonly page: Page) {}

  async goto() {
    await this.page.goto("/");

    await this.page.waitForTimeout(2_000); // wait until page load properly

    await this.feedbackBtn.click();
  }

  async fillupBugForm() {
    await this.feedbackInput.fill(faker.lorem.paragraph(2));
    await this.fillCommonFields();
  }

  async fillupFeatureForm() {
    await this.ideaOrNewFeatureInput.fill(faker.lorem.words(4));
    await this.summarizeIdeaInput.fill(faker.lorem.paragraph(2));
    await this.additionalDetailsInput.fill(faker.lorem.paragraph(2));
    await this.fillCommonFields();
  }

  async fillCommonFields() {
    const attachmentInputSelector = "input[type=file]";
    await this.page.setInputFiles(attachmentInputSelector, [
      "./lib/_mock/mockAttachment.png",
    ]);
    await this.emailInput.fill(faker.internet.email());
  }
}
