import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { expect, Page } from "@playwright/test";
import { randomUUID } from "crypto";

test.beforeEach(async ({ page }) => {
  await setAllureEpic("10. User Snap");
  await page.goto("/");
  await page.waitForTimeout(2_000); // wait until page load properly

  await page.getByTestId("feedback-footer-button").click();
});

test("10A. Should open feedback modal", async ({ page }) => {
  await expect(page.getByLabel("Usersnap widget")).toBeVisible();
  await expect(
    page.getByRole("button", {
      name: "Report an issue Something",
    })
  ).toBeVisible();
  await expect(
    page.getByRole("button", {
      name: "Idea or new feature Let us",
    })
  ).toBeVisible();
});

test("10B. Should verify a bug report form", async ({ page }) => {
  await page
    .getByRole("button", {
      name: "Report an issue Something",
    })
    .click();

  await expect(
    page.getByRole("heading", { name: "Report a bug" })
  ).toBeVisible();
  await expect(page.getByPlaceholder("Your feedback")).toBeVisible();
  await expect(page.getByText("Drag & drop or Browse")).toBeVisible();
  await expect(page.getByLabel("Take screenshot")).toBeVisible();
  await expect(page.getByLabel("Record")).toBeVisible();
  await expect(page.getByRole("button", { name: "Submit" })).toBeVisible();
});

test("10C. Should verify feature form", async ({ page }) => {
  await page
    .getByRole("button", {
      name: "Idea or new feature Let us",
    })
    .click();

  await expect(
    page.getByRole("heading", { name: "Idea or new feature" })
  ).toBeVisible();
  await expect(page.getByPlaceholder("Example: New navigation")).toBeVisible();
  await expect(page.getByPlaceholder("Example: New navigation")).toBeVisible();
  await expect(page.getByLabel("Any additional details")).toBeVisible();
  await expect(page.getByText("Drag & drop or Browse")).toBeVisible();
  await expect(page.getByLabel("Please summarize your idea or")).toBeVisible();
  await expect(page.getByLabel("Take screenshot")).toBeVisible();
  await expect(page.getByLabel("Record")).toBeVisible();
  await expect(page.getByRole("button", { name: "Submit" })).toBeVisible();
});

test.describe("Feedback Tests", () => {
  const attachmentInputSelector = "input[type=file]";
  const feedbackApiUrl =
    "https://widget.usersnap.com/api/widget/xhrrpc?submit_feedback";
  const bucketUrl = "https://s3.eu-central-1.amazonaws.com/upload.usersnap.com";
  const mockAttachmentPath = "./lib/_mock/mockAttachment.png";

  const interceptBucket = async (page: Page) => {
    await page.route(bucketUrl, async (route) =>
      route.fulfill({
        status: 204,
      })
    );
  };

  const interceptUsersnap = async (page: Page) => {
    await page.route(feedbackApiUrl, async (route) =>
      route.fulfill({
        status: 200,
        body: JSON.stringify({
          status: true,
          data: {
            feedback: {
              feedback_id: randomUUID(),
              assignee_id: randomUUID(),
              labels: [],
            },
            screen_recording_url: null,
            attachment_urls: [
              {
                url: bucketUrl,
                fields: {
                  "Content-Type": "image/png",
                  key: randomUUID(),
                },
              },
            ],
          },
        }),
      })
    );
  };

  test("10D. Should report an issue", async ({ page }) => {
    // Intercept Usersnap submit API
    await interceptUsersnap(page);
    await interceptBucket(page);
    await page
      .getByRole("button", {
        name: "Report an issue Something",
      })
      .click();

    await page.getByPlaceholder("Your feedback").fill(faker.lorem.paragraph(2));
    await page.setInputFiles(attachmentInputSelector, [mockAttachmentPath]);

    await page.getByRole("button", { name: "Submit" }).click();

    await expect(page.getByText("Thank you!")).toBeVisible();
  });

  test("10E. Should submit an idea or new feature", async ({ page }) => {
    // Intercept Usersnap submit API
    await interceptUsersnap(page);
    await interceptBucket(page);

    await page
      .getByRole("button", {
        name: "Idea or new feature Let us",
      })
      .click();

    await page
      .getByPlaceholder("Example: New navigation")
      .fill(faker.lorem.words(4));
    await page
      .getByLabel("Please summarize your idea or")
      .fill(faker.lorem.paragraph(2));
    await page
      .getByLabel("Any additional details")
      .fill(faker.lorem.paragraph(2));
    await page.setInputFiles(attachmentInputSelector, [mockAttachmentPath]);

    await page.getByRole("button", { name: "Submit" }).click();

    await expect(page.getByText("Thank you!")).toBeVisible();
  });
});
