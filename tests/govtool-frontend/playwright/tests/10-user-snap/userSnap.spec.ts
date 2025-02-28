import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { isMobile } from "@helpers/mobile";
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
    page.getByRole("button", { name: "Send an idea Let us know what" })
  ).toBeVisible();
});

test("10B. Should verify a bug report form", async ({ page }) => {
  await page
    .getByRole("button", {
      name: "Report an issue Something",
    })
    .click();
  await expect(
    page.getByRole("button", { name: "Close annotation" })
  ).toBeVisible();

  if (isMobile(page)) {
    await expect(
      page.getByRole("button", { name: "Add screenshot" })
    ).toBeVisible();
    await page.getByRole("button", { name: "Close annotation" }).click();
    await expect(page.getByLabel("Take screenshot")).toBeVisible();
  }

  await expect(
    page.getByRole("heading", { name: "Report a bug" })
  ).toBeVisible();
  await expect(page.getByPlaceholder("Add description")).toBeVisible();
  await expect(page.getByLabel("Record")).toBeVisible();
  await expect(page.getByRole("button", { name: "Submit" })).toBeVisible();

  // check record action
  await page.getByLabel("Record").click();
  await expect(
    page.getByRole("button", { name: "Cancel recording" })
  ).toBeVisible();
});

test("10C. Should verify feature form", async ({ page }) => {
  await page
    .getByRole("button", { name: "Send an idea Let us know what" })
    .click();

  await expect(
    page.getByRole("heading", { name: "Feature Request" })
  ).toBeVisible();
  await expect(page.getByPlaceholder("Feature description")).toBeVisible();
  await expect(page.getByPlaceholder("Please add some context")).toBeVisible();
  await expect(page.getByLabel("Take screenshot")).toBeVisible();
  await expect(page.getByLabel("Record")).toBeVisible();
  await expect(
    page.getByRole("button", { name: "Request Feature" })
  ).toBeVisible();

  // check screenshot action
  await page.getByLabel("Take screenshot").click();
  await expect(
    page.getByRole("button", { name: "Close annotation" })
  ).toBeVisible();
  await expect(
    page.getByRole("button", { name: "Add screenshot" })
  ).toBeVisible();
  await page.getByRole("button", { name: "Close annotation" }).click();

  // check record action
  await page.getByLabel("Record").click();
  await expect(
    page.getByRole("button", { name: "Cancel recording" })
  ).toBeVisible();
});

test.describe("Submit Usersnap", () => {
  const feedbackApiUrl =
    "https://widget.usersnap.com/api/widget/xhrrpc?submit_feedback";
  const bucketUrl = "https://s3.eu-central-1.amazonaws.com/upload.usersnap.com";

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

    if (isMobile(page)) {
      await page.getByRole("button", { name: "Add screenshot" }).click();
    } else {
      // Draw rectangle
      await page
        .locator('[class^="jt-container-"] > svg')
        .first()
        .evaluate((element) => {
          const rectangleSvg = `<g class="root-0-0-11 highlight" data-drawing="true" stroke-width="2" stroke="#fed200" x="147" y="16" width="1091" height="653"><rect width="1091" height="653" fill-opacity="0" x="147" y="16"></rect></g>`;
          element.innerHTML = rectangleSvg;
        });
    }

    await page
      .getByPlaceholder("Add description")
      .fill(faker.lorem.paragraph(2));

    await page.getByRole("button", { name: "Submit" }).click();

    await expect(page.getByText("Thank you!")).toBeVisible();
  });

  test("10E. Should submit an idea or new feature", async ({ page }) => {
    // Intercept Usersnap submit API
    await interceptUsersnap(page);
    await interceptBucket(page);

    await page
      .getByRole("button", { name: "Send an idea Let us know what" })
      .click();

    await page
      .getByPlaceholder("Feature description")
      .fill(faker.lorem.words(4));
    await page
      .getByPlaceholder("Please add some context")
      .fill(faker.lorem.paragraph(2));

    // add screenshot
    await page.getByLabel("Take screenshot").click();
    await page.getByRole("button", { name: "Add screenshot" }).click();

    await page.getByRole("button", { name: "Request Feature" }).click();

    await expect(page.getByText("Thank you!")).toBeVisible();
  });
});
