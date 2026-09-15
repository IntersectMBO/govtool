import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { expect } from "@playwright/test";

const CHATWOOT_WIDGET_IFRAME = "#chatwoot_live_chat_widget";

test.beforeEach(async ({ page }) => {
  await setAllureEpic("10. Feedback");
  await page.goto("/");
  await page.waitForTimeout(2_000); // wait until page load properly

  await page.getByTestId("feedback-footer-button").click();
});

test("10A. Should open the Chatwoot widget from the feedback button", async ({
  page,
}) => {
  await expect(page.locator(CHATWOOT_WIDGET_IFRAME)).toBeVisible();
});

test("10B. Should display the chat composer inside the widget", async ({
  page,
}) => {
  const widget = page.frameLocator(CHATWOOT_WIDGET_IFRAME);

  await expect(widget.getByPlaceholder(/type your message/i)).toBeVisible();
});

test("10C. Should type a message in the chat composer", async ({ page }) => {
  const widget = page.frameLocator(CHATWOOT_WIDGET_IFRAME);
  const message = faker.lorem.sentence();

  const composer = widget.getByPlaceholder(/type your message/i);
  await composer.fill(message);

  await expect(composer).toHaveValue(message);
});
