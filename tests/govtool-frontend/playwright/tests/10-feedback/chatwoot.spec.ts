import { faker } from "@faker-js/faker";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import { expect, Page } from "@playwright/test";

const CHATWOOT_WIDGET_IFRAME = "#chatwoot_live_chat_widget";

// The feedback button does nothing until the Chatwoot SDK has loaded.
test.beforeEach(async ({ page }) => {
  await setAllureEpic("10. Feedback");
  await page.goto("/");
  await page.waitForFunction(() => (window as any).$chatwoot?.hasLoaded);

  await page.getByTestId("feedback-footer-button").click();
});

// Depending on the inbox settings (e.g. outside working hours), the widget
// opens on a home screen and the composer appears only after
// "Start Conversation".
async function openChatComposer(page: Page) {
  const widget = page.frameLocator(CHATWOOT_WIDGET_IFRAME);
  const composer = widget.getByPlaceholder(/type your message/i);
  const startConversation = widget.getByText(/start conversation/i);

  await expect(composer.or(startConversation)).toBeVisible();
  if (await startConversation.isVisible()) {
    await startConversation.click();
  }
  return composer;
}

test("10A. Should open the Chatwoot widget from the feedback button", async ({
  page,
}) => {
  await expect(page.locator(CHATWOOT_WIDGET_IFRAME)).toBeVisible();
});

test("10B. Should display the chat composer inside the widget", async ({
  page,
}) => {
  const composer = await openChatComposer(page);

  await expect(composer).toBeVisible();
});

test("10C. Should type a message in the chat composer", async ({ page }) => {
  const message = faker.lorem.sentence();

  const composer = await openChatComposer(page);
  await composer.fill(message);

  await expect(composer).toHaveValue(message);
});
