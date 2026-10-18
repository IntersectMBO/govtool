import { valid as mockValid } from "@mock/index";
import { expect, Page } from "@playwright/test";

/** Sets the pdf username through the "setup your username" modal. */
export async function setPdfUsername(page: Page, name: string) {
  await page.getByTestId("username-input").fill(name);

  const proceedBtn = page.getByTestId("proceed-button");
  await proceedBtn.click();
  await proceedBtn.click();

  await page.getByTestId("close-button").click();
}

/**
 * Call right after clicking `verify-user-link`. Test wallets are new each run,
 * so the first pdf sign-in has no username and pdf-ui opens the "setup your
 * username" modal. Waits until the sign-in either opens that modal, which then
 * gets a unique valid username, or completes for a user that already has one.
 * Returns whether a username was set.
 */
export async function setUsernameIfPrompted(
  page: Page,
  timeout = 30_000
): Promise<boolean> {
  const modal = page.getByTestId("setup-username-modal");
  // pdf-ui shows one of these links until the user is signed in and named.
  const pending = page
    .getByTestId("verify-user-link")
    .or(page.getByTestId("create-govtool-display-name-link"));

  let prompted = false;
  await expect(async () => {
    prompted = await modal.isVisible();
    if (!prompted) expect(await pending.count()).toBe(0);
  }, "pdf sign-in did not complete").toPass({ timeout });

  if (prompted) await setPdfUsername(page, mockValid.username());
  return prompted;
}
