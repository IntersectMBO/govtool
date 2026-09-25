import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import LoginPage from "@pages/loginPage";

test.use({ walletName: "user01", walletFundsAda: 0 });
test.beforeEach(async () => {
  await setAllureEpic("1. Wallet connect");
});

test("1B. Should connect wallet with single stake key", async ({ page }) => {
  const loginPage = new LoginPage(page);
  await loginPage.goto();
  await loginPage.isLoggedIn();
});
