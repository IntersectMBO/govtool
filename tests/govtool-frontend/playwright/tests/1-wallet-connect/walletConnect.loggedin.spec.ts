import { user01Wallet } from "@constants/staticWallets";
import { test } from "@fixtures/walletExtension";
import { setAllureEpic } from "@helpers/allure";
import LoginPage from "@pages/loginPage";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });
test.beforeEach(async () => {
  await setAllureEpic("1. Wallet connect");
});

test("1B: Should connect wallet with single stake key", async ({ page }) => {
  const loginPage = new LoginPage(page);
  await loginPage.goto();
  await loginPage.isLoggedIn();
});
