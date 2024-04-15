import { test } from "@fixtures/walletExtension";
import { user01Wallet } from "@constants/staticWallets";
import LoginPage from "@pages/loginPage";

test.use({ storageState: ".auth/user01.json", wallet: user01Wallet });

test("1B: Should connect wallet with single stake key @smoke @fast", async ({
  page,
}) => {
  const loginPage = new LoginPage(page);
  await loginPage.goto();
  await loginPage.isLoggedIn();
});
