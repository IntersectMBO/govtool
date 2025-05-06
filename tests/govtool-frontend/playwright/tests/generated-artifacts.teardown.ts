import environments from "@constants/environments";
import { setAllureEpic, setAllureStory } from "@helpers/allure";
import { test as cleanup } from "@fixtures/walletExtension";
import { deleteFile, deleteFolder } from "@helpers/file";

cleanup.describe.configure({ timeout: environments.txTimeOut });
cleanup.beforeEach(async () => {
  await setAllureEpic("Setup");
  await setAllureStory("Cleanup");
});

cleanup(`Clean up generated files and folders`, async () => {
  const fileToRemove = [
    "registerDRepCopyWallets.json",
    "registerDRepWallets.json",
    "registeredDRepWallets.json",
    "registeredDRepCopyWallets.json",
    "protocolParameter.json",
  ];

  for (const fileName of fileToRemove) {
    await deleteFile(fileName);
  }

  const foldersToRemove = [".auth", ".download"];
  for (const folderName of foldersToRemove) {
    await deleteFolder(folderName);
  }
});
