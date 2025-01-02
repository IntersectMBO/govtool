import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { Page } from "@playwright/test";

export async function fetchFirstActiveDRepDetails(page: Page) {
  let dRepGivenName: string;
  let dRepId: string;
  let dRepDirectoryPage: DRepDirectoryPage;
  await page.route(
    "**/drep/list?page=0&pageSize=10&sort=Random&**",
    async (route) => {
      const response = await route.fetch();
      const json = await response.json();
      const elements = json["elements"].filter(
        (element) => element["givenName"] != null
      );
      dRepGivenName =
        elements[Math.floor(Math.random() * elements.length)]["givenName"];
      dRepId = json["elements"][0]["view"];
      await route.fulfill({
        status: 200,
        contentType: "application/json",
        body: JSON.stringify(json),
      });
    }
  );

  const responsePromise = page.waitForResponse(
    "**/drep/list?page=0&pageSize=10&sort=Random&**"
  );

  dRepDirectoryPage = new DRepDirectoryPage(page);
  await dRepDirectoryPage.goto();
  await dRepDirectoryPage.filterBtn.click();
  await page.getByTestId("Active-checkbox").click();
  await responsePromise;

  await dRepDirectoryPage.searchInput.click();
  return { dRepGivenName, dRepId, dRepDirectoryPage };
}

export async function calculateImageSHA256(imageUrl: string) {
  const toHex = (buffer: ArrayBuffer) => {
    return Array.from(new Uint8Array(buffer))
      .map((byte) => byte.toString(16).padStart(2, "0"))
      .join("");
  };
  try {
    if (imageUrl == "") {
      return "";
    }
    const response = await fetch(imageUrl);
    if (!response.ok) {
      throw new Error(`Failed to fetch image: ${response.statusText}`);
    }
    const arrayBuffer = await response.arrayBuffer();
    const hashBuffer = await crypto.subtle.digest("SHA-256", arrayBuffer);
    return toHex(hashBuffer);
  } catch (error) {
    console.error("Error calculating SHA256:", error);
    return null;
  }
}
