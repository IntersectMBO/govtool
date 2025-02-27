import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { expect, Page } from "@playwright/test";
import { IDRep } from "@types";
import { bech32 } from "bech32";
import * as crypto from "crypto";
import { functionWaitedAssert } from "./waitedLoop";

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
        (element: IDRep) =>
          element.givenName != null &&
          !element.isScriptBased &&
          element.status == "Active"
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

  await functionWaitedAssert(
    async () => {
      dRepDirectoryPage = new DRepDirectoryPage(page);
      await dRepDirectoryPage.goto();
      await responsePromise;

      await dRepDirectoryPage.searchInput.click();
      await dRepDirectoryPage.searchInput.fill(dRepId);
      await expect(page.getByTestId(`${dRepId}-copy-id-button`)).toBeVisible({
        timeout: 20_000,
      });
    },
    {
      name: "because the selected dRep ID has the 'doNotList' flag set to true",
      message: "DRep not found",
    }
  );
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

export function fromHex(prefix: string, hex: string) {
  return bech32.encode(prefix, bech32.toWords(Buffer.from(hex, "hex")));
}

export function tohex(drepId: string) {
  return Buffer.from(
    bech32.fromWords(bech32.decode(drepId, 100).words)
  ).toString("hex");
}

export function convertDRepToCIP129(drepId: string, script = false): string {
  const hexPattern = /^[0-9a-fA-F]+$/;
  let cip129DRep: string;
  let cip129DrepHex: string;
  const prefix = script ? "23" : "22";
  const addPrefix = (hex: string) => {
    if (hex.length === 56) {
      return prefix + hex;
    } else if (hex.length === 58) {
      return hex;
    } else {
      throw new Error("Invalid DRep hex length");
    }
  };
  const drepIdFromHex = (hex: string) => {
    return fromHex("drep", hex);
  };
  if (hexPattern.test(drepId)) {
    cip129DrepHex = addPrefix(drepId);
  } else {
    try {
      const decodedHex = tohex(drepId);
      cip129DrepHex = addPrefix(decodedHex);
    } catch (error: any) {
      throw new Error("Invalid DRep Bech32 format");
    }
  }
  cip129DRep = drepIdFromHex(cip129DrepHex);
  return cip129DRep;
}
