import DRepDirectoryPage from "@pages/dRepDirectoryPage";
import { expect, Page } from "@playwright/test";
import { IDRep, IDRepInfo } from "@types";
import { bech32 } from "bech32";
import * as crypto from "crypto";
import { functionWaitedAssert } from "./waitedLoop";
import {
  invalid,
  invalid as mockInvalid,
  valid as mockValid,
} from "@mock/index";
import { faker } from "@faker-js/faker";
import { ShelleyWallet } from "./crypto";
import environments from "@constants/environments";

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

export function convertDRep(
  drepId: string,
  script = false
): { cip129: string; cip105: string } {
  const hexPattern = /^[0-9a-fA-F]+$/;
  let cip129DRep: string;
  let cip129DrepHex: string;
  let cip105DRep: string;
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
  const drepIdFromHex = (prefix: string, hex: string) => {
    return fromHex(prefix, hex);
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
  cip105DRep = drepIdFromHex(
    cip129DrepHex.slice(0, 2) == "22" ? "drep" : "drep_script",
    cip129DrepHex.slice(-56)
  );
  cip129DRep = drepIdFromHex("drep", cip129DrepHex);
  return { cip129: cip129DRep, cip105: cip105DRep };
}

export async function generateValidDRepInfo(): Promise<IDRepInfo> {
  return {
    name: mockValid.name(),
    objectives: faker.lorem.paragraph(2),
    motivations: faker.lorem.paragraph(2),
    qualifications: faker.lorem.paragraph(2),
    paymentAddress: (await ShelleyWallet.generate()).addressBech32(
      environments.networkId
    ),
    image: faker.image.avatarGitHub(),
    linksReferenceLinks: [
      {
        url: faker.internet.url(),
        description: faker.internet.displayName(),
      },
    ],
    identityReferenceLinks: [
      {
        url: faker.internet.url(),
        description: faker.internet.displayName(),
      },
    ],
  };
}

export function generateInvalidDRepInfo(): IDRepInfo {
  return {
    name: mockInvalid.name(),
    objectives: faker.lorem.paragraph(40),
    motivations: faker.lorem.paragraph(40),
    qualifications: faker.lorem.paragraph(40),
    paymentAddress: faker.string.alphanumeric(45),
    image: invalid.url(),
    linksReferenceLinks: [
      {
        url: mockInvalid.url(),
        description: faker.lorem.paragraph(40),
      },
    ],
    identityReferenceLinks: [
      {
        url: mockInvalid.url(),
        description: faker.lorem.paragraph(40),
      },
    ],
  };
}
