import { faker } from "@faker-js/faker";
import { valid as mockValid } from "@mock/index";
import { Download } from "@playwright/test";
import metadataBucketService from "@services/metadataBucketService";
const blake = require("blakejs");

import * as fs from "fs";
import { ShelleyWallet } from "./crypto";
import { calculateImageSHA256 } from "./dRep";
import { imageObject } from "@types";
import environments from "@constants/environments";

export async function downloadMetadata(download: Download): Promise<{
  name: string;
  data: JSON;
}> {
  const path = `.download/${download.suggestedFilename()}`;
  await download.saveAs(path);
  const fileContent = fs.readFileSync(path, "utf-8");
  const jsonData = JSON.parse(fileContent);
  return { name: download.suggestedFilename(), data: jsonData };
}

export function calculateHash(data: string) {
  const buffer = Buffer.from(data, "utf8");
  const hexDigest = blake.blake2bHex(buffer, null, 32);
  return hexDigest;
}

async function calculateMetadataHash() {
  try {
    const paymentAddress = (await ShelleyWallet.generate()).addressBech32(
      environments.networkId
    );
    const imageUrl = faker.image.avatarGitHub();
    const imageSHA256 = (await calculateImageSHA256(imageUrl)) || "";
    const imageObject: imageObject = {
      "@type": "ImageObject",
      contentUrl: imageUrl,
      sha256: imageSHA256,
    };
    const givenName = faker.person.firstName();
    const data = JSON.stringify(
      mockValid.metadata(paymentAddress, imageObject, givenName),
      null,
      2
    );

    const hexDigest = calculateHash(data);

    const jsonData = JSON.parse(data);
    return { hexDigest, jsonData, givenName };
  } catch (error) {
    console.error("Error reading file:", error);
  }
}

export async function uploadMetadataAndGetJsonHash() {
  const {
    hexDigest: dataHash,
    jsonData,
    givenName,
  } = await calculateMetadataHash();
  const url = await metadataBucketService.uploadMetadata(givenName, jsonData);
  return { dataHash, url, givenName };
}

export async function uploadScriptAndGenerateUrl(payload: Object) {
  const data = JSON.stringify(payload);
  const jsonData = JSON.parse(data);
  return await metadataBucketService.uploadMetadata(
    "guardrail-script",
    jsonData
  );
}
