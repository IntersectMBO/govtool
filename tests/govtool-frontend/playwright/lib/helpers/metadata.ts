import { faker } from "@faker-js/faker";
import { valid as mockValid } from "@mock/index";
import { Download } from "@playwright/test";
import metadataBucketService from "@services/metadataBucketService";
const blake = require("blakejs");

import * as fs from "fs";
import { ShelleyWallet } from "./crypto";

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

async function calculateMetadataHash() {
  try {
    const paymentAddress = (await ShelleyWallet.generate()).addressBech32(0);
    const data = JSON.stringify(mockValid.metadata(paymentAddress), null, 2);

    const buffer = Buffer.from(data, "utf8");
    const hexDigest = blake.blake2bHex(buffer, null, 32);

    const jsonData = JSON.parse(data);
    return { hexDigest, jsonData };
  } catch (error) {
    console.error("Error reading file:", error);
  }
}

export async function uploadMetadataAndGetJsonHash() {
  const { hexDigest: dataHash, jsonData } = await calculateMetadataHash();
  const url = await metadataBucketService.uploadMetadata(
    faker.person.firstName(),
    jsonData
  );
  return { dataHash, url };
}
