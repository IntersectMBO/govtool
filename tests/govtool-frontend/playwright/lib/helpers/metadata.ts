import { faker } from "@faker-js/faker";
import { valid as mockValid } from "@mock/index";
import { Download } from "@playwright/test";
import metadataBucketService from "@services/metadataBucketService";
const blake = require("blakejs");

import * as fs from "fs";
import path = require("path");

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

function calculateMetadataHash() {
  try {
    // Get the JSON data as a string
    const data = JSON.stringify(mockValid.metadata());

    // Convert the string to a buffer
    const buffer = Buffer.from(data, "utf8");
    const hexDigest = blake.blake2bHex(buffer, null, 32);

    // Parse the JSON data
    const jsonData = JSON.parse(data);
    return { hexDigest, jsonData };
  } catch (error) {
    console.error("Error reading file:", error);
  }
}

export async function uploadMetadataAndGetJsonHash() {
  const { hexDigest: dataHash, jsonData } = calculateMetadataHash();
  const url = await metadataBucketService.uploadMetadata(
    faker.person.firstName(),
    jsonData
  );
  return { dataHash, url };
}
