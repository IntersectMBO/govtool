import test, { expect } from "@playwright/test";
import kuberService from "@services/kuberService";
import { ProtocolParams } from "@types";
import { allure } from "allure-playwright";
import { bech32 } from "bech32";

export function lovelaceToAda(lovelace: number) {
  if (lovelace === 0) return 0;

  return lovelace / 1e6;
}

export function generateWalletAddress() {
  const randomBytes = new Uint8Array(10);
  return bech32.encode("addr_test", randomBytes);
}

export async function getProtocolParamsMajorVersion() {
  const protocolParameter: ProtocolParams =
    await kuberService.queryProtocolParams();
  return protocolParameter.protocolVersion.major;
}

export async function skipIfNotHardFork() {
  const currentProtocolVersion = await getProtocolParamsMajorVersion();
  if (currentProtocolVersion < 9) {
    await allure.description(
      "Govtool Features will be available after hardfork."
    );
    test.skip();
  }
}
