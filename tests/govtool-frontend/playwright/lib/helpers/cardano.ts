import environments from "@constants/environments";
import test from "@playwright/test";
import kuberService from "@services/kuberService";
import { ProposalType, ProtocolParams } from "@types";
import { allure } from "allure-playwright";
import { bech32 } from "bech32";
import { functionWaitedAssert } from "./waitedLoop";
import { createFile, getFile } from "./file";
import { faucetWallet } from "@constants/staticWallets";

export function lovelaceToAda(lovelace: number) {
  if (lovelace === 0) return 0;

  return lovelace / 1e6;
}

export function generateWalletAddress() {
  const randomBytes = new Uint8Array(10);
  return bech32.encode("addr_test", randomBytes);
}

export async function getProtocolParamsMajorVersion() {
  let protocolParameter = await getFile("protocolParameter.json");
  if (protocolParameter === undefined) {
    await functionWaitedAssert(
      async () => {
        protocolParameter = await kuberService.queryProtocolParams();
        await createFile("protocolParameter.json", protocolParameter);
      },
      { name: "queryProtocolParams" }
    );
  }
  return protocolParameter.protocolVersion.major;
}

export async function isBootStrapingPhase() {
  const protocolParameterMajorVersion = await getProtocolParamsMajorVersion();
  return protocolParameterMajorVersion === 9;
}

export async function skipIfNotInfoAndBootstrapping(type: ProposalType) {
  const isBootStraping = await isBootStrapingPhase();
  if (type !== ProposalType.info && isBootStraping) {
    await allure.description(
      "This Features will be available only after hardfork."
    );
    test.skip();
  }
}

export async function skipIfMainnet() {
  if (environments.networkId === 1) {
    await allure.description(
      "Ada spendable features are not available on mainnet."
    );
    test.skip();
  }
}

export async function skipIfBalanceIsInsufficient(limit = 10) {
  const balance = await getWalletBalance(faucetWallet.address);
  if (balance <= limit) {
    await allure.description("Not enough balance to perform this action.");
    test.skip();
  }
}

export async function getWalletBalance(address: string) {
  let balance: number = 0;
  const faucetWalletBalanceDetails =
    (await getFile("faucetWalletBalance.json")) || {};
  await functionWaitedAssert(
    async () => {
      balance = await kuberService.getBalance(address);
      if (faucetWalletBalanceDetails["address"] === address) {
        balance = faucetWalletBalanceDetails["balance"];
      } else {
        faucetWalletBalanceDetails["balance"] = balance;
        faucetWalletBalanceDetails["address"] = faucetWallet.address;
        await createFile(
          "faucetWalletBalance.json",
          faucetWalletBalanceDetails
        );
      }
    },
    { message: "get balance" }
  );

  return balance;
}
