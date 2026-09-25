import environments from "@constants/environments";
import * as fs from "fs";
import { Ed25519KeyAsync } from "libcardano";
import { HdWallet, ShelleyWallet, SimpleCip30Wallet } from "libcardano-wallet";
import * as lockfile from "lockfile";
import path = require("path");
import { blockfrostSubmitter, kuber, queryService } from "./services";

const network = environments.networkId as 0 | 1;

/**
 * Every role is one CIP-1852 account of TEST_WALLET_MNEMONIC, at the run's
 * account base plus the role's position here. A new base each run gives every
 * run wallets with no chain history, and all of them stay recoverable from the
 * mnemonic. Append new roles; reordering moves wallets between roles.
 */
export const WALLET_ROLES = [
  "adaHolder01",
  "adaHolder02",
  "adaHolder03",
  "adaHolder04",
  "adaHolder05",
  "adaHolder06",
  "dRep01",
  "dRep02",
  "dRep03",
  "user01",
] as const;

export type WalletRole = (typeof WALLET_ROLES)[number];

/** Written once per run by the global setup; see hdRunSetup.ts. */
export const HD_RUN_FILE = path.resolve(__dirname, "../_mock/hdRun.json");

const FAUCET_LOCK = path.resolve(__dirname, "../../.faucet.lock");

export function runAccountBase(): number {
  if (process.env.HD_ACCOUNT_BASE) return Number(process.env.HD_ACCOUNT_BASE);
  return JSON.parse(fs.readFileSync(HD_RUN_FILE, "utf-8")).base;
}

let root: Promise<HdWallet> | undefined;

function rootWallet(): Promise<HdWallet> {
  const mnemonic = process.env.TEST_WALLET_MNEMONIC;
  if (!mnemonic) throw new Error("TEST_WALLET_MNEMONIC is not set");
  root ??= HdWallet.fromMnemonicString(mnemonic);
  return root;
}

export async function roleWallet(role: WalletRole): Promise<SimpleCip30Wallet> {
  const account = await (
    await rootWallet()
  ).getAccount(runAccountBase() + WALLET_ROLES.indexOf(role));
  return new SimpleCip30Wallet(
    queryService,
    blockfrostSubmitter,
    await account.singleAddressWallet(0),
    network
  );
}

export async function faucetWallet(): Promise<SimpleCip30Wallet> {
  const payment = await Ed25519KeyAsync.fromPrivateKeyHex(
    environments.faucet.payment.private
  );
  const stake = await Ed25519KeyAsync.fromPrivateKeyHex(
    environments.faucet.stake.private
  );
  return new SimpleCip30Wallet(
    queryService,
    blockfrostSubmitter,
    new ShelleyWallet(payment, stake),
    network
  );
}

export async function lovelaceBalance(
  wallet: SimpleCip30Wallet
): Promise<bigint> {
  const utxos = await kuber.queryUTxOByAddress(
    wallet.wallet.addressBech32(network)
  );
  return utxos.reduce(
    (sum, utxo) => sum + BigInt(utxo.txOut.value.lovelace),
    BigInt(0)
  );
}

/**
 * Builds a transaction through Kuber, signs it in this process, submits it
 * through Blockfrost and waits until Kuber sees it on chain. Returns the tx
 * hash.
 */
export async function buildSignSubmit(
  wallet: SimpleCip30Wallet,
  request: Record<string, unknown>
): Promise<string> {
  const signed = await kuber.buildAndSignWithWallet(wallet, request);
  const txHash = await blockfrostSubmitter.submitTx(
    signed.transaction.toBytes().toString("hex")
  );
  await kuber.waitForTxConfirmation(txHash, environments.txTimeOut);
  return txHash;
}

/**
 * Tops the wallet up from the faucet so it holds at least `ada`. Faucet use
 * is serialized across workers, since concurrent transactions would try to
 * spend the same faucet outputs.
 */
export async function ensureFunded(
  wallet: SimpleCip30Wallet,
  ada: number
): Promise<void> {
  const wanted = BigInt(ada) * BigInt(1_000_000);
  if ((await lovelaceBalance(wallet)) >= wanted) return;

  await withFileLock(FAUCET_LOCK, async () => {
    if ((await lovelaceBalance(wallet)) >= wanted) return;
    await buildSignSubmit(await faucetWallet(), {
      outputs: [
        { address: wallet.wallet.addressBech32(network), value: `${ada}A` },
      ],
    });
  });
}

async function withFileLock<T>(file: string, fn: () => Promise<T>) {
  await new Promise<void>((resolve, reject) =>
    lockfile.lock(file, { wait: 20 * 60_000, pollPeriod: 500 }, (err) =>
      err ? reject(err) : resolve()
    )
  );
  try {
    return await fn();
  } finally {
    await new Promise<void>((resolve) => lockfile.unlock(file, () => resolve()));
  }
}
