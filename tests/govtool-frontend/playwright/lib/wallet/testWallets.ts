import environments from "@constants/environments";
import { bech32 } from "bech32";
import { createHash } from "crypto";
import * as fs from "fs";
import { Ed25519KeyAsync } from "libcardano";
import { HdWallet, ShelleyWallet, SimpleCip30Wallet } from "libcardano-wallet";
import * as lockfile from "lockfile";
import path = require("path");
import { blockfrostSubmitter, kuber, queryService } from "./services";

const network = environments.networkId as 0 | 1;

/**
 * A test wallet: one CIP-1852 account of TEST_WALLET_MNEMONIC, picked by name.
 * The same name gives the same wallet everywhere in a run, so a setup step and
 * the tests that rely on it share a wallet by agreeing on its name. Keys stay
 * in this process; tests and pages see only the public details.
 */
export type TestWallet = {
  name: string;
  account: number;
  signer: SimpleCip30Wallet;
  address: string;
  /** Bech32 reward address (stake_test1… / stake1…). */
  stakeAddress: string;
  /** Reward address bytes, hex. */
  rewardAddress: string;
  /** CIP-105 DRep id (drep1…). */
  dRepId: string;
  payment: { public: string; pkh: string };
  stake: { public: string; pkh: string };
  dRep: { public: string; pkh: string };
};

/** Written once per run by the global setup; see hdRunSetup.ts. */
export const HD_RUN_FILE = path.resolve(__dirname, "../_mock/hdRun.json");

const FAUCET_LOCK = path.resolve(__dirname, "../../.faucet.lock");

/** The run id that, with a wallet's name, picks its account. */
export function runId(): string {
  if (process.env.HD_RUN_ID) return process.env.HD_RUN_ID;
  return JSON.parse(fs.readFileSync(HD_RUN_FILE, "utf-8")).runId;
}

/**
 * The hardened account index for a name in this run: 31 bits of a hash of the
 * run id and the name. A new run id each run gives every run fresh accounts;
 * collisions across thousands of runs are negligible in 2^31.
 */
export function accountFor(name: string): number {
  const digest = createHash("sha256").update(`${runId()}:${name}`).digest();
  return digest.readUInt32BE(0) & 0x7fffffff;
}

let root: Promise<HdWallet> | undefined;

function rootWallet(): Promise<HdWallet> {
  const mnemonic = process.env.TEST_WALLET_MNEMONIC;
  if (!mnemonic) throw new Error("TEST_WALLET_MNEMONIC is not set");
  root ??= HdWallet.fromMnemonicString(mnemonic);
  return root;
}

const cache = new Map<string, Promise<TestWallet>>();

/** The wallet with this name in this run. */
export function testWallet(name: string): Promise<TestWallet> {
  let wallet = cache.get(name);
  if (!wallet) {
    wallet = deriveTestWallet(name);
    cache.set(name, wallet);
  }
  return wallet;
}

async function deriveTestWallet(name: string): Promise<TestWallet> {
  const account = accountFor(name);
  const shelley = await (
    await rootWallet()
  ).getAccount(account).then((a) => a.singleAddressWallet(0));
  const key = (k: { publicBytes(): Buffer; publicKeyHash(): Buffer }) => ({
    public: k.publicBytes().toString("hex"),
    pkh: k.publicKeyHash().toString("hex"),
  });
  const dRep = key(shelley.dRepKey!);
  return {
    name,
    account,
    signer: new SimpleCip30Wallet(queryService, blockfrostSubmitter, shelley, network),
    address: shelley.addressBech32(network),
    stakeAddress: shelley.stakeAddressBech32(network)!,
    rewardAddress: shelley.stakeAddressBytes(network)!.toString("hex"),
    dRepId: bech32.encode("drep", bech32.toWords(Buffer.from(dRep.pkh, "hex"))),
    payment: key(shelley.paymentKey),
    stake: key(shelley.stakeKey!),
    dRep,
  };
}

/**
 * A new random wallet with no funds and no chain history, for tests that only
 * need a wallet to exist. Not recoverable, so never fund one. `networkId`
 * overrides the network it reports, for the wrong-network test.
 */
export async function randomWallet(
  networkId: 0 | 1 = network
): Promise<SimpleCip30Wallet> {
  return new SimpleCip30Wallet(
    queryService,
    blockfrostSubmitter,
    await ShelleyWallet.generate(),
    networkId
  );
}

/** A random address on the test network, for forms that ask for one. */
export async function randomAddress(): Promise<string> {
  return (await ShelleyWallet.generate()).addressBech32(network);
}

/** A random reward (stake) address on the test network. */
export async function randomStakeAddress(): Promise<string> {
  return (await ShelleyWallet.generate()).stakeAddressBech32(network)!;
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
  wallet: SimpleCip30Wallet | TestWallet | string
): Promise<bigint> {
  const address =
    typeof wallet === "string"
      ? wallet
      : "address" in wallet
        ? wallet.address
        : wallet.wallet.addressBech32(network);
  const utxos = await kuber.queryUTxOByAddress(address);
  return utxos.reduce(
    (sum, utxo) => sum + BigInt(utxo.txOut.value.lovelace),
    BigInt(0)
  );
}

export async function adaBalance(
  wallet: SimpleCip30Wallet | TestWallet | string
): Promise<number> {
  return Number(await lovelaceBalance(wallet)) / 1_000_000;
}

/**
 * Builds a transaction through Kuber, signs it in this process, submits it
 * through Blockfrost and waits until Kuber sees it on chain. Returns the tx
 * hash.
 */
export async function buildSignSubmit(
  wallet: SimpleCip30Wallet | TestWallet,
  request: Record<string, unknown>
): Promise<string> {
  const signer = "signer" in wallet ? wallet.signer : wallet;
  const signed = await kuber.buildAndSignWithWallet(signer, request);
  const txHash = await blockfrostSubmitter.submitTx(
    signed.transaction.toBytes().toString("hex")
  );
  await kuber.waitForTxConfirmation(txHash, environments.txTimeOut);
  return txHash;
}

/**
 * Sends ADA from the faucet. Faucet use is serialized across workers, since
 * concurrent transactions would try to spend the same faucet outputs.
 */
export function sendFromFaucet(
  outputs: { address: string; value: string | number }[]
): Promise<string> {
  return withFileLock(FAUCET_LOCK, async () =>
    buildSignSubmit(await faucetWallet(), { outputs })
  );
}

/** Tops the wallet up from the faucet so it holds at least `ada`. */
export async function ensureFunded(
  wallet: SimpleCip30Wallet | TestWallet,
  ada: number
): Promise<void> {
  const address =
    "address" in wallet ? wallet.address : wallet.wallet.addressBech32(network);
  const wanted = BigInt(ada) * BigInt(1_000_000);
  if ((await lovelaceBalance(address)) >= wanted) return;

  await withFileLock(FAUCET_LOCK, async () => {
    if ((await lovelaceBalance(address)) >= wanted) return;
    await buildSignSubmit(await faucetWallet(), {
      outputs: [{ address, value: `${ada}A` }],
    });
  });
}

/** Runs `fn` holding a lock file, so only one worker at a time runs it. */
export async function withFileLock<T>(file: string, fn: () => Promise<T>) {
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
