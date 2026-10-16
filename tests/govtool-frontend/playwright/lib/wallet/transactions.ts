import environments from "@constants/environments";
import { uploadMetadataAndGetJsonHash } from "@helpers/metadata";
import { isStakeAddressRegistered } from "./services";
import {
  buildSignSubmit,
  ensureFunded,
  faucetWallet,
  testWallet,
  TestWallet,
  withFileLock,
} from "./testWallets";
import path = require("path");

/**
 * Governance transactions for test wallets. Each is built by Kuber from the
 * wallet's UTxOs, signed in this process (libcardano-wallet adds the stake or
 * DRep witness a certificate needs) and submitted through Blockfrost.
 */

export type Anchor = { url: string; dataHash: string };

export function registerStake(wallet: TestWallet): Promise<string> {
  return buildSignSubmit(wallet, {
    certificates: [{ type: "registerstake", key: wallet.stake.pkh }],
  });
}

/**
 * Registers the wallet's DRep key, with the anchor if given. `withStake` also
 * registers the stake key in the same transaction.
 */
export function registerDRep(
  wallet: TestWallet,
  anchor?: Anchor,
  { withStake = false }: { withStake?: boolean } = {}
): Promise<string> {
  return buildSignSubmit(wallet, {
    certificates: [
      ...(withStake ? [{ type: "registerstake", key: wallet.stake.pkh }] : []),
      {
        type: "registerdrep",
        key: wallet.dRep.pkh,
        ...(anchor && { anchor }),
      },
    ],
  });
}

export function deregisterDRep(wallet: TestWallet): Promise<string> {
  // The deposit refund can pay the fee on its own, and Kuber would then pick
  // no input; a transaction must spend at least one.
  return buildSignSubmit(wallet, {
    inputs: wallet.address,
    certificates: [{ type: "deregisterdrep", key: wallet.dRep.pkh }],
  });
}

/** Delegates voting power to a DRep id, or to "abstain" / "noconfidence". */
export function delegateVote(
  wallet: TestWallet,
  dRep: string | "abstain" | "noconfidence"
): Promise<string> {
  return buildSignSubmit(wallet, {
    certificates: [{ type: "delegate", key: wallet.stake.pkh, drep: dRep }],
  });
}

/** Sends everything the wallet holds back to the faucet. */
export async function sweepToFaucet(wallet: TestWallet): Promise<string> {
  return buildSignSubmit(wallet, {
    inputs: wallet.address,
    changeAddress: (await faucetWallet()).wallet.addressBech32(
      environments.networkId
    ),
  });
}

/** Whether the wallet's DRep key is registered and not retired (Blockfrost). */
export async function isDRepRegistered(wallet: TestWallet): Promise<boolean> {
  const res = await fetch(
    `${environments.blockfrostApiUrl}/v0/governance/dreps/${wallet.dRepId}`,
    { headers: { project_id: environments.blockfrostApiKey } }
  );
  if (res.status === 404) return false;
  if (!res.ok) throw new Error(`Blockfrost DRep lookup failed: ${res.status}`);
  const dRep = (await res.json()) as { retired: boolean };
  return !dRep.retired;
}

const LOCK_DIR = path.resolve(__dirname, "../..");

/**
 * Registers the wallet's stake key unless it is already registered. Serialized
 * per wallet across workers, as two concurrent registrations of one key would
 * fail. The wallet must hold the deposit and fee.
 */
export function ensureStakeRegistered(wallet: TestWallet): Promise<void> {
  return withStakeLock(wallet, async () => {
    if (!(await isStakeAddressRegistered(wallet.stakeAddress))) {
      await registerStake(wallet);
      await waitForStakeIndexed(wallet);
    }
  });
}

/**
 * Waits until Blockfrost reports the stake key registered. Kuber confirms the
 * transaction from the node a few seconds before Blockfrost indexes it, and the
 * page wallet answers each CIP-95 stake key call with its own Blockfrost read:
 * a page that connects inside that window can see the key as neither
 * registered nor unregistered, and GovTool then selects no stake key.
 */
async function waitForStakeIndexed(wallet: TestWallet): Promise<void> {
  const deadline = Date.now() + environments.txTimeOut;
  while (!(await isStakeAddressRegistered(wallet.stakeAddress))) {
    if (Date.now() > deadline) {
      throw new Error(
        `Stake key of ${wallet.name} registered, but not indexed by Blockfrost`
      );
    }
    await new Promise((resolve) => setTimeout(resolve, 2_000));
  }
}

function withStakeLock<T>(wallet: TestWallet, fn: () => Promise<T>) {
  const name = wallet.name.replace(/[^\w@:-]/g, "_");
  return withFileLock(
    path.join(LOCK_DIR, `.stakeRegistration-${name}.lock`),
    fn
  );
}

/**
 * The named wallet, funded and with its stake key registered, as a wallet
 * that has been used on chain before. The app then adds no stake registration
 * to its transactions.
 */
export async function stakeRegisteredWallet(
  name: string,
  ada = 50
): Promise<TestWallet> {
  const wallet = await testWallet(name);
  await ensureFunded(wallet, ada);
  await ensureStakeRegistered(wallet);
  return wallet;
}

/**
 * The named wallet, funded beyond the DRep deposit and registered as a DRep.
 * Registration happens once per run; later calls find it on chain.
 *
 * Without an anchor it registers with fresh CIP-119 metadata. `soleVoter`
 * registers without one instead: GovTool treats a DRep registered without
 * metadata as a Direct Voter, whose dashboard and flows differ (no retire-button,
 * delegating first retires it).
 *
 * The stake key is registered too unless `stakeRegistered` is false, as the
 * old static DRep wallets had it; the DRep registration and the stake
 * registration go in one transaction when both are missing.
 */
export async function registeredDRepWallet(
  name: string,
  {
    anchor,
    ada = 600,
    soleVoter = false,
    stakeRegistered = true,
  }: {
    anchor?: Anchor;
    ada?: number;
    soleVoter?: boolean;
    stakeRegistered?: boolean;
  } = {}
): Promise<TestWallet> {
  const wallet = await testWallet(name);
  await ensureFunded(wallet, ada);
  // Under the stake lock, so a concurrent ensureStakeRegistered of this wallet
  // cannot submit a second registration.
  await withStakeLock(wallet, async () => {
    const withStake =
      stakeRegistered && !(await isStakeAddressRegistered(wallet.stakeAddress));
    if (!(await isDRepRegistered(wallet))) {
      if (!anchor && !soleVoter) {
        const { url, dataHash } = await uploadMetadataAndGetJsonHash();
        anchor = { url, dataHash };
      }
      await registerDRep(wallet, anchor, { withStake });
    } else if (withStake) {
      await registerStake(wallet);
    }
    if (withStake) await waitForStakeIndexed(wallet);
  });
  return wallet;
}
