import environments from "@constants/environments";
import { isStakeAddressRegistered } from "./services";
import {
  buildSignSubmit,
  ensureFunded,
  faucetWallet,
  testWallet,
  TestWallet,
} from "./testWallets";

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

export function registerDRep(
  wallet: TestWallet,
  anchor?: Anchor
): Promise<string> {
  return buildSignSubmit(wallet, {
    certificates: [
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

/** The named wallet, funded and with its stake key registered. */
export async function stakeRegisteredWallet(
  name: string,
  ada = 50
): Promise<TestWallet> {
  const wallet = await testWallet(name);
  await ensureFunded(wallet, ada);
  if (!(await isStakeAddressRegistered(wallet.stakeAddress))) {
    await registerStake(wallet);
  }
  return wallet;
}

/**
 * The named wallet, funded beyond the DRep deposit and registered as a DRep.
 * Registration happens once per run; later calls find it on chain.
 */
export async function registeredDRepWallet(
  name: string,
  { anchor, ada = 600 }: { anchor?: Anchor; ada?: number } = {}
): Promise<TestWallet> {
  const wallet = await testWallet(name);
  await ensureFunded(wallet, ada);
  if (!(await isDRepRegistered(wallet))) {
    await registerDRep(wallet, anchor);
  }
  return wallet;
}
