import environments from "@constants/environments";
import { blockfrostSubmitTransaction } from "@services/blockfrostService";
import { KuberApiProvider } from "kuber-client";
import { QueryAPIProvider } from "libcardano-wallet";

/**
 * Builds transactions and answers chain queries. It only ever receives
 * addresses and unsigned transactions: signing happens in this process.
 */
export const kuber = new KuberApiProvider(
  environments.kuber.apiUrl,
  environments.kuber.apiKey
);

/**
 * Submits signed transactions through Blockfrost, which relays them to a much
 * larger set of nodes than Kuber's own.
 */
export const blockfrostSubmitter = {
  submitTx: async (txHex: string): Promise<string> =>
    blockfrostSubmitTransaction(Buffer.from(txHex, "hex")),
};

/** Whether a stake address is registered on chain, according to Blockfrost. */
export async function isStakeAddressRegistered(
  stakeAddress: string
): Promise<boolean> {
  const res = await fetch(
    `${environments.blockfrostApiUrl}/v0/accounts/${stakeAddress}`,
    { headers: { project_id: environments.blockfrostApiKey } }
  );
  if (res.status === 404) return false;
  if (!res.ok) {
    throw new Error(`Blockfrost account lookup failed: ${res.status}`);
  }
  // `registered` is the stake registration; `active` only means delegated to
  // a stake pool.
  const account = (await res.json()) as { registered: boolean };
  return account.registered;
}

/**
 * Chain queries for the test wallets: Kuber, plus stake address registration
 * from Blockfrost, which the Kuber API does not serve. The wallet uses the
 * latter to report its stake key as registered or not (CIP-95).
 */
export const queryService: QueryAPIProvider = {
  queryUTxOByAddress: (address) => kuber.queryUTxOByAddress(address),
  queryUTxOByTxIn: (txIn) => kuber.queryUTxOByTxIn(txIn),
  queryProtocolParameters: () => kuber.queryProtocolParameters(),
  queryStakeAddressRegistered: isStakeAddressRegistered,
};
