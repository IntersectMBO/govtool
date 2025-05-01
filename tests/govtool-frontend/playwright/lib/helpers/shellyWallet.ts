import { bech32 } from "bech32";
import { blake2bHex } from "blakejs";
import convertBufferToHex from "./convertBufferToHex";
import { ShelleyWallet } from "./crypto";
const KEY_HASH_LENGTH = 28;

export default function extractDRepFromWallet(wallet: ShelleyWallet) {
  const dRepPubKey = convertBufferToHex(wallet.dRepKey.public);

  const dRepKeyBytes = Buffer.from(dRepPubKey, "hex");
  const dRepId = blake2bHex(dRepKeyBytes, undefined, 28);
  const words = bech32.toWords(Buffer.from(dRepId, "hex"));
  const dRepIdBech32 = bech32.encode("drep", words);
  return dRepIdBech32;
}

export function rewardAddressRawBytes(network: number, stakePkh: string) {
  const stakePkhBytes = Uint8Array.from(Buffer.from(stakePkh, "hex"));
  const rewardAccountPrefix = 0xe0;
  const header = network | rewardAccountPrefix;
  const result = new Uint8Array(KEY_HASH_LENGTH + 1);
  result[0] = header;
  result.set(stakePkhBytes, 1);
  return result;
}

export function rewardAddressBech32(
  networkId: number,
  stakePkh: string
): string {
  const prefix = networkId == 0 ? "stake" : "stake_test";
  return bech32.encode(
    prefix,
    bech32.toWords(Buffer.from(rewardAddressRawBytes(networkId, stakePkh))),
    200
  );
}

export async function generateWallets(num: number) {
  return await Promise.all(
    Array.from({ length: num }, () =>
      ShelleyWallet.generate().then((wallet) => wallet.json())
    )
  );
}
