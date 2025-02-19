import { bech32 } from "bech32";
import { blake2bHex } from "blakejs";
import convertBufferToHex from "./convertBufferToHex";
import { ShelleyWallet } from "./crypto";

export default function extractDRepFromWallet(wallet: ShelleyWallet) {
  const stakePubKey = convertBufferToHex(wallet.stakeKey.public);

  const dRepKeyBytes = Buffer.from(stakePubKey, "hex");
  const dRepId = blake2bHex(dRepKeyBytes, undefined, 28);
  const words = bech32.toWords(Buffer.from(dRepId, "hex"));
  const dRepIdBech32 = bech32.encode("drep", words);
  return dRepIdBech32;
}

export async function generateWallets(num: number) {
  return await Promise.all(
    Array.from({ length: num }, () =>
      ShelleyWallet.generate().then((wallet) => wallet.json())
    )
  );
}
