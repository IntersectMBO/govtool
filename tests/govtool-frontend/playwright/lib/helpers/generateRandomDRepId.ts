import { bech32 } from "bech32";
import { blake2b } from "blakejs";
import { randomBytes } from "crypto";

export default function generateRandomDRepId() {
  const dRepKey = randomBytes(32);
  const dRepKeyHash = blake2b(dRepKey, undefined, 28);

  return bech32.encode("drep", bech32.toWords(dRepKeyHash));
}
