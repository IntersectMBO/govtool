import { bech32 } from "bech32";
import { blake2bHex } from "blakejs";

export default function extractDRepsFromStakePubKey(stakePubKey: string) {
  const dRepKeyBytes = Buffer.from(stakePubKey, "hex");
  const dRepId = blake2bHex(dRepKeyBytes, undefined, 28);
  const words = bech32.toWords(Buffer.from(dRepId, "hex"));
  const dRepIdBech32 = bech32.encode("drep", words);
  return { dRepId, dRepIdBech32 };
}
