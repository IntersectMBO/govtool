import { Buffer } from "buffer";
import blake from "blakejs";
import { bech32 } from "bech32";

import { CardanoApiWallet } from "@models";

export const formHexToBech32 = (dRepID?: string) => {
  if (!dRepID) return;
  const words = bech32.toWords(Buffer.from(dRepID, "hex"));
  const dRepIDBech32 = bech32.encode("drep", words);
  return dRepIDBech32;
};

export const getPubDRepID = async (walletApi: CardanoApiWallet) => {
  try {
    // From wallet get pub DRep key
    const raw = await walletApi.cip95.getPubDRepKey();
    const dRepKey = raw;
    // From wallet's DRep key hash to get DRep ID
    const dRepKeyBytes = Buffer.from(dRepKey, "hex");
    const dRepID = blake.blake2bHex(dRepKeyBytes, undefined, 28);
    // into bech32
    const dRepIDBech32 = formHexToBech32(dRepID);

    return {
      dRepKey,
      dRepID,
      dRepIDBech32,
    };
  } catch (err) {
    console.error(err);
    return {
      dRepKey: undefined,
      dRepID: undefined,
      dRepIDBech32: undefined,
    };
  }
};
