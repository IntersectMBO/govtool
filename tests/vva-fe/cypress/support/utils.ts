import { blake2b, blake2bHex } from "blakejs";
import { randomBytes } from "crypto";
import { bech32 } from "bech32";
import { Ed25519Key, ShelleyWallet } from "../lib/wallet/crypto";
import { Logger } from "../lib/logger/logger";
import { CipMethodOverride, WalletParams } from "../lib/wallet/types";
import { Decoder, Encoder } from "cbor-x";
import walletState from "../state/walletState";

export const cborxEncoder = new Encoder({
  mapsAsObjects: false,
  useRecords: false,
});
export const cborxDecoder = new Decoder({ mapsAsObjects: false });

export function injectOpen() {
  let a = { args: [] };
  cy.window().then((w: any) => {
    w.open = (...args) => {
      a.args = args;
    };
  });
  return a;
}

export function convertUint8ArrayToHex(buffer: Uint8Array) {
  return Buffer.from(buffer).toString("hex");
}
export function random32BytesHex() {
  return randomBytes(32).toString("hex");
}

export function generateDRep() {
  const dRepKey = randomBytes(32);
  return loadDRepFromKey(dRepKey);
}

export function loadDRepFromKey(dRepKey?: any) {
  const dRepKeyHash = blake2b(dRepKey, undefined, 28);
  return {
    id: bech32.encode("drep", bech32.toWords(dRepKeyHash)),
    key: dRepKey.toString("hex"),
  };
}

export function getWebAppNetworkId(): number {
  const networkId: string = Cypress.env("networkId");
  //@ts-ignore
  return networkId.toLowerCase() == "testnet" || networkId == 0 ? 0 : 1;
}

export const isMobile = () => {
  return (
    Cypress.config("viewportWidth") <
    Cypress.env("mobileViewportWidthBreakpoint")
  );
};

export const removeWhiteSpaces = (str: string) => str.replace(/\s+/g, "");

export const groupElements = (elements: any[], key?: string) => {
  const groupedElements = {};

  elements.forEach((element) => {
    const elementType = key ? element[`${key}`] : element.type;

    if (!groupedElements[elementType]) {
      groupedElements[elementType] = [];
    }
    groupedElements[elementType].push({
      ...element,
      txHash: element.txHash,
    });
  });

  return groupedElements;
};

// export const loadDRep = async (type: "dRep" | "adaHolder") => {
//   const stakePrivKeyHex =
//     type === "dRep"
//       ? dRepWalletDetails.stake.private
//       : adaHolderWalletDetails.stake.private;
//   const stakeKey = await Ed25519Key.fromPrivateKeyHex(stakePrivKeyHex);
//   const dRepKey = Buffer.from(stakeKey.pkh).toString("hex");
//   return loadDRepFromKey(dRepKey);
// };

export const getDRepIDHash = (dRepIdBech32: string) => {
  const decodedDRepId = bech32.decode(dRepIdBech32);
  const dRepIDHex = Buffer.from(bech32.fromWords(decodedDRepId.words)).toString(
    "hex"
  );
  Logger.success("Decoded DRepId Hex: " + dRepIDHex);
  return dRepIDHex;
};

export const getPubDRepIds = (dRepKey: string) => {
  const dRepKeyBytes = Buffer.from(dRepKey, "hex");
  const dRepID = blake2bHex(dRepKeyBytes, undefined, 28);
  const words = bech32.toWords(Buffer.from(dRepID, "hex"));
  const dRepIdBech32 = bech32.encode("drep", words);
  return { dRepID, dRepIdBech32 };
};

export function destructWalletArgs(args: CipMethodOverride & WalletParams) {
  args = {
    ...args,
    onTxSign: (txId) => {
      walletState.setTxHash(txId);
    },
    onWalletInitialized: ({ wallet, stakePkh, signingKey, address }) => {
      walletState.setWallet(wallet);
      walletState.setStakePkh(stakePkh);
      walletState.setSigningKey(signingKey);
      walletState.setAddress(address);

      Logger.success("[Wallet State] Wallet state saved.");
    },
  };
  const overrides: CipMethodOverride = {};
  const walletParams: WalletParams = {
    loadFundsAndRegister: null,
    onTxSign: null,
    withStakeSigning: false,
    onWalletInitialized: null,
  };

  args &&
    Object.keys(args).forEach((key) => {
      if (key in walletParams) {
        walletParams[key] = args[key];
      } else {
        overrides[key] = args[key];
      }
    });
  console.log("overrides: " + JSON.stringify(overrides));
  console.log("walletParams: " + JSON.stringify(walletParams));
  return { overrides, walletParams };
}

export function saveWallets(wallets: ShelleyWallet[]) {
  const jsonWallets = [];
  for (let i = 0; i < wallets.length; i++) {
    jsonWallets.push({
      ...wallets[i].json(),
      address: wallets[i].addressBech32(0),
    });
  }
  cy.writeFile("cypress/fixtures/wallets.json", jsonWallets, { flag: "a" });
}

export function getTxHash(tx: string) {
  let decodedTx = cborxDecoder.decode(Buffer.from(tx, "hex"));
  const txbody = Uint8Array.from(cborxEncoder.encode(decodedTx[0]));
  return blake2bHex(txbody, undefined, 32);
}

export const getShortenedGovActionId = (txHash: string, index: number) => {
  const firstPart = txHash.slice(0, 4);
  const lastPart = txHash.slice(-4);

  return `${firstPart}...${lastPart}#${index}`;
};
