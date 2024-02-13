import * as blake from "blakejs";
import {
  CIP30Provider,
  Cip95Instance,
  CipMethodOverride,
  WalletParams,
} from "./types";
import { encode, Decoder, Encoder, addExtension } from "cbor-x";
import { ShelleyWallet } from "./crypto";
import walletState from "../../state/walletState";
import {
  cborxDecoder,
  cborxEncoder,
  getPubDRepIds,
  getTxHash,
} from "../../support/utils";
import { Logger } from "../logger/logger";
import kuberService, * as kuber from "../../services/kuberService";
import { KuberValue } from "../../models/types";
import blockfrostService from "../../services/blockfrostService";

class AuxData {
  value: any;
  constructor(value: any) {
    this.value = value;
  }
}
// this is required to override default beehaviour of AuxData
addExtension({
  Class: AuxData,
  tag: 259, // register our own extension code for 259 tag
  encode(instance, encode) {
    return encode(instance.value);
  },
  decode(data: any) {
    return new AuxData(data);
  },
});

/**
 * Creates a CIP-30  and CIP-95 compliant wallet instance.
 *
 * @param {ShelleyWallet=} wallet_ - The Shelley wallet to use. If not provided, a new wallet will be generated.
 * @param {number=} networkId - The network ID for the wallet (0 for testnet, 1 for mainnet ).
 * @param {boolean=} withStakeSigning When set to true, when signing transactions, signature of stakeKey will also be added
 * @returns {Promise<Cip95Instance>} A promise that resolves when the CIP-95 wallet is created.
 */

export async function mkCip95Wallet(
  wallet_?: ShelleyWallet,
  networkId: number = 0,
  withStakeSigning = false,
  onSignTxCallback?: any,
  onWalletInitialized?: any
) {
  const wallet = wallet_ || (await ShelleyWallet.generate());
  const walletAddress = wallet.addressBech32(networkId);
  // Setting wallet state for further use
  onWalletInitialized &&
    onWalletInitialized({
      wallet,
      stakePkh: Buffer.from(wallet.stakeKey.pkh).toString("hex"),
      signingKey: Buffer.from(wallet.paymentKey.private).toString("hex"),
      address: walletAddress,
    });
  console.debug("Wallet address:" + walletAddress);
  const walletAddressRaw = Buffer.from(wallet.addressRawBytes(networkId));
  const walletAddressHex = walletAddressRaw.toString("hex");

  const rewardAddr = Buffer.from(
    wallet.rewardAddressRawBytes(networkId)
  ).toString("hex");
  const stakePublicKey = Buffer.from(wallet.stakeKey.public).toString("hex");
  const dRepDetails = getPubDRepIds(stakePublicKey);
  console.debug("Injected Wallet DRepIds: " + JSON.stringify(dRepDetails));
  const walletInstance: Cip95Instance & {
    experimental: Record<string, CallableFunction>;
  } = {
    getBalance: async () => {
      const apiResponse = await blockfrostService.addresses(walletAddress);
      return Buffer.from(encode(amountToObject(apiResponse.amount))).toString(
        "hex"
      );
    },

    submitTx: async (tx) => {
      console.debug("[CardanoWallet] SubmittingTx: ", tx);
      return await kuberService.submitTransaction(tx).then(async (res) => {
        expect(res.status).eq(200);
        return getTxHash(tx);
        // if (res.status <= 200 || res.status <= 299) {
        //   await blockfrost.submitTransaction(tx).catch(e=>{
        //       console.log("Ignoring transaction submission error on blockfrost")
        //   });
        // }
      });
    },
    getUtxos: (p1, p2) =>
      getUtxosKuber(walletAddress, walletAddressRaw, p1, p2),
    getUsedAddresses: async () => [walletAddressHex],
    getUnusedAddresses: async () => [walletAddressHex],
    getChangeAddress: async () => walletAddressHex,
    getRewardAddresses: async () => [rewardAddr],
    getNetworkId: async () => networkId,
    experimental: {
      on: (eventName, callback) => {
        return;
      },
      off: (eventName, callback) => {
        return;
      },
      getCollateral: () => {
        return "";
      },
    },
    cip95: {
      getPubDRepKey: async () => stakePublicKey,
      getUnregisteredPubStakeKeys: async () => [stakePublicKey],
      getRegisteredPubStakeKeys: async () => [stakePublicKey],
    },
    // CIP-95 -----------------------------
    getActivePubStakeKeys: async () => [rewardAddr],

    signTx: async (tx, partialSign) => {
      // decode transaction body and calculate hash
      let decodedTx = cborxDecoder.decode(Buffer.from(tx, "hex"));
      const reEncodedTx = Buffer.from(cborxEncoder.encode(decodedTx)).toString(
        "hex"
      );

      if (tx != reEncodedTx) {
        console.warn("[CardanoWallet] Re-encoded tx is not same");
        console.warn("[CardanoWallet]   Starting Tx", tx);
        console.warn("[CardanoWallet] Re-Encoded Tx", reEncodedTx);
      }

      const txbody = Uint8Array.from(cborxEncoder.encode(decodedTx[0]));
      const txHash = blake.blake2b(txbody, undefined, 32);
      Logger.success(
        "[CardanoWallet] Signing Tx hash=" + Buffer.from(txHash).toString("hex")
      );

      // sign the transaction hash with payment key
      const paymentKeySig = await wallet.paymentKey.signRaw(txHash);

      // create witness set object
      const witness = new Map();
      const vkeyWitnesses = [[wallet.paymentKey.public, paymentKeySig]];

      // encode witness set to cbor hex and return
      if (withStakeSigning) {
        Logger.success("Signing stake key...");
        const stakeKeySig = await wallet.stakeKey.signRaw(txHash);
        vkeyWitnesses.push([wallet.stakeKey.public, stakeKeySig]);
      }

      witness.set(0, vkeyWitnesses);

      if (onSignTxCallback) {
        onSignTxCallback(Buffer.from(txHash).toString("hex"));
      }
      return Buffer.from(cborxEncoder.encode(witness)).toString("hex");
    },

    signData: async (address, payload) => {
      return "";
    },
    getExtensions: () => [{ cip: 95 }],
  };
  return walletInstance;
}

/**
 * Creates a Cardano Wallet Extension with CIP-95 compliance and optional overrides.
 * the returned object can be injected to `window.cardano.demos` for testing wallet extension feature.
 *
 * @param {CipMethodOverride=} overrides - Optional overrides for CIP-20 and cip95 overrides.
 * @param {ShelleyWallet=} shellyWallet - Optional Shelley wallet to use in the extension.
 * @param {boolean=} withStakeSigning When set to true, when signing transactions, signature of stakeKey will also be added
 * @returns {Promise<CIP30Provider>} A promise that resolves with the created CIP-30 provider.
 */

export async function mkCardanoWalletExtension(
  overrides?: CipMethodOverride,
  walletParams?: WalletParams,
  shellyWallet?: ShelleyWallet
): Promise<CIP30Provider> {
  let extension = await mkCip95Wallet(
    shellyWallet,
    0,
    walletParams?.withStakeSigning,
    walletParams?.onTxSign,
    walletParams?.onWalletInitialized
  );

  let result = { ...extension };

  if (overrides) {
    Object.keys(overrides).forEach((k) => {
      if (result[k]) {
        console.debug(
          "[CardanoWallet] Overriding",
          k,
          "to",
          typeof overrides[k],
          overrides[k]
        );
        result[k] = async () => overrides[k];
      }
      if (result.cip95 && result.cip95[k]) {
        result.cip95[k] = async () => overrides[k];
      }
    });
  }
  let enabled = false;

  return {
    apiVersion: "1.3.1",
    icon: "data:image/svg+xml,%3C%3Fxml version='1.0' encoding='utf-8'%3F%3E%3Csvg viewBox='0 0 500 500' xmlns='http://www.w3.org/2000/svg'%3E%3Crect x='309.36' y='12.441' width='121.115' height='472.347' style='fill: rgb(128  177  211)%3B'/%3E%3Cellipse style='fill: rgb(128  177  211)%3B' cx='231.272' cy='320.966' rx='171.791' ry='137.051'/%3E%3C/svg%3E",

    enable: async function () {
      console.debug("[Cardano Wallet] Enable called");
      enabled = true;
      return result;
    },

    isEnabled: async function () {
      return enabled;
    },

    name: "Demos",

    supportedExtensions: [
      {
        cip: 95,
      },
    ],
  };
}

// convert MultiAsset Value from blockfrost for cbor encoding as per the ledger cddl
function amountToObject(
  amount
): [BigInt, Map<string, Map<string, BigInt>>] | BigInt {
  let lovelace: BigInt;
  let assets: Map<string, Map<string, BigInt>> = new Map();
  amount.forEach((a) => {
    if (a.unit == "lovelace") {
      lovelace = BigInt(a.quantity);
    } else {
      let policy = a.unit.substring(0, 56);
      let tokenName = a.unit.substring(56, a.unit.length);
      if (assets.has(policy)) {
        assets.get(policy).set(tokenName, BigInt(a.quantity));
      } else {
        let quantityMap = new Map();
        quantityMap.set(tokenName, BigInt(a.quantity));
        assets.set(policy, quantityMap);
      }
    }
  });
  if (assets.size > 0) {
    return [lovelace, assets];
  } else {
    return lovelace;
  }
}

const getUtxosBlockfrost = async (
  walletAddress,
  walletAddressRaw,
  amount,
  paginate
) => {
  const apiResponse = await blockfrostService.addressesUtxos(walletAddress);

  return apiResponse.map((utxo, idx) => {
    return Buffer.from(
      cborxEncoder.encode([
        [Buffer.from(utxo.tx_hash, "hex"), utxo.output_index],
        [walletAddressRaw, amountToObject(utxo.amount)],
      ])
    ).toString("hex");
  });
};

const getUtxosKuber = async (
  walletAddress,
  walletAddressRaw,
  amount,
  paginate
) => {
  function kuberValuetoObject(
    value: KuberValue
  ): [bigint, Map<string, Map<string, bigint>>] | bigint {
    const lovelace = BigInt(value.lovelace as bigint | number | string);
    let assets: Map<string, Map<string, bigint>> = new Map();
    for (let policy in value) {
      const assetMap = value[policy] as Record<string, bigint | number>;
      for (let tokenName in assetMap) {
        if (assets.has(policy)) {
          assets.get(policy).set(tokenName, BigInt(assetMap[tokenName]));
        } else {
          let quantityMap = new Map();
          quantityMap.set(tokenName, BigInt(assetMap[tokenName]));
          assets.set(policy, quantityMap);
        }
      }
    }

    if (assets.size > 0) {
      return [lovelace, assets];
    } else {
      return lovelace;
    }
  }

  const apiResponse = await kuberService.queryUtxos(walletAddress);
  let result = [];
  return apiResponse.map((utxo) => {
    const txin = utxo.txin.split("#");
    return Buffer.from(
      cborxEncoder.encode([
        [Buffer.from(txin[0], "hex"), BigInt(txin[1])],
        [walletAddressRaw, kuberValuetoObject(utxo.value)],
      ])
    ).toString("hex");
  });
};
