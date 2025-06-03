import environments from "../constants/environments";
import { ed25519 as ed } from "@noble/curves/ed25519";
import { bech32 } from "bech32";
import * as blake from "blakejs";
import {
  addressBech32,
  addressRawBytes,
  rewardAddressBech32,
  rewardAddressRawBytes,
} from "./shellyWallet";

const KEY_HASH_LENGTH = 28;
const ADDR_LENGTH = KEY_HASH_LENGTH * 2 + 1;

// Stores ed25519 KeyPair and hash of publicKey
export class Ed25519Key {
  private: Uint8Array;
  public: Uint8Array;
  pkh: Uint8Array;

  private constructor(priv: Uint8Array, pub: Uint8Array, pkh: Uint8Array) {
    this.private = priv;
    this.public = pub;
    this.pkh = pkh;
  }
  public static async generate() {
    const privKey = ed.utils.randomPrivateKey(); // Secure random private key
    return await Ed25519Key.fromPrivateKey(privKey);
  }

  public static async fromPrivateKey(privKey: Uint8Array) {
    const pubKey = ed.getPublicKey(privKey);
    const pkh = blake.blake2b(pubKey, undefined, KEY_HASH_LENGTH);
    const key = new Ed25519Key(privKey, pubKey, pkh);
    return key;
  }
  public static async fromPrivateKeyHex(privKey) {
    return await Ed25519Key.fromPrivateKey(
      Uint8Array.from(Buffer.from(privKey, "hex"))
    );
  }

  public bech32Pkh(prefix: string = "stake"): string {
    return bech32.encode(prefix, bech32.toWords(this.pkh));
  }
  public bech32PublicKey(prefix: string = "vk_"): string {
    return bech32.encode(prefix, bech32.toWords(this.public));
  }
  public bech32PrivateKey(prefix: string = "sk_"): string {
    return bech32.encode(prefix, bech32.toWords(this.private));
  }
  public async signRaw(message: Uint8Array) {
    return ed.sign(message, this.private);
  }
  public async verify(message, signature) {
    return ed.verify(signature, message, this.public);
  }

  public json() {
    return {
      private: Buffer.from(this.private).toString("hex"),
      public: Buffer.from(this.public).toString("hex"),
      pkh: Buffer.from(this.pkh).toString("hex"),
    };
  }
  public static fromJson(json: any): Ed25519Key {
    if (!json || typeof json !== "object") {
      throw new Error(
        "Invalid JSON format for Ed25519Key: Input must be a non-null object."
      );
    }

    if (!json.private || !json.public || !json.pkh) {
      throw new Error(
        "Invalid JSON format for Ed25519Key: Missing required fields (private, public, or pkh)."
      );
    }

    return new Ed25519Key(
      Uint8Array.from(Buffer.from(json.private, "hex")),
      Uint8Array.from(Buffer.from(json.public, "hex")),
      Uint8Array.from(Buffer.from(json.pkh, "hex"))
    );
  }
}

// Shelley Wallet has 2 ed25519 key pair
// - one for payment purpose
// - one for staking/governance purpose
export class ShelleyWallet {
  paymentKey: Ed25519Key;
  stakeKey: Ed25519Key;
  dRepKey: Ed25519Key;

  public constructor(payment, stake, dRep) {
    this.paymentKey = payment;
    this.stakeKey = stake;
    this.dRepKey = dRep;
  }

  public static async generate() {
    const wallet = new ShelleyWallet(
      await Ed25519Key.generate(),
      await Ed25519Key.generate(),
      await Ed25519Key.generate()
    );
    return wallet;
  }

  addressBech32(networkId: number): string {
    const stakePkh = Buffer.from(this.stakeKey.pkh).toString("hex");
    const paymentPkh = Buffer.from(this.paymentKey.pkh).toString("hex");
    return addressBech32(networkId, paymentPkh, stakePkh);
  }

  addressRawBytes(networkId) {
    const stakePkh = Buffer.from(this.stakeKey.pkh).toString("hex");
    const paymentPkh = Buffer.from(this.paymentKey.pkh).toString("hex");
    return addressRawBytes(networkId, paymentPkh, stakePkh);
  }

  rewardAddressRawBytes(network: number) {
    return rewardAddressRawBytes(network, this.stakeKey.json().pkh);
  }

  rewardAddressBech32(networkId: number): string {
    return rewardAddressBech32(networkId, this.stakeKey.json().pkh);
  }

  dRepIdBech32() {
    const dRepPubKey = Buffer.from(this.dRepKey.public).toString("hex");
    const dRepKeyBytes = Buffer.from(dRepPubKey, "hex");
    const dRepId = blake.blake2bHex(dRepKeyBytes, undefined, 28);
    const words = bech32.toWords(Buffer.from(dRepId, "hex"));
    const dRepIdBech32 = bech32.encode("drep", words);
    return dRepIdBech32;
  }

  public json() {
    return {
      payment: this.paymentKey.json(),
      stake: this.stakeKey.json(),
      dRep: this.dRepKey.json(),
      dRepId: this.dRepIdBech32(),
      address: this.addressBech32(environments.networkId),
    };
  }

  public static fromJson(obj: {
    payment: object;
    stake: object;
    dRep: object;
  }): ShelleyWallet {
    if (!obj || typeof obj !== "object") {
      throw new Error("ShelleyWallet.fromJson: The input must be an object.");
    }

    const paymentKey = obj.payment;
    const stakeKey = obj.stake;
    const dRepKey = obj.dRep;

    if (!paymentKey || typeof paymentKey !== "object") {
      throw new Error(
        "ShelleyWallet.fromJson : Invalid payment key: It must be an object."
      );
    }

    if (!stakeKey || typeof stakeKey !== "object") {
      throw new Error(
        "ShelleyWallet.fromJson : Invalid stake key: It must be an object."
      );
    }
    if (!dRepKey || typeof dRepKey !== "object") {
      throw new Error(
        "ShelleyWallet.fromJson : Invalid dRep key: It must be an object."
      );
    }
    return new ShelleyWallet(
      Ed25519Key.fromJson(paymentKey),
      Ed25519Key.fromJson(stakeKey),
      Ed25519Key.fromJson(dRepKey)
    );
  }
}

export interface Address {
  toBech32(): string;
  toRawBytes(): Uint8Array;
  toRawBytesHex(): string;
}
export class ShelleyWalletAddress implements Address {
  paymentKeyHash: Uint8Array;
  stakeKeyHash: Uint8Array;
  network: number;

  private constructor(
    network: number | "mainnet" | "testnet",
    pkh: Uint8Array,
    skh: Uint8Array
  ) {
    this.network =
      network == "mainnet" ? 1 : network == "testnet" ? 0 : network;
    this.paymentKeyHash = pkh;
    this.stakeKeyHash = skh;
  }
  public static fromRawBytes(bytea: Uint8Array | string | Buffer) {
    let bytebuffer: Buffer;
    if (bytea.length == ADDR_LENGTH * 2 && typeof bytea == "string") {
      bytebuffer = Buffer.from(bytea, "hex");
    } else {
      if (bytea.length !== ADDR_LENGTH) {
        throw Error(
          "ShelleyAddress.fromRawBytes: Invalid byte array length. expected: " +
            ADDR_LENGTH +
            " got: " +
            bytea.length
        );
      }
      bytebuffer = Buffer.from(bytea);
    }

    let paymentKeyHash = bytebuffer.subarray(1, 29);
    let stakeKeyHash = bytebuffer.subarray(29, ADDR_LENGTH);

    return new ShelleyWalletAddress(
      bytebuffer.at(0),
      paymentKeyHash,
      stakeKeyHash
    );
  }
  toBech32(): string {
    const prefix = this.network == 0 ? "addr_test" : "addr";
    return bech32.encode(
      prefix,
      bech32.toWords(Buffer.from(this.toRawBytes())),
      200
    );
  }
  toRawBytes(): Uint8Array {
    const rawBytes = new Uint8Array(ADDR_LENGTH);
    rawBytes[0] = this.network;
    rawBytes.set(this.paymentKeyHash, 1);
    rawBytes.set(this.stakeKeyHash, KEY_HASH_LENGTH + 1);
    return rawBytes;
  }
  toRawBytesHex(): string {
    return Buffer.from(this.toRawBytes()).toString("hex");
  }
}

export const createKeyFromPrivateKeyHex = async (
  privateKeyHex: string
): Promise<Ed25519Key> => {
  return await Ed25519Key.fromPrivateKeyHex(privateKeyHex);
};
