import { ShelleyWallet } from "@mock/cardano-test-wallet/crypto";
import * as blake from "blakejs";
import environments from "lib/constants/environments";
import { KuberValue } from "@types";
import fetch, { BodyInit, RequestInit } from "node-fetch";
import { faucetWallet } from "@constants/staticWallets";
import { cborxDecoder, cborxEncoder } from "../helpers/cborEncodeDecode";
import convertBufferToHex from "../helpers/convertBufferToHex";

type CertificateType = "registerstake" | "registerdrep" | "deregisterdrep";

type KuberBalanceResponse = {
  address: string;
  txin: string;
  value: KuberValue;
};

const config = {
  apiUrl: environments.kuber.apiUrl,
  apiKey: environments.kuber.apiKey,
};

class Kuber {
  walletAddr: string;
  signingKey: string;
  version: string;

  constructor(walletAddr: string, signingKey: string, version = "v1") {
    this.walletAddr = walletAddr;
    this.signingKey = signingKey;
    this.version = version;
  }
  static generateCert(type: CertificateType, key: string) {
    if (type === "registerstake" || type === "deregisterdrep") {
      return {
        type: type,
        key: key,
      };
    } else if (type === "registerdrep") {
      return {
        type: "registerdrep",
        key: key,
        anchor: {
          url: "https://bit.ly/3zCH2HL",
          dataHash:
            "1111111111111111111111111111111111111111111111111111111111111111",
        },
      };
    }
  }
  signTx(tx: any) {
    return {
      ...tx,
      selections: [
        ...(tx.selections || []),
        {
          type: "PaymentSigningKeyShelley_ed25519",
          description: "Payment Signing Key",
          cborHex: "5820" + this.signingKey,
        },
        this.walletAddr,
      ],
      changeAddress: this.walletAddr,
    };
  }

  async signAndSubmitTx(tx: any) {
    const signedTx = this.signTx(tx);
    const res = (await callKuber(
      `/api/${this.version}/tx?submit=true`,
      "POST",
      JSON.stringify(signedTx),
    )) as any;
    console.debug("[Kuber] Tx: " + JSON.stringify(res));
    let decodedTx = cborxDecoder.decode(Buffer.from(res.cborHex, "hex"));
    const submittedTxBody = Uint8Array.from(cborxEncoder.encode(decodedTx[0]));
    const submittedTxHash = Buffer.from(
      blake.blake2b(submittedTxBody, undefined, 32),
    ).toString("hex");
    return { cbor: res.cborHex, txId: submittedTxHash };
  }
}

const kuberService = {
  initializeWallets: (
    senderAddress: string,
    signingKey: string,
    wallets: ShelleyWallet[],
  ) => {
    const kuber = new Kuber(senderAddress, signingKey);
    const outputs = [];
    const stakes = [];
    const certificates = [];
    for (let i = 0; i < wallets.length; i++) {
      const wallet = wallets[i];
      const address = wallet.addressBech32(0);
      outputs.push({
        address: address,
        value: 0,
      });
      stakes.push({
        type: "PaymentSigningKeyShelley_ed25519",
        description: "Payment Signing Key",
        cborHex: "5820" + convertBufferToHex(wallet.stakeKey.private),
      });
      certificates.push(
        Kuber.generateCert(
          "registerstake",
          convertBufferToHex(wallet.stakeKey.pkh),
        ),
      );
    }
    return kuber.signAndSubmitTx({
      selections: [...stakes],
      outputs,
      certificates,
    });
  },

  submitTransaction(tx: any) {
    return fetch(config.apiUrl + "/api/v1/tx/submit", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "api-key": config.apiKey,
      },

      body: JSON.stringify({
        tx: {
          description: "",
          type: "Tx ConwayEra",
          cborHex: tx,
        },
      }),
      redirect: "follow",
    });
  },
  transferADA: (
    senderAddress: string,
    receiverAddressList: string[],
    signingKey?: string, // private payment key
    ADA = 20,
  ) => {
    const kuber = new Kuber(senderAddress, signingKey);
    const req = {
      outputs: receiverAddressList.map((addr) => {
        return {
          address: addr,
          value: `${ADA}A`,
        };
      }),
    };
    return kuber.signAndSubmitTx(req);
  },

  dRepRegistration: (addr: string, signingKey: string, pkh: string) => {
    const kuber = new Kuber(addr, signingKey);
    const req = {
      certificates: [Kuber.generateCert("registerdrep", pkh)],
    };
    return kuber.signAndSubmitTx(req);
  },

  dRepDeRegistration: (
    addr: string,
    signingKey: string,
    stakePrivateKey: string,
    pkh: string,
  ) => {
    const kuber = new Kuber(addr, signingKey);
    const selections = [
      {
        type: "PaymentSigningKeyShelley_ed25519",
        description: "Payment Signing Key",
        cborHex: "5820" + stakePrivateKey,
      },
    ];
    const req = {
      selections,
      inputs: addr,
      certificates: [Kuber.generateCert("deregisterdrep", pkh)],
    };
    return kuber.signAndSubmitTx(req);
  },

  stakeDelegation: (
    addr: string,
    signingKey: string,
    stakePrivateKey: string,
    pkh: string,
    dRep: string | "abstain" | "noconfidence",
  ) => {
    const kuber = new Kuber(addr, signingKey);
    const selections = [
      {
        type: "PaymentSigningKeyShelley_ed25519",
        description: "Payment Signing Key",
        cborHex: "5820" + stakePrivateKey,
      },
    ];
    const req = {
      selections,
      certificates: [
        {
          type: "delegate",
          key: pkh,
          drep: dRep,
        },
      ],
    };
    return kuber.signAndSubmitTx(req);
  },

  getBalance: async (addr: string) => {
    const utxos: any[] = await callKuber(`/api/v3/utxo?address=${addr}`);
    const balanceInLovelace = utxos.reduce(
      (acc, utxo) => acc + utxo.value.lovelace,
      0,
    );
    return balanceInLovelace / 1000000;
  },

  registerStake: (
    stakePrivateKey: string,
    pkh: string,
    signingKey: string,
    addr: string,
  ) => {
    const kuber = new Kuber(addr, signingKey);
    const selections = [
      {
        type: "PaymentSigningKeyShelley_ed25519",
        description: "Payment Signing Key",
        cborHex: "5820" + stakePrivateKey,
      },
    ];
    const req = {
      selections,
      certificates: [Kuber.generateCert("registerstake", pkh)],
    };
    return kuber.signAndSubmitTx(req);
  },

  createGovAction(proposalsCount = 2) {
    const kuber = new Kuber(faucetWallet.address, faucetWallet.payment.private);
    const infoProposal = {
      deposit: 1000000000,
      refundAccount: {
        network: "Testnet",
        credential: {
          "key hash":
            "db1bc3c3f99ce68977ceaf27ab4dd917123ef9e73f85c304236eab23",
        },
      },
      anchor: {
        url: "https://bit.ly/3zCH2HL",
        dataHash:
          "1111111111111111111111111111111111111111111111111111111111111111",
      },
    };
    const req = kuber.signTx({
      proposals: Array.from({ length: proposalsCount }, (_, i) => infoProposal),
    });
    return callKuber("/api/v1/tx?submit=true", "POST", JSON.stringify(req));
  },

  getTransactionDetails(txHash: string) {
    return fetch(config.apiUrl + "/api/v3/utxo?txin=" + txHash + "%230", {
      method: "GET",
      headers: {
        "Content-Type": "application/json",
        "api-key": config.apiKey,
      },
    });
  },

  queryUtxos(address: string): Promise<[KuberBalanceResponse]> {
    return callKuber("/api/v3/utxo?address=" + address) as Promise<
      [KuberBalanceResponse]
    >;
  },

  voteOnProposal(
    addr: string,
    signingKey: string,
    voter: string, // dRepHash
    dRepStakePrivKey: string,
    proposal: string,
  ) {
    const kuber = new Kuber(addr, signingKey);
    const req = {
      selections: [
        {
          type: "PaymentSigningKeyShelley_ed25519",
          description: "Payment Signing Key",
          cborHex: "5820" + dRepStakePrivKey,
        },
      ],
      vote: {
        voter,
        role: "drep",
        proposal,
        vote: true,
        anchor: {
          url: "https://bit.ly/3zCH2HL",
          dataHash:
            "1111111111111111111111111111111111111111111111111111111111111111",
        },
      },
    };
    return kuber.signAndSubmitTx(req);
  },

  abstainDelegations(): Promise<{ txId: string }> {
    // TODO Implement multiple abstain delegations
    return Promise.resolve({
      txId: "",
    });
  },
};
async function callKuber(
  path: any,
  method: "GET" | "POST" = "GET",
  body?: BodyInit,
  contentType = "application/json",
) {
  const url = config.apiUrl + path;

  const headers: Record<string, string> = {
    "api-key": config.apiKey,
  };
  if (contentType) {
    headers["content-type"] = contentType;
  }

  const options: RequestInit = {
    method,
    headers,
  };

  if (method === "POST") {
    if (body) options.body = body;
  }

  return fetch(url, options).then(async (res) => {
    if (res.status === 200) {
      return res.json();
    } else {
      return res.text().then((txt) => {
        let err;
        let json: any;
        try {
          json = JSON.parse(txt);
          if (json) {
            err = Error(
              `KuberApi [Status ${res.status}] : ${
                json.message ? json.message : txt
              }`,
            );
          } else {
            err = Error(`KuberApi [Status ${res.status}] : ${txt}`);
          }
        } catch (e) {
          err = Error(`KuberApi [Status ${res.status}] : ${txt}`);
        }
        err.status = res.status;
        throw err;
      });
    }
  });
}

export default kuberService;
