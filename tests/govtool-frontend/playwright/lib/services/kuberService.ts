import {
  KuberValue,
  ProtocolParams,
  StaticWallet,
  WalletAndAnchorType,
} from "@types";
import * as blake from "blakejs";
import environments from "@constants/environments";
import { LockInterceptor, LockInterceptorInfo } from "lib/lockInterceptor";
import fetch, { BodyInit, RequestInit } from "node-fetch";
import { cborxEncoder } from "@helpers/encodeDecode";
import { Logger } from "@helpers/logger";
import { blockfrostSubmitTransaction } from "@services/blockfrostService";
import { getWalletConfigForFaucet } from "@helpers/index";

type CertificateType = "registerstake" | "registerdrep" | "deregisterdrep";

export type TxSubmitResponse = {
  cbor: string;
  txId: string;
  lockInfo?: LockInterceptorInfo;
};

type KuberBalanceResponse = {
  txin: string;
  value: KuberValue;
  address?: string;
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

  static generateCert(
    type: CertificateType,
    key: string,
    metadata?: WalletAndAnchorType
  ) {
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
          url: metadata?.url || "",
          dataHash: metadata?.dataHash || "",
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
    const signedTxBody = Uint8Array.from(cborxEncoder.encode(tx));
    const lockId = Buffer.from(
      blake.blake2b(signedTxBody, undefined, 32)
    ).toString("hex");
    const submitTxCallback = async () => {
      return this.submitTx(signedTx, lockId);
    };
    return LockInterceptor.intercept<TxSubmitResponse>(
      this.walletAddr,
      submitTxCallback
    );
  }

  async submitTx(signedTx: any, lockId?: string) {
    Logger.info(
      `Submitting tx: ${JSON.stringify({ lock_id: lockId, tx: signedTx })}`
    );

    const response = (await callKuber(
      `/api/${this.version}/tx?submit=false`,
      "POST",
      JSON.stringify(signedTx)
    )) as any;
    const cborSignedTx = Buffer.from(response.cborHex, "hex");

    const submittedTxHash = await blockfrostSubmitTransaction(cborSignedTx);
    Logger.success(`Tx submitted: ${submittedTxHash}`);
    return {
      cbor: response.cborHex,
      txId: submittedTxHash,
    };
  }
}

const kuberService = {
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
  // register stake and outputs 20A
  initializeWallets: (
    wallets: StaticWallet[],
    faucetAddress: string = getWalletConfigForFaucet().address,
    faucetStakeKey: string = getWalletConfigForFaucet().payment.private
  ) => {
    const kuber = new Kuber(faucetAddress, faucetStakeKey);
    const outputs = [];
    const stakes = [];
    const certificates = [];
    for (let i = 0; i < wallets.length; i++) {
      outputs.push({
        address: wallets[i].address,
        value: `${20}A`,
      });
      stakes.push({
        type: "PaymentSigningKeyShelley_ed25519",
        description: "Payment Signing Key",
        cborHex: "5820" + wallets[i].stake.private,
      });
      certificates.push(
        Kuber.generateCert("registerstake", wallets[i].stake.pkh)
      );
    }
    return kuber.signAndSubmitTx({
      selections: [...stakes],
      outputs,
      certificates,
    });
  },
  mergeUtXos: (wallets: StaticWallet[]) => {
    const kuber = new Kuber(
      getWalletConfigForFaucet().address,
      getWalletConfigForFaucet().payment.private
    );
    const selections = wallets.map((wallet) => ({
      type: "PaymentSigningKeyShelley_ed25519",
      description: "Payment Signing Key",
      cborHex: "5820" + wallet.payment.private,
    }));

    const inputs = wallets.map((wallet) => wallet.address);
    inputs.push(getWalletConfigForFaucet().address);
    return kuber.signAndSubmitTx({
      inputs,
      selections,
      changeAddress: getWalletConfigForFaucet().address,
    });
  },

  transferADA: (receiverAddressList: string[], ADA = 20) => {
    const kuber = new Kuber(
      getWalletConfigForFaucet().address,
      getWalletConfigForFaucet().payment.private
    );
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

  multipleTransferADA: (
    outputs: { address: string; value: string | number }[],
    addr = getWalletConfigForFaucet().address,
    signingKey = getWalletConfigForFaucet().payment.private
  ) => {
    const kuber = new Kuber(addr, signingKey);
    const req = {
      outputs,
    };
    return kuber.signAndSubmitTx(req);
  },

  multipleDRepRegistration: (metadataAndWallets: WalletAndAnchorType[]) => {
    const kuber = new Kuber(
      getWalletConfigForFaucet().address,
      getWalletConfigForFaucet().payment.private
    );
    const req = {
      certificates: metadataAndWallets.map((metadataAndWallet) =>
        Kuber.generateCert(
          "registerdrep",
          metadataAndWallet.wallet.dRep.pkh,
          metadataAndWallet
        )
      ),
      selections: metadataAndWallets.map((metadata) => {
        return {
          type: "PaymentSigningKeyShelley_ed25519",
          description: "Stake Signing Key",
          cborHex: `5820${metadata.wallet.dRep.private}`,
        };
      }),
    };
    return kuber.signAndSubmitTx(req);
  },

  dRepRegistration: (
    dRepSigningKey: string,
    dRepPkh: string,
    metadata: WalletAndAnchorType
  ) => {
    const kuber = new Kuber(
      getWalletConfigForFaucet().address,
      getWalletConfigForFaucet().payment.private
    );

    const req = {
      certificates: [Kuber.generateCert("registerdrep", dRepPkh, metadata)],
      selections: [
        {
          type: "PaymentSigningKeyShelley_ed25519",
          description: "Stake Signing Key",
          cborHex: `5820${dRepSigningKey}`,
        },
      ],
    };
    return kuber.signAndSubmitTx(req);
  },
  dRepDeRegistration: (
    addr: string,
    signingKey: string,
    dRepPrivateKey: string,
    pkh: string
  ) => {
    const kuber = new Kuber(addr, signingKey);
    const selections = [
      {
        type: "PaymentSigningKeyShelley_ed25519",
        description: "Payment Signing Key",
        cborHex: "5820" + dRepPrivateKey,
      },
    ];
    const req = {
      selections,
      inputs: addr,
      certificates: [Kuber.generateCert("deregisterdrep", pkh)],
    };
    return kuber.signAndSubmitTx(req);
  },

  multipleDRepDeRegistration: (wallets: StaticWallet[]) => {
    const kuber = new Kuber(
      getWalletConfigForFaucet().address,
      getWalletConfigForFaucet().payment.private
    );
    const req = {
      certificates: wallets.map((wallet) =>
        Kuber.generateCert("deregisterdrep", wallet.dRep.pkh)
      ),
      selections: wallets.map((wallet) => {
        return {
          type: "PaymentSigningKeyShelley_ed25519",
          description: "Stake Signing Key",
          cborHex: `5820${wallet.dRep.private}`,
        };
      }),
      inputs: getWalletConfigForFaucet().address,
    };
    return kuber.signAndSubmitTx(req);
  },

  stakeDelegation: (
    addr: string,
    signingKey: string,
    stakePrivateKey: string,
    pkh: string,
    dRep: string | "abstain" | "noconfidence"
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
      0
    );
    return balanceInLovelace / 1000000;
  },

  registerStake: (
    stakePrivateKey: string,
    pkh: string,
    signingKey: string,
    addr: string
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
    const kuber = new Kuber(
      getWalletConfigForFaucet().address,
      getWalletConfigForFaucet().payment.private
    );
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

  queryProtocolParams() {
    return callKuber("/api/v3/protocol-params") as Promise<ProtocolParams>;
  },

  voteOnProposal(
    addr: string,
    signingKey: string,
    voter: string, // dRepHash
    dRepStakePrivKey: string,
    proposal: string
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

  abstainDelegations(
    stakePrivKeys: string[],
    stakePkhs: string[]
  ): Promise<TxSubmitResponse> {
    const kuber = new Kuber(
      getWalletConfigForFaucet().address,
      getWalletConfigForFaucet().payment.private
    );
    const selections = stakePrivKeys.map((key) => {
      return {
        type: "PaymentSigningKeyShelley_ed25519",
        description: "Payment Signing Key",
        cborHex: "5820" + key,
      };
    });

    const certificates = stakePkhs.map((pkh) => {
      return {
        type: "delegate",
        key: pkh,
        drep: "abstain",
      };
    });
    const req = {
      selections,
      certificates,
    };
    return kuber.signAndSubmitTx(req);
  },
};
async function callKuber(
  path: any,
  method: "GET" | "POST" = "GET",
  body?: BodyInit,
  contentType = "application/json"
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
              }`
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
