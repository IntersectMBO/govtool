const config = {
  apiUrl: Cypress.env("blockfrostUrl"),
  apiKey: Cypress.env("blockfrostApiKey"),
};

const blockfrostService = {
  async addresses(addr: string) {
    return await callBlockfrost("/addresses/" + addr).catch((e) => {
      if (e.status == 404) {
        return "00";
      } else {
        throw e;
      }
    });
  },
  async addressesUtxos(addr: string) {
    return await callBlockfrost(
      "/addresses/" + addr + "/utxos?order=desc"
    ).catch((e) => {
      if (e.status == 404) {
        return [];
      } else {
        throw e;
      }
    });
  },
  async submitTransaction(tx: string) {
    return await callBlockfrost(
      "/tx/submit",
      "POST",
      Buffer.from(tx, "hex"),
      "application/cbor"
    );
  },
  async getAssetDetail(asset: string) {
    return await callBlockfrost("/assets/" + asset);
  },

  async getDatum(hash: string) {
    return await callBlockfrost("/scripts/datum/" + hash);
  },
  async getTransactionDetails(txHash: any) {
    return await callBlockfrost("/txs/" + txHash);
  },
  async getTransactionUtxos(txHash: any) {
    return await callBlockfrost("/txs/" + txHash + "/utxos");
  },

  async getTransactions(addr: any) {
    return await callBlockfrost("/addresses/" + addr + "/utxos");
  },
};

async function callBlockfrost(
  path: any,
  method: "GET" | "POST" = "GET",
  body?: BodyInit,
  contentType?: string
) {
  const url = config.apiUrl + path;

  const headers: Record<string, string> = {
    project_id: config.apiKey,
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
              `BlockfrostApi [Status ${res.status}] : ${
                json.message ? json.message : txt
              }`
            );
          } else {
            err = Error(`BlockfrostApi [Status ${res.status}] : ${txt}`);
          }
        } catch (e) {
          err = Error(`BlockfrostApi [Status ${res.status}] : ${txt}`);
        }
        err.status = res.status;
        throw err;
      });
    }
  });
}

export default blockfrostService;
