const config = {
  apiUrl: "https://cardano-sanchonet.blockfrost.io/api/v0",
  apiKey: "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
};
export const blockfrost = {
  async addresses(addr: string) {
    return await getBlockfrost("/addresses/" + addr).catch((e) => {
      if (e.status == 404) {
        return "00";
      } else {
        throw e;
      }
    });
  },
  async addressesUtxos(addr: string) {
    return await getBlockfrost("/addresses/" + addr + "/utxos?order=desc").catch((e) => {
      if (e.status == 404) {
        return [];
      } else {
        throw e;
      }
    });
  },
  async submitTransaction(tx:string){
    return await submitTransaction(Buffer.from(tx,'hex'))
  }
};

function getAssetDetail(asset: string) {
  return getBlockfrost("/assets/" + asset);
}

function getDatum(hash: string) {
  return getBlockfrost("/scripts/datum/" + hash);
}
function getTransactionDetails(txHash: any) {
  return getBlockfrost("/txs/" + txHash);
}
function getTransactionUtxos(txHash: any) {
  return getBlockfrost("/txs/" + txHash + "/utxos");
}

function getTransactions(addr: any) {
  return getBlockfrost("/addresses/" + addr + "/utxos");
}


function submitTransaction(txData: any) {
    const path = '/tx/submit';
    const method = 'POST';
    const contentType = 'application/cbor';

    return  getBlockfrost(path, method, txData, contentType);

}


async function getBlockfrost(path: any, method: 'GET' | 'POST' = 'GET', body?: BodyInit,contentType?:string) {
  const url = config.apiUrl + path;

  const headers: Record<string, string> = {
    project_id: config.apiKey,
  };
  if(contentType){
    headers['content-type']=contentType
  }

  const options: RequestInit = {
    method,
    headers,
  };

  if (method === 'POST' ) {
    if(body)
      options.body = body;
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
