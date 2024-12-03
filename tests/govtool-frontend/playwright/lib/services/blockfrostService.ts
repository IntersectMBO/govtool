import environments from "@constants/environments";

const blockfrostApiUrl =
  "https://cardano-" + environments.network + ".blockfrost.io/api";
const blockfrostApiKey = environments.blockfrostApiKey;

export async function blockfrostSubmitTransaction(cborSignedTx: Buffer) {
  const url = `${blockfrostApiUrl}/v0/tx/submit`;
  const res = await fetch(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/cbor",
      project_id: blockfrostApiKey,
    },
    body: cborSignedTx,
  });
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
            `BlockFrost [Status ${res.status}] : ${
              json.message ? json.message : txt
            }`
          );
        } else {
          err = Error(`BlockFrost [Status ${res.status}] : ${txt}`);
        }
      } catch (e) {
        err = Error(`BlockFrost [Status ${res.status}] : ${txt}`);
      }
      err.status = res.status;
      throw err;
    });
  }
}
