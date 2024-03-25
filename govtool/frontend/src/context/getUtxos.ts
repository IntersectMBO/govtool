import { CardanoApiWallet } from "@models";
import * as Sentry from "@sentry/react";
import { TransactionUnspentOutput } from "@emurgo/cardano-serialization-lib-asmjs";
import { Buffer } from "buffer";

type Utxos = {
  txid: unknown;
  txindx: number;
  amount: string;
  str: string;
  multiAssetStr: string;
  TransactionUnspentOutput: TransactionUnspentOutput;
}[];

export const getUtxos = async (
  enabledApi: CardanoApiWallet
): Promise<Utxos | undefined> => {
  const utxos = [];

  try {
    const rawUtxos = await enabledApi.getUtxos();

    // TODO maybe refactor
    // eslint-disable-next-line no-restricted-syntax
    for (const rawUtxo of rawUtxos) {
      const utxo = TransactionUnspentOutput.from_bytes(
        Buffer.from(rawUtxo, "hex")
      );
      const input = utxo.input();
      const txid = input.transaction_id().to_hex();

      const txindx = input.index();
      const output = utxo.output();
      const amount = output.amount().coin().to_str(); // ADA amount in lovelace
      const multiasset = output.amount().multiasset();
      let multiAssetStr = "";

      if (multiasset) {
        const keys = multiasset.keys(); // policy Ids of thee multiasset
        const N = keys.len();

        for (let i = 0; i < N; i++) {
          const policyId = keys.get(i);
          const policyIdHex = policyId.to_hex();
          const assets = multiasset.get(policyId);
          if (assets) {
            const assetNames = assets.keys();
            const K = assetNames.len();

            for (let j = 0; j < K; j++) {
              const assetName = assetNames.get(j);
              const assetNameString = assetName.name().toString();
              // eslint-disable-next-line @typescript-eslint/ban-ts-comment
              // @ts-ignore
              const assetNameHex = Buffer.from(
                assetName.name(),
                "utf8"
              ).toString("hex");
              const multiassetAmt = multiasset.get_asset(policyId, assetName);
              multiAssetStr += `+ ${multiassetAmt.to_str()} + ${policyIdHex}.${assetNameHex} (${assetNameString})`;
            }
          }
        }
      }

      const obj = {
        amount,
        multiAssetStr,
        str: `${txid} #${txindx} = ${amount}`,
        TransactionUnspentOutput: utxo,
        txid,
        txindx,
      };
      utxos.push(obj);
    }

    return utxos;
  } catch (err) {
    Sentry.captureException(err);
    // eslint-disable-next-line no-console
    console.error(err);
  }
};
