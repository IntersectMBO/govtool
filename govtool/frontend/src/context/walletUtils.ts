import { getAdaHolderCurrentDelegation, getVoterInfo } from "@services";
import { CardanoApiWallet, VoterInfo } from "@models";
import * as Sentry from "@sentry/react";
import { TransactionUnspentOutput } from "@emurgo/cardano-serialization-lib-asmjs";
import { Buffer } from "buffer";
import { DRepActionType } from "./wallet";

type Utxos = {
  txid: any;
  txindx: number;
  amount: string;
  str: string;
  multiAssetStr: string;
  TransactionUnspentOutput: TransactionUnspentOutput;
}[];

export const setLimitedRegistrationInterval = (
  intervalTime: number,
  attemptsNumber: number,
  dRepID: string,
  transactionType: DRepActionType | Omit<DRepActionType, "update">,
  setVoter: (key: undefined | VoterInfo) => void,
): Promise<boolean> =>
  new Promise((resolve) => {
    const desiredResult = transactionType === "registration";
    let count = 0;

    const interval = setInterval(async () => {
      if (count < attemptsNumber) {
        // TODO: Refactor this to not iterate over the same data
        count++;

        try {
          const data = await getVoterInfo(dRepID);

          if (
            data.isRegisteredAsDRep === desiredResult ||
            data.isRegisteredAsSoleVoter === desiredResult
          ) {
            setVoter(data);
            clearInterval(interval);
            resolve(desiredResult);
          }
        } catch (error) {
          clearInterval(interval);
          resolve(!desiredResult);
        }
      } else {
        clearInterval(interval);
        resolve(!desiredResult);
      }
    }, intervalTime);
  });

export const setLimitedDelegationInterval = (
  intervalTime: number,
  attemptsNumber: number,
  dRepID: string,
  delegateTo: string,
  stakeKey?: string,
): Promise<boolean> =>
  new Promise((resolve) => {
    let count = 0;

    const interval = setInterval(async () => {
      if (count < attemptsNumber) {
        count++;

        try {
          const currentDelegation = await getAdaHolderCurrentDelegation({
            stakeKey,
          });

          if (
            (delegateTo === dRepID && currentDelegation === dRepID) ||
            (delegateTo === "no confidence" &&
              currentDelegation === "drep_always_no_confidence") ||
            (delegateTo === "abstain" &&
              currentDelegation === "drep_always_abstain") ||
            (delegateTo !== dRepID && delegateTo === currentDelegation)
          ) {
            clearInterval(interval);
            resolve(true);
          }
        } catch (error) {
          clearInterval(interval);
          resolve(false);
        }
      } else {
        clearInterval(interval);
        resolve(false);
      }
    }, intervalTime);
  });

export const getUtxos = async (
  enabledApi: CardanoApiWallet
): Promise<Utxos | undefined> => {
  const utxos = [];

  try {
    const rawUtxos = await enabledApi.getUtxos();

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
          const policyIdHex = Buffer.from(policyId.to_bytes(), "utf8").toString(
            "hex"
          );
          const assets = multiasset.get(policyId);
          if (assets) {
            const assetNames = assets.keys();
            const K = assetNames.len();

            for (let j = 0; j < K; j++) {
              const assetName = assetNames.get(j);
              const assetNameString = Buffer.from(
                assetName.name(),
                "utf8"
              ).toString();
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
        txid,
        txindx,
        amount,
        str: `${txid} #${txindx} = ${amount}`,
        multiAssetStr,
        TransactionUnspentOutput: utxo,
      };
      utxos.push(obj);
    }

    return utxos;
  } catch (err) {
    Sentry.captureException(err);
    console.log(err);
  }
};
