import {
  Dispatch,
  SetStateAction,
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
} from "react";
import {
  Address,
  Anchor,
  AnchorDataHash,
  BigNum,
  Certificate,
  CertificatesBuilder,
  Credential,
  DRep,
  DrepDeregistration,
  DrepRegistration,
  DrepUpdate,
  Ed25519KeyHash,
  GovernanceActionId,
  LinearFee,
  PublicKey,
  RewardAddress,
  Transaction,
  TransactionBuilder,
  TransactionBuilderConfigBuilder,
  TransactionHash,
  TransactionOutput,
  TransactionUnspentOutput,
  TransactionUnspentOutputs,
  TransactionWitnessSet,
  URL,
  Value,
  VoteDelegation,
  Voter,
  VotingBuilder,
  VotingProcedure,
  StakeRegistration,
} from "@emurgo/cardano-serialization-lib-asmjs";
import { Buffer } from "buffer";
import { useNavigate } from "react-router-dom";
import { Link } from "@mui/material";
import * as Sentry from "@sentry/react";

import { useModal, useSnackbar } from ".";

import { PATHS } from "@consts";
import { CardanoApiWallet, DRepInfo, Protocol } from "@models";
import type { StatusModalState } from "@organisms";
import {
  getPubDRepId,
  WALLET_LS_KEY,
  DELEGATE_TRANSACTION_KEY,
  REGISTER_TRANSACTION_KEY,
  DELEGATE_TO_KEY,
  PROTOCOL_PARAMS_KEY,
  getItemFromLocalStorage,
  setItemToLocalStorage,
  removeItemFromLocalStorage,
  openInNewTab,
  SANCHO_INFO_KEY,
  VOTE_TRANSACTION_KEY,
  checkIsMaintenanceOn,
} from "@utils";
import { getDRepInfo, getEpochParams, getTransactionStatus } from "@services";
import {
  setLimitedDelegationInterval,
  setLimitedRegistrationInterval,
} from "./walletUtils";

interface Props {
  children: React.ReactNode;
}

interface EnableResponse {
  status: string;
  stakeKey?: boolean;
  error?: string;
}
const TIME_TO_EXPIRE_TRANSACTION = 3 * 60 * 1000; // 3 MINUTES
const REFRESH_TIME = 15 * 1000; // 15 SECONDS

type TransactionHistoryItem = {
  transactionHash: string;
  time?: Date;
};

export type DRepActionType = "retirement" | "registration" | "update" | "";

interface CardanoContext {
  address?: string;
  disconnectWallet: () => Promise<void>;
  enable: (walletName: string) => Promise<EnableResponse>;
  error?: string;
  dRep: DRepInfo | undefined;
  isEnabled: boolean;
  pubDRepKey: string;
  dRepID: string;
  dRepIDBech32: string;
  isMainnet: boolean;
  stakeKey?: string;
  setDRep: (key: undefined | DRepInfo) => void;
  setStakeKey: (key: string) => void;
  stakeKeys: string[];
  walletApi?: CardanoApiWallet;
  delegatedDRepId?: string;
  setDelegatedDRepId: (key: string) => void;
  buildSignSubmitConwayCertTx: ({
    certBuilder,
    votingBuilder,
    type,
    registrationType,
  }: {
    certBuilder?: CertificatesBuilder;
    votingBuilder?: VotingBuilder;
    type?: "delegation" | "registration" | "vote";
    proposalId?: string;
    registrationType?: DRepActionType;
  }) => Promise<string>;
  buildDRepRegCert: (
    url?: string,
    hash?: string
  ) => Promise<CertificatesBuilder>;
  buildVoteDelegationCert: (vote: string) => Promise<CertificatesBuilder>;
  buildDRepUpdateCert: (
    url?: string,
    hash?: string
  ) => Promise<CertificatesBuilder>;
  buildDRepRetirementCert: () => Promise<CertificatesBuilder>;
  buildVote: (
    voteChoice: string,
    txHash: string,
    index: number,
    cip95MetadataURL?: string,
    cip95MetadataHash?: string
  ) => Promise<VotingBuilder>;
  delegateTransaction: TransactionHistoryItem;
  registerTransaction: TransactionHistoryItem & { type: DRepActionType };
  delegateTo: string;
  voteTransaction: TransactionHistoryItem & { proposalId: string };
  isPendingTransaction: () => boolean;
  isDrepLoading: boolean;
  setIsDrepLoading: Dispatch<SetStateAction<boolean>>;
}

type Utxos = {
  txid: any;
  txindx: number;
  amount: string;
  str: string;
  multiAssetStr: string;
  TransactionUnspentOutput: TransactionUnspentOutput;
}[];

const NETWORK = import.meta.env.VITE_NETWORK_FLAG;

const CardanoContext = createContext<CardanoContext>({} as CardanoContext);
CardanoContext.displayName = "CardanoContext";

function CardanoProvider(props: Props) {
  const [isEnabled, setIsEnabled] = useState(false);
  const [dRep, setDRep] = useState<DRepInfo | undefined>(undefined);
  const [walletApi, setWalletApi] = useState<CardanoApiWallet | undefined>(
    undefined
  );
  const [address, setAddress] = useState<string | undefined>(undefined);
  const [pubDRepKey, setPubDRepKey] = useState<string>("");
  const [dRepID, setDRepID] = useState<string>("");
  const [dRepIDBech32, setDRepIDBech32] = useState<string>("");
  const [stakeKey, setStakeKey] = useState<string | undefined>(undefined);
  const [stakeKeys, setStakeKeys] = useState<string[]>([]);
  const [isMainnet, setIsMainnet] = useState<boolean>(false);
  const { openModal, closeModal } = useModal<StatusModalState>();
  const [registeredStakeKeysListState, setRegisteredPubStakeKeysState] =
    useState<string[]>([]);
  const [error, setError] = useState<string | undefined>(undefined);
  const [delegatedDRepId, setDelegatedDRepId] = useState<string | undefined>(
    undefined
  );
  const [delegateTo, setDelegateTo] = useState<string>("");
  const [walletState, setWalletState] = useState<{
    changeAddress: undefined | string;
    usedAddress: undefined | string;
  }>({
    changeAddress: undefined,
    usedAddress: undefined,
  });
  const [delegateTransaction, setDelegateTransaction] =
    useState<TransactionHistoryItem>({
      time: undefined,
      transactionHash: "",
    });
  const [registerTransaction, setRegisterTransaction] = useState<
    TransactionHistoryItem & { type: DRepActionType }
  >({ time: undefined, transactionHash: "", type: "" });
  const [voteTransaction, setVoteTransaction] = useState<
    { proposalId: string } & TransactionHistoryItem
  >({ time: undefined, transactionHash: "", proposalId: "" });
  const [isDrepLoading, setIsDrepLoading] = useState<boolean>(true);

  const { addSuccessAlert, addWarningAlert, addErrorAlert } = useSnackbar();

  const isPendingTransaction = useCallback(() => {
    if (
      registerTransaction?.transactionHash ||
      delegateTransaction?.transactionHash ||
      voteTransaction?.transactionHash
    ) {
      openModal({
        type: "statusModal",
        state: {
          status: "info",
          title: "Please wait for your previous transaction to be completed.",
          message:
            "Before performing a new action please wait for the previous action transaction to be completed.",
          buttonText: "Ok",
          onSubmit: () => {
            closeModal();
          },
          dataTestId: "transaction-inprogress-modal",
        },
      });
      return true;
    }
    return false;
  }, [
    registerTransaction?.transactionHash,
    delegateTransaction?.transactionHash,
    voteTransaction?.transactionHash,
  ]);

  useEffect(() => {
    const delegateTransaction = JSON.parse(
      getItemFromLocalStorage(DELEGATE_TRANSACTION_KEY + `_${stakeKey}`)
    );
    const registerTransaction = JSON.parse(
      getItemFromLocalStorage(REGISTER_TRANSACTION_KEY + `_${stakeKey}`)
    );
    const voteTransaction = JSON.parse(
      getItemFromLocalStorage(VOTE_TRANSACTION_KEY + `_${stakeKey}`)
    );
    const delegateTo = getItemFromLocalStorage(
      DELEGATE_TO_KEY + `_${stakeKey}`
    );
    if (delegateTransaction?.transactionHash) {
      setDelegateTransaction(delegateTransaction);
    }
    if (registerTransaction?.transactionHash) {
      setRegisterTransaction(registerTransaction);
    }
    if (voteTransaction?.transactionHash) {
      setVoteTransaction(voteTransaction);
    }
    if (delegateTo) {
      setDelegateTo(delegateTo);
    }
  }, [isEnabled, stakeKey]);

  useEffect(() => {
    if (delegateTransaction?.transactionHash) {
      const checkDelegateTransaction = async () => {
        const resetDelegateTransaction = () => {
          clearInterval(interval);
          removeItemFromLocalStorage(DELEGATE_TRANSACTION_KEY + `_${stakeKey}`);
          setDelegateTransaction({
            time: undefined,
            transactionHash: "",
          });
        };
        const status = await getTransactionStatus(
          delegateTransaction.transactionHash
        );
        if (status.transactionConfirmed) {
          if (isEnabled) {
            await setLimitedDelegationInterval(
              3000,
              10,
              dRepID,
              delegateTo,
              stakeKey
            ).then((isDelegated) => {
              if (isDelegated) {
                addSuccessAlert(
                  "Your voting power has been successfully delegated!"
                );
              } else {
                addWarningAlert(
                  "Your voting power has been successfully delegated! Please refresh the page."
                );
              }
            });
          }
          resetDelegateTransaction();
        }
        if (
          new Date().getTime() - new Date(delegateTransaction?.time).getTime() >
          TIME_TO_EXPIRE_TRANSACTION
        ) {
          resetDelegateTransaction();
          if (isEnabled) addErrorAlert("Delegation transaction failed");
        }
      };
      let interval = setInterval(checkDelegateTransaction, REFRESH_TIME);
      checkDelegateTransaction();
    }
    if (registerTransaction?.transactionHash) {
      const checkRegisterTransaction = async () => {
        const resetRegisterTransaction = () => {
          clearInterval(interval);
          removeItemFromLocalStorage(REGISTER_TRANSACTION_KEY + `_${stakeKey}`);
          setRegisterTransaction({
            time: undefined,
            transactionHash: "",
            type: "",
          });
        };
        const status = await getTransactionStatus(
          registerTransaction.transactionHash
        );
        if (status.transactionConfirmed) {
          if (isEnabled) {
            if (
              registerTransaction.type === "registration" ||
              registerTransaction.type === "retirement"
            ) {
              await setLimitedRegistrationInterval(
                3000,
                10,
                dRepID,
                registerTransaction.type,
                setDRep
              ).then((isRegistered) => {
                if (registerTransaction.type === "registration") {
                  if (isRegistered) {
                    addSuccessAlert(
                      "You have successfully registered as a DRep!"
                    );
                  } else {
                    addWarningAlert(
                      "You have successfully registered as a DRep! Please refresh the page."
                    );
                  }
                } else if (registerTransaction.type === "retirement") {
                  if (!isRegistered) {
                    addSuccessAlert(
                      "You have successfully retired from being a DRep!"
                    );
                  } else {
                    addWarningAlert(
                      "You have successfully retired from being a DRep! Please refresh the page."
                    );
                  }
                }
              });
            } else {
              addSuccessAlert("You have successfully updated DRep metadata!");
            }
          }
          resetRegisterTransaction();
        }
        if (
          new Date().getTime() - new Date(registerTransaction?.time).getTime() >
          TIME_TO_EXPIRE_TRANSACTION
        ) {
          resetRegisterTransaction();
          if (isEnabled)
            addErrorAlert(
              registerTransaction.type === "retirement"
                ? "Retirement transaction failed"
                : registerTransaction.type === "registration"
                ? "Registration transaction failed"
                : "Update DRep metadata transaction failed"
            );
        }
      };
      let interval = setInterval(checkRegisterTransaction, REFRESH_TIME);
      checkRegisterTransaction();
    }
    if (voteTransaction?.transactionHash) {
      const checkVoteTransaction = async () => {
        const resetVoteTransaction = () => {
          clearInterval(interval);
          removeItemFromLocalStorage(VOTE_TRANSACTION_KEY + `_${stakeKey}`);
          setVoteTransaction({
            time: undefined,
            transactionHash: "",
            proposalId: "",
          });
        };
        const status = await getTransactionStatus(
          voteTransaction.transactionHash
        );
        if (status.transactionConfirmed) {
          resetVoteTransaction();
          if (isEnabled) addSuccessAlert("You have successfully voted!");
        }
        if (
          new Date().getTime() - new Date(voteTransaction?.time).getTime() >
          TIME_TO_EXPIRE_TRANSACTION
        ) {
          resetVoteTransaction();
          if (isEnabled) addErrorAlert("Vote transaction failed");
        }
      };
      let interval = setInterval(checkVoteTransaction, REFRESH_TIME);
      checkVoteTransaction();
    }
    if (
      isEnabled &&
      (voteTransaction?.transactionHash ||
        registerTransaction?.transactionHash ||
        delegateTransaction?.transactionHash)
    ) {
      addWarningAlert("Transaction in progress. Please wait.", 10000);
    }
  }, [delegateTransaction, registerTransaction, voteTransaction]);

  const getChangeAddress = async (enabledApi: CardanoApiWallet) => {
    try {
      const raw = await enabledApi.getChangeAddress();
      const changeAddress = Address.from_bytes(
        Buffer.from(raw, "hex")
      ).to_bech32();
      setWalletState((prev) => ({ ...prev, changeAddress }));
    } catch (err) {
      Sentry.captureException(err);
      console.log(err);
    }
  };

  const getUsedAddresses = async (enabledApi: CardanoApiWallet) => {
    try {
      const raw = await enabledApi.getUsedAddresses();
      const rawFirst = raw[0];
      const usedAddress = Address.from_bytes(
        Buffer.from(rawFirst, "hex")
      ).to_bech32();
      setWalletState((prev) => ({ ...prev, usedAddress }));
    } catch (err) {
      Sentry.captureException(err);
      console.log(err);
    }
  };

  const getUtxos = async (
    enabledApi: CardanoApiWallet
  ): Promise<Utxos | undefined> => {
    let Utxos = [];

    try {
      const rawUtxos = await enabledApi.getUtxos();

      for (const rawUtxo of rawUtxos) {
        const utxo = TransactionUnspentOutput.from_bytes(
          Buffer.from(rawUtxo, "hex")
        );
        const input = utxo.input();
        const txid = Buffer.from(
          input.transaction_id().to_bytes(),
          "utf8"
        ).toString("hex");
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
            const policyIdHex = Buffer.from(
              policyId.to_bytes(),
              "utf8"
            ).toString("hex");
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
          txid: txid,
          txindx: txindx,
          amount: amount,
          str: `${txid} #${txindx} = ${amount}`,
          multiAssetStr: multiAssetStr,
          TransactionUnspentOutput: utxo,
        };
        Utxos.push(obj);
      }

      return Utxos;
    } catch (err) {
      Sentry.captureException(err);
      console.log(err);
    }
  };

  const enable = useCallback(
    async (walletName: string) => {
      await checkIsMaintenanceOn();

      // todo: use .getSupportedExtensions() to check if wallet supports CIP-95
      if (!isEnabled && walletName) {
        try {
          // Check that this wallet supports CIP-95 connection
          if (!window.cardano[walletName].supportedExtensions) {
            throw new Error("Your wallet does not support CIP-30 extensions.");
          } else if (
            !window.cardano[walletName].supportedExtensions.some(
              (item) => item.cip === 95
            )
          ) {
            throw new Error(
              "Your wallet does not support the required CIP-30 extension, CIP-95."
            );
          }
          // Enable wallet connection
          const enabledApi = await window.cardano[walletName]
            .enable({
              extensions: [{ cip: 95 }],
            })
            .catch((e) => {
              Sentry.captureException(e);
              throw e.info;
            });
          await getChangeAddress(enabledApi);
          await getUsedAddresses(enabledApi);
          setIsEnabled(true);
          setWalletApi(enabledApi);
          // Check if wallet has enabled the CIP-95 extension
          const enabledExtensions = await enabledApi.getExtensions();
          if (!enabledExtensions.some((item) => item.cip === 95)) {
            throw new Error(
              "Your wallet did not enable the needed CIP-95 functions during connection."
            );
          }
          const network = await enabledApi.getNetworkId();
          if (network != NETWORK) {
            throw new Error(
              `You are trying to connect with a wallet connected to ${
                network == 1 ? "mainnet" : "testnet"
              }. Please adjust your wallet settings to connect to ${
                network != 1 ? "mainnet" : "testnet"
              } or select a different wallet`
            );
          }
          setIsMainnet(network == 1);
          // Check and set wallet address
          const usedAddresses = await enabledApi.getUsedAddresses();
          const unusedAddresses = await enabledApi.getUnusedAddresses();
          if (!usedAddresses.length && !unusedAddresses.length) {
            throw new Error("No addresses found.");
          }
          if (!usedAddresses.length) {
            setAddress(unusedAddresses[0]);
          } else {
            setAddress(usedAddresses[0]);
          }

          const registeredStakeKeysList =
            await enabledApi.cip95.getRegisteredPubStakeKeys();
          setRegisteredPubStakeKeysState(registeredStakeKeysList);

          const unregisteredStakeKeysList =
            await enabledApi.cip95.getUnregisteredPubStakeKeys();

          let stakeKeysList;
          if (registeredStakeKeysList.length > 0) {
            stakeKeysList = registeredStakeKeysList.map((stakeKey) => {
              const stakeKeyHash = PublicKey.from_hex(stakeKey).hash();
              const stakeCredential = Credential.from_keyhash(stakeKeyHash);
              if (network === 1)
                return RewardAddress.new(1, stakeCredential)
                  .to_address()
                  .to_hex();
              else
                return RewardAddress.new(0, stakeCredential)
                  .to_address()
                  .to_hex();
            });
          } else {
            console.log(
              "Warning, no registered stake keys, using unregistered stake keys"
            );
            stakeKeysList = unregisteredStakeKeysList.map((stakeKey) => {
              const stakeKeyHash = PublicKey.from_hex(stakeKey).hash();
              const stakeCredential = Credential.from_keyhash(stakeKeyHash);
              if (network === 1)
                return RewardAddress.new(1, stakeCredential)
                  .to_address()
                  .to_hex();
              else
                return RewardAddress.new(0, stakeCredential)
                  .to_address()
                  .to_hex();
            });
          }

          setStakeKeys(stakeKeysList);

          let stakeKeySet = false;
          const savedStakeKey = getItemFromLocalStorage(
            `${WALLET_LS_KEY}_stake_key`
          );
          if (savedStakeKey && stakeKeysList.includes(savedStakeKey)) {
            setStakeKey(savedStakeKey);
            stakeKeySet = true;
          } else if (stakeKeysList.length === 1) {
            setStakeKey(stakeKeysList[0]);

            setItemToLocalStorage(
              `${WALLET_LS_KEY}_stake_key`,
              stakeKeysList[0]
            );
            stakeKeySet = true;
          }
          const dRepIds = await getPubDRepId(enabledApi);
          setPubDRepKey(dRepIds?.dRepKey || "");
          setDRepID(dRepIds?.dRepID || "");
          setDRepIDBech32(dRepIds?.dRepIDBech32 || "");
          setItemToLocalStorage(`${WALLET_LS_KEY}_name`, walletName);

          const protocol = await getEpochParams();
          setItemToLocalStorage(PROTOCOL_PARAMS_KEY, protocol);

          return { status: "OK", stakeKey: stakeKeySet };
        } catch (e) {
          Sentry.captureException(e);
          console.error(e);
          setError(`${e}`);
          setAddress(undefined);
          setWalletApi(undefined);
          setPubDRepKey("");
          setStakeKey(undefined);
          setIsEnabled(false);
          throw {
            status: "ERROR",
            error: `${e == undefined ? "Something went wrong" : e}`,
          };
        }
      }
      throw { status: "ERROR", error: `Something went wrong` };
    },
    [isEnabled, stakeKeys]
  );

  const disconnectWallet = useCallback(async () => {
    removeItemFromLocalStorage(`${WALLET_LS_KEY}_name`);
    removeItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`);
    setWalletApi(undefined);
    setAddress(undefined);
    setStakeKey(undefined);
    setIsEnabled(false);
  }, []);

  // Create transaction builder
  const initTransactionBuilder = useCallback(async () => {
    const protocolParams = getItemFromLocalStorage(
      PROTOCOL_PARAMS_KEY
    ) as Protocol;

    if (protocolParams) {
      const txBuilder = TransactionBuilder.new(
        TransactionBuilderConfigBuilder.new()
          .fee_algo(
            LinearFee.new(
              BigNum.from_str(String(protocolParams.min_fee_a)),
              BigNum.from_str(String(protocolParams.min_fee_b))
            )
          )
          .pool_deposit(BigNum.from_str(String(protocolParams.pool_deposit)))
          .key_deposit(BigNum.from_str(String(protocolParams.key_deposit)))
          .coins_per_utxo_word(
            BigNum.from_str(String(protocolParams.coins_per_utxo_size))
          )
          .max_value_size(protocolParams.max_val_size)
          .max_tx_size(protocolParams.max_tx_size)
          .prefer_pure_change(true)
          .build()
      );
      return txBuilder;
    }
  }, []);

  const getTxUnspentOutputs = useCallback(async (utxos: Utxos) => {
    let txOutputs = TransactionUnspentOutputs.new();
    for (const utxo of utxos) {
      txOutputs.add(utxo.TransactionUnspentOutput);
    }
    return txOutputs;
  }, []);

  // Build, sign and submit transaction
  const buildSignSubmitConwayCertTx = useCallback(
    async ({
      certBuilder,
      votingBuilder,
      type,
      proposalId,
      registrationType,
    }: {
      certBuilder?: CertificatesBuilder;
      votingBuilder?: VotingBuilder;
      type?: "delegation" | "registration" | "vote";
      proposalId?: string;
      registrationType?: DRepActionType;
    }) => {
      await checkIsMaintenanceOn();
      const isPendingTx = isPendingTransaction();
      if (isPendingTx) return;

      console.log(walletState, "walletState");
      console.log(certBuilder, "certBuilder");
      try {
        const txBuilder = await initTransactionBuilder();

        if (!txBuilder) {
          throw new Error("Application can not create transaction");
        }

        if (certBuilder) {
          txBuilder.set_certs_builder(certBuilder);
        }

        if (votingBuilder) {
          txBuilder.set_voting_builder(votingBuilder);
        }

        if (
          !walletState.changeAddress ||
          !walletState.usedAddress ||
          !walletApi
        )
          throw new Error("Check the wallet is connected.");
        const shelleyOutputAddress = Address.from_bech32(
          walletState.usedAddress
        );
        const shelleyChangeAddress = Address.from_bech32(
          walletState.changeAddress
        );

        // Add output of 1 ADA to the address of our wallet
        let outputValue = BigNum.from_str("1000000");

        if (registrationType === "retirement" && dRep?.deposit) {
          outputValue = outputValue.checked_add(
            BigNum.from_str(`${dRep?.deposit}`)
          );
        }

        txBuilder.add_output(
          TransactionOutput.new(shelleyOutputAddress, Value.new(outputValue))
        );

        const utxos = await getUtxos(walletApi);

        if (!utxos) {
          throw new Error("Application can not get utxos");
        }
        // Find the available UTXOs in the wallet and use them as Inputs for the transaction
        const txUnspentOutputs = await getTxUnspentOutputs(utxos);

        // Use UTxO selection strategy 2
        txBuilder.add_inputs_from(txUnspentOutputs, 2);

        // Set change address, incase too much ADA provided for fee
        txBuilder.add_change_if_needed(shelleyChangeAddress);

        // Build transaction body
        const txBody = txBuilder.build();

        // Make a full transaction, passing in empty witness set
        const transactionWitnessSet = TransactionWitnessSet.new();
        const tx = Transaction.new(
          txBody,
          TransactionWitnessSet.from_bytes(transactionWitnessSet.to_bytes())
        );
        // Ask wallet to to provide signature (witnesses) for the transaction
        let txVkeyWitnesses;

        txVkeyWitnesses = await walletApi.signTx(
          Buffer.from(tx.to_bytes(), "utf8").toString("hex"),
          true
        );

        // Create witness set object using the witnesses provided by the wallet
        txVkeyWitnesses = TransactionWitnessSet.from_bytes(
          Buffer.from(txVkeyWitnesses, "hex")
        );
        transactionWitnessSet.set_vkeys(txVkeyWitnesses.vkeys());
        // Build transaction with witnesses
        const signedTx = Transaction.new(tx.body(), transactionWitnessSet);
        console.log(
          Buffer.from(signedTx.to_bytes(), "utf8").toString("hex"),
          "signed tx cbor"
        );

        // Submit built signed transaction to chain, via wallet's submit transaction endpoint
        const result = await walletApi.submitTx(
          Buffer.from(signedTx.to_bytes(), "utf8").toString("hex")
        );
        // Set results so they can be rendered
        const cip95ResultTx = Buffer.from(signedTx.to_bytes(), "utf8").toString(
          "hex"
        );
        const resultHash = result;
        const cip95ResultWitness = Buffer.from(
          txVkeyWitnesses.to_bytes(),
          "utf8"
        ).toString("hex");

        if (type === "registration") {
          setRegisterTransaction({
            time: new Date(),
            transactionHash: resultHash,
            type: registrationType ?? "",
          });
          setItemToLocalStorage(
            REGISTER_TRANSACTION_KEY + `_${stakeKey}`,
            JSON.stringify({
              time: new Date(),
              transactionHash: resultHash,
              type: registrationType,
            })
          );
        }
        if (type === "delegation") {
          setDelegateTransaction({
            time: new Date(),
            transactionHash: resultHash,
          });
          setItemToLocalStorage(
            DELEGATE_TRANSACTION_KEY + `_${stakeKey}`,
            JSON.stringify({
              time: new Date(),
              transactionHash: resultHash,
            })
          );
        }
        if (type === "vote") {
          setVoteTransaction({
            time: new Date(),
            transactionHash: resultHash,
            proposalId: proposalId ?? "",
          });
          setItemToLocalStorage(
            VOTE_TRANSACTION_KEY + `_${stakeKey}`,
            JSON.stringify({
              time: new Date(),
              transactionHash: resultHash,
              proposalId: proposalId ?? "",
            })
          );
        }
        console.log(cip95ResultTx, "cip95ResultTx");
        console.log(resultHash, "cip95ResultHash");
        console.log(cip95ResultWitness, "cip95ResultWitness");
        return resultHash;
      } catch (error) {
        const walletName = getItemFromLocalStorage(`${WALLET_LS_KEY}_name`);
        const isWalletConnected = await window.cardano[walletName].isEnabled();

        if (!isWalletConnected) {
          disconnectWallet();
        }

        Sentry.captureException(error);
        console.log(error, "error");
        throw error?.info ?? error;
      }
    },
    [
      walletState,
      walletApi,
      getUtxos,
      registerTransaction.transactionHash,
      delegateTransaction.transactionHash,
      voteTransaction.transactionHash,
      stakeKey,
      isPendingTransaction,
      dRep,
    ]
  );

  const buildVoteDelegationCert = useCallback(
    async (target: string): Promise<CertificatesBuilder> => {
      try {
        // Build Vote Delegation Certificate
        const certBuilder = CertificatesBuilder.new();
        let stakeCred;
        if (!stakeKey) {
          throw new Error("No stake key selected");
        }
        // Remove network tag from stake key hash
        const stakeKeyHash = Ed25519KeyHash.from_hex(stakeKey.substring(2));
        // if chosen stake key is registered use it, else register it
        if (registeredStakeKeysListState.length > 0) {
          stakeCred = Credential.from_keyhash(stakeKeyHash);
        } else {
          console.log("Registering stake key");
          stakeCred = Credential.from_keyhash(stakeKeyHash);
          const stakeKeyRegCert = StakeRegistration.new(stakeCred);
          certBuilder.add(Certificate.new_stake_registration(stakeKeyRegCert));
        }
        // Create correct DRep
        let targetDRep;
        if (target === "abstain") {
          targetDRep = DRep.new_always_abstain();
        } else if (target === "no confidence") {
          targetDRep = DRep.new_always_no_confidence();
        } else {
          if (target.includes("drep")) {
            targetDRep = DRep.new_key_hash(Ed25519KeyHash.from_bech32(target));
          } else {
            targetDRep = DRep.new_key_hash(Ed25519KeyHash.from_hex(target));
          }
        }
        // Create cert object
        const voteDelegationCert = VoteDelegation.new(stakeCred, targetDRep);
        // add cert to tbuilder

        certBuilder.add(Certificate.new_vote_delegation(voteDelegationCert));
        setDelegateTo(target);
        setItemToLocalStorage(DELEGATE_TO_KEY + `_${stakeKey}`, target);
        return certBuilder;
      } catch (e) {
        Sentry.captureException(e);
        console.log(e);
        throw e;
      }
    },
    [stakeKey, registeredStakeKeysListState]
  );

  // conway alpha
  const buildDRepRegCert = useCallback(
    async (
      cip95MetadataURL?: string,
      cip95MetadataHash?: string
    ): Promise<CertificatesBuilder> => {
      try {
        const epochParams = getItemFromLocalStorage(PROTOCOL_PARAMS_KEY);
        // Build DRep Registration Certificate
        const certBuilder = CertificatesBuilder.new();

        // Get wallet's DRep key
        const dRepKeyHash = Ed25519KeyHash.from_hex(dRepID);
        const dRepCred = Credential.from_keyhash(dRepKeyHash);

        let dRepRegCert;
        // If there is an anchor
        if (cip95MetadataURL && cip95MetadataHash) {
          const url = URL.new(cip95MetadataURL);
          const hash = AnchorDataHash.from_hex(cip95MetadataHash);
          const anchor = Anchor.new(url, hash);
          // Create cert object using one Ada as the deposit
          dRepRegCert = DrepRegistration.new_with_anchor(
            dRepCred,
            BigNum.from_str(`${epochParams.drep_deposit}`),
            anchor
          );
        } else {
          console.log("DRep Registration - not using anchor");
          dRepRegCert = DrepRegistration.new(
            dRepCred,
            BigNum.from_str(`${epochParams.drep_deposit}`)
          );
        }
        // add cert to tbuilder
        certBuilder.add(Certificate.new_drep_registration(dRepRegCert));
        return certBuilder;
      } catch (e) {
        Sentry.captureException(e);
        console.log(e);
        throw e;
      }
    },
    [dRepID]
  );

  // conway alpha
  const buildDRepUpdateCert = useCallback(
    async (
      cip95MetadataURL?: string,
      cip95MetadataHash?: string
    ): Promise<CertificatesBuilder> => {
      try {
        // Build DRep Registration Certificate
        const certBuilder = CertificatesBuilder.new();

        // Get wallet's DRep key
        const dRepKeyHash = Ed25519KeyHash.from_hex(dRepID);
        const dRepCred = Credential.from_keyhash(dRepKeyHash);

        let dRepUpdateCert;
        // If there is an anchor
        if (cip95MetadataURL && cip95MetadataHash) {
          const url = URL.new(cip95MetadataURL);
          const hash = AnchorDataHash.from_hex(cip95MetadataHash);
          const anchor = Anchor.new(url, hash);
          // Create cert object using one Ada as the deposit
          dRepUpdateCert = DrepUpdate.new_with_anchor(dRepCred, anchor);
        } else {
          dRepUpdateCert = DrepUpdate.new(dRepCred);
        }
        // add cert to tbuilder
        certBuilder.add(Certificate.new_drep_update(dRepUpdateCert));
        return certBuilder;
      } catch (e) {
        Sentry.captureException(e);
        console.log(e);
        throw e;
      }
    },
    [dRepID]
  );

  // conway alpha
  const buildDRepRetirementCert =
    useCallback(async (): Promise<CertificatesBuilder> => {
      try {
        // Build DRep Registration Certificate
        const certBuilder = CertificatesBuilder.new();
        // Get wallet's DRep key
        const dRepKeyHash = Ed25519KeyHash.from_hex(dRepID);
        const dRepCred = Credential.from_keyhash(dRepKeyHash);

        const dRepRetirementCert = DrepDeregistration.new(
          dRepCred,
          BigNum.from_str(`${dRep?.deposit}`)
        );
        // add cert to tbuilder
        certBuilder.add(
          Certificate.new_drep_deregistration(dRepRetirementCert)
        );
        return certBuilder;
      } catch (e) {
        Sentry.captureException(e);
        console.log(e);
        throw e;
      }
    }, [dRepID, dRep]);

  const buildVote = useCallback(
    async (
      voteChoice: string,
      txHash: string,
      index: number,
      cip95MetadataURL?: string,
      cip95MetadataHash?: string
    ): Promise<VotingBuilder> => {
      try {
        // Get wallet's DRep key
        const dRepKeyHash = Ed25519KeyHash.from_hex(dRepID);
        // Vote things
        const voter = Voter.new_drep(Credential.from_keyhash(dRepKeyHash));
        const govActionId = GovernanceActionId.new(
          // placeholder
          TransactionHash.from_hex(txHash),
          index
        );

        let votingChoice;
        if (voteChoice === "yes") {
          votingChoice = 1;
        } else if (voteChoice === "no") {
          votingChoice = 0;
        } else {
          votingChoice = 2;
        }

        let votingProcedure;
        if (cip95MetadataURL && cip95MetadataHash) {
          const url = URL.new(cip95MetadataURL);
          const hash = AnchorDataHash.from_hex(cip95MetadataHash);
          const anchor = Anchor.new(url, hash);
          // Create cert object using one Ada as the deposit
          votingProcedure = VotingProcedure.new_with_anchor(
            votingChoice,
            anchor
          );
        } else {
          votingProcedure = VotingProcedure.new(votingChoice);
        }

        const votingBuilder = VotingBuilder.new();
        votingBuilder.add(voter, govActionId, votingProcedure);

        return votingBuilder;
      } catch (e) {
        Sentry.captureException(e);
        console.log(e);
        throw e;
      }
    },
    [dRepID]
  );

  const value = useMemo(
    () => ({
      address,
      enable,
      dRep,
      isEnabled,
      isMainnet,
      disconnectWallet,
      dRepID,
      dRepIDBech32,
      pubDRepKey,
      stakeKey,
      setDRep,
      setStakeKey,
      stakeKeys,
      walletApi,
      error,
      delegatedDRepId,
      setDelegatedDRepId,
      buildSignSubmitConwayCertTx,
      buildDRepRegCert,
      buildDRepUpdateCert,
      buildDRepRetirementCert,
      buildVote,
      buildVoteDelegationCert,
      delegateTransaction,
      registerTransaction,
      delegateTo,
      voteTransaction,
      isPendingTransaction,
      isDrepLoading,
      setIsDrepLoading,
    }),
    [
      address,
      enable,
      dRep,
      isEnabled,
      isMainnet,
      disconnectWallet,
      dRepID,
      dRepIDBech32,
      pubDRepKey,
      stakeKey,
      setDRep,
      setStakeKey,
      stakeKeys,
      walletApi,
      error,
      delegatedDRepId,
      setDelegatedDRepId,
      buildSignSubmitConwayCertTx,
      buildDRepRegCert,
      buildDRepUpdateCert,
      buildDRepRetirementCert,
      buildVote,
      buildVoteDelegationCert,
      delegateTransaction,
      registerTransaction,
      delegateTo,
      voteTransaction,
      isPendingTransaction,
      isDrepLoading,
      setIsDrepLoading,
    ]
  );

  return <CardanoContext.Provider value={value} {...props} />;
}

function useCardano() {
  const context = useContext(CardanoContext);
  const { openModal, closeModal } = useModal<StatusModalState>();
  const { addSuccessAlert } = useSnackbar();
  const navigate = useNavigate();

  if (context === undefined) {
    throw new Error("useCardano must be used within a CardanoProvider");
  }

  const enable = useCallback(
    async (walletName: string) => {
      try {
        const isSanchoInfoShown = getItemFromLocalStorage(
          SANCHO_INFO_KEY + `_${walletName}`
        );
        const result = await context.enable(walletName);
        if (!result.error) {
          closeModal();
          if (result.stakeKey) {
            addSuccessAlert(`Wallet connected`, 3000);
          }
          if (!isSanchoInfoShown) {
            openModal({
              type: "statusModal",
              state: {
                status: "info",
                dataTestId: "info-about-sancho-net-modal",
                message: (
                  <p style={{ margin: 0 }}>
                    The SanchoNet GovTool is currently in beta and it connects
                    to{" "}
                    <Link
                      onClick={() => openInNewTab("https://sancho.network/")}
                      sx={{ cursor: "pointer" }}
                    >
                      SanchoNet
                    </Link>
                    .
                    <br />
                    <br /> Please note, this tool uses ‘Test ada’
                    <span style={{ fontWeight: 700 }}> NOT real ada</span>. All
                    governance actions and related terms pertain to SanchoNet."
                  </p>
                ),
                title: "This tool is connected to SanchoNet",
                buttonText: "Ok",
              },
            });
            setItemToLocalStorage(SANCHO_INFO_KEY + `_${walletName}`, true);
          }
          return result;
        }
      } catch (e: any) {
        Sentry.captureException(e);
        await context.disconnectWallet();
        navigate(PATHS.home);
        openModal({
          type: "statusModal",
          state: {
            status: "warning",
            message: e?.error?.replace("Error: ", ""),
            onSubmit: () => {
              closeModal();
            },
            title: "Oops!",
            dataTestId: "wallet-connection-error-modal",
          },
        });
        throw e;
      }
    },
    [context, openModal, context.isEnabled]
  );

  const disconnectWallet = useCallback(async () => {
    await context.disconnectWallet();
  }, [context]);

  return { ...context, enable, disconnectWallet };
}

export { CardanoProvider, useCardano };
