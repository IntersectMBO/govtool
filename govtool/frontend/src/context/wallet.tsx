import {
  createContext,
  useCallback,
  useContext,
  useMemo,
  useState,
} from "react";
import {
  Address,
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
  Value,
  VoteDelegation,
  Voter,
  VotingBuilder,
  VotingProcedure,
  StakeRegistration,
  VotingProposalBuilder,
  InfoAction,
  VotingProposal,
  GovernanceAction,
  TreasuryWithdrawals,
  TreasuryWithdrawalsAction,
} from "@emurgo/cardano-serialization-lib-asmjs";
import { Buffer } from "buffer";
import { useNavigate } from "react-router-dom";
import { Link } from "@mui/material";
import * as Sentry from "@sentry/react";
import { Trans } from "react-i18next";

import { PATHS } from "@consts";
import { CardanoApiWallet, Protocol } from "@models";
import type { StatusModalState } from "@organisms";
import {
  checkIsMaintenanceOn,
  generateAnchor,
  getItemFromLocalStorage,
  getPubDRepID,
  openInNewTab,
  PROTOCOL_PARAMS_KEY,
  removeItemFromLocalStorage,
  SANCHO_INFO_KEY,
  setItemToLocalStorage,
  WALLET_LS_KEY,
} from "@utils";
import { getEpochParams } from "@services";
import { useTranslation } from "@hooks";
import { getUtxos } from "./getUtxos";
import { useModal, useSnackbar } from ".";
import {
  PendingTransaction,
  TransactionStateWithResource,
  TransactionStateWithoutResource,
  usePendingTransaction,
} from "./pendingTransaction";

interface Props {
  children: React.ReactNode;
}

interface EnableResponse {
  status: string;
  stakeKey?: boolean;
  error?: string;
}

type InfoProps = {
  hash: string;
  url: string;
};

type TreasuryProps = {
  amount: string;
  hash: string;
  receivingAddress: string;
  url: string;
};

type BuildSignSubmitConwayCertTxArgs = {
  certBuilder?: CertificatesBuilder | Certificate;
  govActionBuilder?: VotingProposalBuilder;
  votingBuilder?: VotingBuilder;
  voterDeposit?: string;
} & (
  | Pick<TransactionStateWithoutResource, "type" | "resourceId">
  | Pick<TransactionStateWithResource, "type" | "resourceId">
);

interface CardanoContextType {
  address?: string;
  disconnectWallet: () => Promise<void>;
  enable: (walletName: string) => Promise<EnableResponse>;
  isEnableLoading: string | null;
  error?: string;
  isEnabled: boolean;
  pubDRepKey: string;
  dRepID: string;
  dRepIDBech32: string;
  isMainnet: boolean;
  stakeKey?: string;
  setStakeKey: (key: string) => void;
  stakeKeys: string[];
  walletApi?: CardanoApiWallet;
  buildSignSubmitConwayCertTx: ({
    certBuilder,
    govActionBuilder,
    resourceId,
    type,
    votingBuilder,
    voterDeposit,
  }: BuildSignSubmitConwayCertTxArgs) => Promise<string>;
  buildDRepRegCert: (url?: string, hash?: string) => Promise<Certificate>;
  buildVoteDelegationCert: (vote: string) => Promise<CertificatesBuilder>;
  buildDRepUpdateCert: (url?: string, hash?: string) => Promise<Certificate>;
  buildDRepRetirementCert: (voterDeposit: string) => Promise<Certificate>;
  buildVote: (
    voteChoice: string,
    txHash: string,
    index: number,
    cip95MetadataURL?: string,
    cip95MetadataHash?: string,
  ) => Promise<VotingBuilder>;
  pendingTransaction: PendingTransaction;
  isPendingTransaction: () => boolean;
  buildNewInfoGovernanceAction: (
    infoProps: InfoProps,
  ) => Promise<VotingProposalBuilder | undefined>;
  buildTreasuryGovernanceAction: (
    treasuryProps: TreasuryProps,
  ) => Promise<VotingProposalBuilder | undefined>;
}

type Utxos = {
  txid: unknown;
  txindx: number;
  amount: string;
  str: string;
  multiAssetStr: string;
  TransactionUnspentOutput: TransactionUnspentOutput;
}[];

const NETWORK = +import.meta.env.VITE_NETWORK_FLAG;

const CardanoContext = createContext<CardanoContextType>(
  {} as CardanoContextType,
);
CardanoContext.displayName = "CardanoContext";

const CardanoProvider = (props: Props) => {
  const [isEnabled, setIsEnabled] = useState(false);
  const [isEnableLoading, setIsEnableLoading] = useState<string | null>(null);
  const [walletApi, setWalletApi] = useState<CardanoApiWallet | undefined>(
    undefined,
  );
  const [address, setAddress] = useState<string | undefined>(undefined);
  const [pubDRepKey, setPubDRepKey] = useState<string>("");
  const [dRepID, setDRepID] = useState<string>("");
  const [dRepIDBech32, setDRepIDBech32] = useState<string>("");
  const [stakeKey, setStakeKey] = useState<string | undefined>(undefined);
  const [stakeKeys, setStakeKeys] = useState<string[]>([]);
  const [isMainnet, setIsMainnet] = useState<boolean>(false);
  const [registeredStakeKeysListState, setRegisteredPubStakeKeysState] =
    useState<string[]>([]);
  const [error, setError] = useState<string | undefined>(undefined);
  const [walletState, setWalletState] = useState<{
    changeAddress: undefined | string;
    usedAddress: undefined | string;
  }>({
    changeAddress: undefined,
    usedAddress: undefined,
  });
  const { t } = useTranslation();
  const epochParams = getItemFromLocalStorage(PROTOCOL_PARAMS_KEY);

  const { isPendingTransaction, updateTransaction, pendingTransaction } =
    usePendingTransaction({ isEnabled, stakeKey });

  const getChangeAddress = async (enabledApi: CardanoApiWallet) => {
    try {
      const raw = await enabledApi.getChangeAddress();
      const changeAddress = Address.from_bytes(
        Buffer.from(raw, "hex"),
      ).to_bech32();
      setWalletState((prev) => ({ ...prev, changeAddress }));
    } catch (err) {
      Sentry.captureException(err);
      console.error(err);
    }
  };

  const getUsedAddresses = async (enabledApi: CardanoApiWallet) => {
    try {
      const raw = await enabledApi.getUsedAddresses();
      const rawFirst = raw[0];
      const usedAddress = Address.from_bytes(
        Buffer.from(rawFirst, "hex"),
      ).to_bech32();
      setWalletState((prev) => ({ ...prev, usedAddress }));
    } catch (err) {
      Sentry.captureException(err);
      console.error(err);
    }
  };

  const enable = useCallback(
    async (walletName: string) => {
      setIsEnableLoading(walletName);
      await checkIsMaintenanceOn();

      // todo: use .getSupportedExtensions() to check if wallet supports CIP-95
      if (!isEnabled && walletName) {
        try {
          // Check that this wallet supports CIP-95 connection
          if (!window.cardano[walletName].supportedExtensions) {
            throw new Error(t("errors.walletNoCIP30Support"));
          } else if (
            !window.cardano[walletName].supportedExtensions.some(
              (item) => item.cip === 95,
            )
          ) {
            throw new Error(t("errors.walletNoCIP30Nor90Support"));
          }
          // Enable wallet connection
          const enabledApi: CardanoApiWallet = await window.cardano[walletName]
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
            throw new Error(t("errors.walletNoCIP90FunctionsEnabled"));
          }
          const network = await enabledApi.getNetworkId();
          if (network !== NETWORK) {
            throw new Error(
              t("errors.tryingConnectTo", {
                networkFrom: network === 1 ? "mainnet" : "testnet",
                networkTo: network !== 1 ? "mainnet" : "testnet",
              }),
            );
          }
          setIsMainnet(network === 1);
          // Check and set wallet address
          const usedAddresses = await enabledApi.getUsedAddresses();
          const unusedAddresses = await enabledApi.getUnusedAddresses();
          if (!usedAddresses.length && !unusedAddresses.length) {
            throw new Error(t("errors.noAddressesFound"));
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
            stakeKeysList = registeredStakeKeysList.map((key) => {
              const stakeKeyHash = PublicKey.from_hex(key).hash();
              const stakeCredential = Credential.from_keyhash(stakeKeyHash);
              if (network === 1) {
                return RewardAddress.new(1, stakeCredential)
                  .to_address()
                  .to_hex();
              }
              return RewardAddress.new(0, stakeCredential)
                .to_address()
                .to_hex();
            });
          } else {
            console.warn(t("warnings.usingUnregisteredStakeKeys"));
            stakeKeysList = unregisteredStakeKeysList.map((key) => {
              const stakeKeyHash = PublicKey.from_hex(key).hash();
              const stakeCredential = Credential.from_keyhash(stakeKeyHash);
              if (network === 1) {
                return RewardAddress.new(1, stakeCredential)
                  .to_address()
                  .to_hex();
              }
              return RewardAddress.new(0, stakeCredential)
                .to_address()
                .to_hex();
            });
          }

          setStakeKeys(stakeKeysList);

          let stakeKeySet = false;
          const savedStakeKey = getItemFromLocalStorage(
            `${WALLET_LS_KEY}_stake_key`,
          );
          if (savedStakeKey && stakeKeysList.includes(savedStakeKey)) {
            setStakeKey(savedStakeKey);
            stakeKeySet = true;
          } else if (stakeKeysList.length === 1) {
            setStakeKey(stakeKeysList[0]);

            setItemToLocalStorage(
              `${WALLET_LS_KEY}_stake_key`,
              stakeKeysList[0],
            );
            stakeKeySet = true;
          }
          const dRepIDs = await getPubDRepID(enabledApi);
          setPubDRepKey(dRepIDs?.dRepKey || "");
          setDRepID(dRepIDs?.dRepID || "");
          setDRepIDBech32(dRepIDs?.dRepIDBech32 || "");
          setItemToLocalStorage(`${WALLET_LS_KEY}_name`, walletName);

          const protocol = await getEpochParams();
          setItemToLocalStorage(PROTOCOL_PARAMS_KEY, protocol);

          return { status: t("ok"), stakeKey: stakeKeySet };
        } catch (e) {
          Sentry.captureException(e);
          console.error(e);
          setError(`${e}`);
          setAddress(undefined);
          setWalletApi(undefined);
          setPubDRepKey("");
          setStakeKey(undefined);
          setIsEnabled(false);
          // eslint-disable-next-line no-throw-literal
          throw {
            status: "ERROR",
            error: `${e ?? t("errors.somethingWentWrong")}`,
          };
        } finally {
          setIsEnableLoading(null);
        }
      }
      // eslint-disable-next-line no-throw-literal
      throw { status: "ERROR", error: t("errors.somethingWentWrong") };
    },
    [isEnabled, stakeKeys],
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
      PROTOCOL_PARAMS_KEY,
    ) as Protocol;

    if (protocolParams) {
      const txBuilder = TransactionBuilder.new(
        TransactionBuilderConfigBuilder.new()
          .fee_algo(
            LinearFee.new(
              BigNum.from_str(String(protocolParams.min_fee_a)),
              BigNum.from_str(String(protocolParams.min_fee_b)),
            ),
          )
          .pool_deposit(BigNum.from_str(String(protocolParams.pool_deposit)))
          .key_deposit(BigNum.from_str(String(protocolParams.key_deposit)))
          .coins_per_utxo_byte(
            BigNum.from_str(String(protocolParams.coins_per_utxo_size)),
          )
          .max_value_size(protocolParams.max_val_size)
          .max_tx_size(protocolParams.max_tx_size)
          .prefer_pure_change(true)
          .build(),
      );
      return txBuilder;
    }
  }, []);

  const getTxUnspentOutputs = async (utxos: Utxos) => {
    const txOutputs = TransactionUnspentOutputs.new();
    // eslint-disable-next-line no-restricted-syntax
    for (const utxo of utxos) {
      txOutputs.add(utxo.TransactionUnspentOutput);
    }
    return txOutputs;
  };

  // Build, sign and submit transaction
  const buildSignSubmitConwayCertTx = useCallback(
    async ({
      certBuilder,
      govActionBuilder,
      resourceId,
      type,
      votingBuilder,
      voterDeposit,
    }: BuildSignSubmitConwayCertTxArgs) => {
      await checkIsMaintenanceOn();
      const isPendingTx = isPendingTransaction();
      if (isPendingTx) return;

      try {
        const txBuilder = await initTransactionBuilder();

        if (!txBuilder) {
          throw new Error(t("errors.appCannotCreateTransaction"));
        }

        if (certBuilder) {
          if (certBuilder instanceof Certificate) {
            const builder = CertificatesBuilder.new();
            builder.add(certBuilder);
            txBuilder.set_certs_builder(builder);
          } else {
            txBuilder.set_certs_builder(certBuilder);
          }
        }

        if (votingBuilder) {
          txBuilder.set_voting_builder(votingBuilder);
        }

        if (govActionBuilder) {
          txBuilder.set_voting_proposal_builder(govActionBuilder);
        }

        if (
          !walletState.changeAddress ||
          !walletState.usedAddress ||
          !walletApi
        )
          throw new Error(t("errors.checkIsWalletConnected"));
        const shelleyOutputAddress = Address.from_bech32(
          walletState.usedAddress,
        );
        const shelleyChangeAddress = Address.from_bech32(
          walletState.changeAddress,
        );

        // Add output of 1 ADA to the address of our wallet
        let outputValue = BigNum.from_str("1000000");

        if (
          (type === "retireAsDrep" ||
            type === "retireAsSoleVoter" ||
            type === "delegate") &&
          voterDeposit
        ) {
          outputValue = outputValue.checked_add(BigNum.from_str(voterDeposit));
        }

        txBuilder.add_output(
          TransactionOutput.new(shelleyOutputAddress, Value.new(outputValue)),
        );

        const utxos = await getUtxos(walletApi);

        if (!utxos) {
          throw new Error(t("errors.appCannotGetUtxos"));
        }
        // Find the available UTXOs in the wallet and use them as Inputs for the transaction
        const txUnspentOutputs = await getTxUnspentOutputs(utxos);

        // Use UTxO selection strategy 3
        try {
          txBuilder.add_inputs_from(txUnspentOutputs, 3);
        } catch (e) {
          console.error(e);
          // Use UTxO selection strategy 2 if strategy 3 fails
          txBuilder.add_inputs_from(txUnspentOutputs, 2);
        }

        // Set change address, incase too much ADA provided for fee
        txBuilder.add_change_if_needed(shelleyChangeAddress);

        // Build transaction body
        const txBody = txBuilder.build();

        // Make a full transaction, passing in empty witness set
        const transactionWitnessSet = TransactionWitnessSet.new();
        const tx = Transaction.new(
          txBody,
          TransactionWitnessSet.from_bytes(transactionWitnessSet.to_bytes()),
        );
        // Ask wallet to to provide signature (witnesses) for the transaction
        let txVkeyWitnesses;

        txVkeyWitnesses = await walletApi.signTx(tx.to_hex(), true);

        // Create witness set object using the witnesses provided by the wallet
        txVkeyWitnesses = TransactionWitnessSet.from_bytes(
          Buffer.from(txVkeyWitnesses, "hex"),
        );
        const vkeys = txVkeyWitnesses.vkeys();

        if (!vkeys) throw new Error(t("errors.appCannotGetVkeys"));

        transactionWitnessSet.set_vkeys(vkeys);
        // Build transaction with witnesses
        const signedTx = Transaction.new(tx.body(), transactionWitnessSet);

        // Submit built signed transaction to chain, via wallet's submit transaction endpoint
        const result = await walletApi.submitTx(signedTx.to_hex());
        // Set results so they can be rendered
        const resultHash = result;

        updateTransaction({
          transactionHash: resultHash,
          type,
          resourceId,
        });

        // eslint-disable-next-line no-console
        console.log(signedTx.to_hex(), "signed tx cbor");
        return resultHash;
        // TODO: type error
        // eslint-disable-next-line @typescript-eslint/no-shadow, @typescript-eslint/no-explicit-any
      } catch (error: any) {
        const walletName = getItemFromLocalStorage(`${WALLET_LS_KEY}_name`);
        const isWalletConnected = await window.cardano[walletName].isEnabled();

        if (!isWalletConnected) {
          disconnectWallet();
        }

        Sentry.captureException(error);
        console.error(error, "error");
        throw error?.info ?? error;
      }
    },
    [isPendingTransaction, stakeKey, updateTransaction, walletApi, walletState],
  );

  const buildVoteDelegationCert = useCallback(
    async (target: string): Promise<CertificatesBuilder> => {
      try {
        // Build Vote Delegation Certificate
        const certBuilder = CertificatesBuilder.new();
        let stakeCred;
        if (!stakeKey) {
          throw new Error(t("errors.noStakeKeySelected"));
        }
        // Remove network tag from stake key hash
        const stakeKeyHash = Ed25519KeyHash.from_hex(stakeKey.substring(2));
        // if chosen stake key is registered use it, else register it
        if (registeredStakeKeysListState.length > 0) {
          stakeCred = Credential.from_keyhash(stakeKeyHash);
        } else {
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
        } else if (target.includes("drep")) {
          targetDRep = DRep.new_key_hash(Ed25519KeyHash.from_bech32(target));
        } else {
          targetDRep = DRep.new_key_hash(Ed25519KeyHash.from_hex(target));
        }
        // Create cert object
        const voteDelegationCert = VoteDelegation.new(stakeCred, targetDRep);
        // add cert to tbuilder
        certBuilder.add(Certificate.new_vote_delegation(voteDelegationCert));

        return certBuilder;
      } catch (e) {
        Sentry.captureException(e);
        console.error(e);
        throw e;
      }
    },
    [stakeKey, registeredStakeKeysListState],
  );

  // conway alpha
  const buildDRepRegCert = useCallback(
    async (
      cip95MetadataURL?: string,
      cip95MetadataHash?: string,
    ): Promise<Certificate> => {
      try {
        // Get wallet's DRep key
        const dRepKeyHash = Ed25519KeyHash.from_hex(dRepID);
        const dRepCred = Credential.from_keyhash(dRepKeyHash);

        let dRepRegCert;
        // If there is an anchor
        if (cip95MetadataURL && cip95MetadataHash) {
          const anchor = generateAnchor(cip95MetadataURL, cip95MetadataHash);
          // Create cert object using one Ada as the deposit
          dRepRegCert = DrepRegistration.new_with_anchor(
            dRepCred,
            BigNum.from_str(`${epochParams.drep_deposit}`),
            anchor,
          );
        } else {
          console.error(t("errors.notUsingAnchor"));
          dRepRegCert = DrepRegistration.new(
            dRepCred,
            BigNum.from_str(`${epochParams.drep_deposit}`),
          );
        }
        return Certificate.new_drep_registration(dRepRegCert);
      } catch (e) {
        Sentry.captureException(e);
        console.error(e);
        throw e;
      }
    },
    [epochParams, dRepID],
  );

  const buildDRepUpdateCert = useCallback(
    async (
      cip95MetadataURL?: string,
      cip95MetadataHash?: string,
    ): Promise<Certificate> => {
      try {
        // Get wallet's DRep key
        const dRepKeyHash = Ed25519KeyHash.from_hex(dRepID);
        const dRepCred = Credential.from_keyhash(dRepKeyHash);

        let dRepUpdateCert;
        // If there is an anchor
        if (cip95MetadataURL && cip95MetadataHash) {
          const anchor = generateAnchor(cip95MetadataURL, cip95MetadataHash);
          // Create cert object using one Ada as the deposit
          dRepUpdateCert = DrepUpdate.new_with_anchor(dRepCred, anchor);
        } else {
          dRepUpdateCert = DrepUpdate.new(dRepCred);
        }
        return Certificate.new_drep_update(dRepUpdateCert);
      } catch (e) {
        Sentry.captureException(e);
        console.error(e);
        throw e;
      }
    },
    [dRepID],
  );

  const buildDRepRetirementCert = useCallback(
    async (voterDeposit: string): Promise<Certificate> => {
      try {
        // Get wallet's DRep key
        const dRepKeyHash = Ed25519KeyHash.from_hex(dRepID);
        const dRepCred = Credential.from_keyhash(dRepKeyHash);

        const dRepRetirementCert = DrepDeregistration.new(
          dRepCred,
          BigNum.from_str(voterDeposit),
        );

        return Certificate.new_drep_deregistration(dRepRetirementCert);
      } catch (e) {
        Sentry.captureException(e);
        console.error(e);
        throw e;
      }
    },
    [dRepID],
  );

  const buildVote = useCallback(
    async (
      voteChoice: string,
      txHash: string,
      index: number,
      cip95MetadataURL?: string,
      cip95MetadataHash?: string,
    ): Promise<VotingBuilder> => {
      try {
        // Get wallet's DRep key
        const dRepKeyHash = Ed25519KeyHash.from_hex(dRepID);
        // Vote things
        const voter = Voter.new_drep(Credential.from_keyhash(dRepKeyHash));
        const govActionId = GovernanceActionId.new(
          // placeholder
          TransactionHash.from_hex(txHash),
          index,
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
          const anchor = generateAnchor(cip95MetadataURL, cip95MetadataHash);
          // Create cert object using one Ada as the deposit
          votingProcedure = VotingProcedure.new_with_anchor(
            votingChoice,
            anchor,
          );
        } else {
          votingProcedure = VotingProcedure.new(votingChoice);
        }

        const votingBuilder = VotingBuilder.new();
        votingBuilder.add(voter, govActionId, votingProcedure);

        return votingBuilder;
      } catch (e) {
        Sentry.captureException(e);
        console.error(e);
        throw e;
      }
    },
    [dRepID],
  );

  const getRewardAddress = useCallback(async () => {
    const addresses = await walletApi?.getRewardAddresses();
    if (!addresses) {
      throw new Error("Can not get reward addresses from wallet.");
    }
    const firstAddress = addresses[0];
    const bech32Address = Address.from_bytes(
      Buffer.from(firstAddress, "hex"),
    ).to_bech32();

    return RewardAddress.from_address(Address.from_bech32(bech32Address));
  }, [walletApi]);

  // info action
  const buildNewInfoGovernanceAction = useCallback(
    async ({ hash, url }: InfoProps) => {
      const govActionBuilder = VotingProposalBuilder.new();
      try {
        // Create new info action
        const infoAction = InfoAction.new();
        const infoGovAct = GovernanceAction.new_info_action(infoAction);
        // Create an anchor
        const anchor = generateAnchor(url, hash);

        const rewardAddr = await getRewardAddress();
        if (!rewardAddr) throw new Error("Can not get reward address");

        // Create voting proposal
        const votingProposal = VotingProposal.new(
          infoGovAct,
          anchor,
          rewardAddr,
          BigNum.from_str(epochParams.gov_action_deposit.toString()),
        );
        govActionBuilder.add(votingProposal);

        return govActionBuilder;
      } catch (err) {
        console.error(err);
      }
    },
    [epochParams, getRewardAddress],
  );

  // treasury action
  const buildTreasuryGovernanceAction = useCallback(
    async ({ amount, hash, receivingAddress, url }: TreasuryProps) => {
      const govActionBuilder = VotingProposalBuilder.new();
      try {
        const treasuryTarget = RewardAddress.from_address(
          Address.from_bech32(receivingAddress),
        );

        if (!treasuryTarget) throw new Error("Can not get tresasury target");

        const myWithdrawal = BigNum.from_str(amount);
        const withdrawals = TreasuryWithdrawals.new();
        withdrawals.insert(treasuryTarget, myWithdrawal);
        // Create new treasury withdrawal gov act
        const treasuryAction = TreasuryWithdrawalsAction.new(withdrawals);
        const treasuryGovAct =
          GovernanceAction.new_treasury_withdrawals_action(treasuryAction);
        // Create an anchor
        const anchor = generateAnchor(url, hash);

        const rewardAddr = await getRewardAddress();

        if (!rewardAddr) throw new Error("Can not get reward address");
        // Create voting proposal
        const votingProposal = VotingProposal.new(
          treasuryGovAct,
          anchor,
          rewardAddr,
          BigNum.from_str(epochParams.gov_action_deposit.toString()),
        );
        govActionBuilder.add(votingProposal);

        return govActionBuilder;
      } catch (err) {
        console.error(err);
      }
    },
    [epochParams, getRewardAddress],
  );

  const value = useMemo(
    () => ({
      address,
      buildDRepRegCert,
      buildDRepRetirementCert,
      buildDRepUpdateCert,
      buildNewInfoGovernanceAction,
      buildSignSubmitConwayCertTx,
      buildTreasuryGovernanceAction,
      buildVote,
      buildVoteDelegationCert,
      disconnectWallet,
      dRepID,
      dRepIDBech32,
      enable,
      error,
      isEnabled,
      isEnableLoading,
      isMainnet,
      isPendingTransaction,
      pendingTransaction,
      pubDRepKey,
      setStakeKey,
      stakeKey,
      stakeKeys,
      walletApi,
    }),
    [
      address,
      buildDRepRegCert,
      buildDRepRetirementCert,
      buildDRepUpdateCert,
      buildNewInfoGovernanceAction,
      buildSignSubmitConwayCertTx,
      buildTreasuryGovernanceAction,
      buildVote,
      buildVoteDelegationCert,
      disconnectWallet,
      dRepID,
      dRepIDBech32,
      enable,
      error,
      isEnabled,
      isEnableLoading,
      isMainnet,
      isPendingTransaction,
      pendingTransaction,
      pubDRepKey,
      setStakeKey,
      stakeKey,
      stakeKeys,
      walletApi,
    ],
  );

  return <CardanoContext.Provider value={value} {...props} />;
};

function useCardano() {
  const context = useContext(CardanoContext);
  const { openModal, closeModal } = useModal<StatusModalState>();
  const { addSuccessAlert } = useSnackbar();
  const navigate = useNavigate();
  const { t } = useTranslation();

  if (context === undefined) {
    throw new Error(t("errors.useCardano"));
  }

  const enable = useCallback(
    async (walletName: string) => {
      try {
        const isSanchoInfoShown = getItemFromLocalStorage(
          `${SANCHO_INFO_KEY}_${walletName}`,
        );
        const result = await context.enable(walletName);
        if (!result.error) {
          closeModal();
          if (result.stakeKey) {
            addSuccessAlert(t("alerts.walletConnected"), 3000);
          }
          if (!isSanchoInfoShown) {
            openModal({
              type: "statusModal",
              state: {
                status: "info",
                dataTestId: "info-about-sancho-net-modal",
                message: (
                  <p style={{ margin: 0 }}>
                    {t("system.sanchoNetIsBeta")}
                    <Link
                      onClick={() => openInNewTab("https://sancho.network/")}
                      sx={{ cursor: "pointer" }}
                    >
                      {t("system.sanchoNet")}
                    </Link>
                    .
                    <br />
                    <br />
                    <Trans
                      i18nKey="system.testAdaNote"
                      components={[
                        <span style={{ fontWeight: 700 }} key="0" />,
                      ]}
                    />
                  </p>
                ),
                title: t("system.toolConnectedToSanchonet"),
                buttonText: t("ok"),
              },
            });
            setItemToLocalStorage(`${SANCHO_INFO_KEY}_${walletName}`, true);
          }
          return result;
        }
        // TODO: type error
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
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
            title: t("modals.common.oops"),
            dataTestId: "wallet-connection-error-modal",
          },
        });
        throw e;
      }
    },
    [context, openModal, context.isEnabled],
  );

  const disconnectWallet = useCallback(async () => {
    await context.disconnectWallet();
  }, [context]);

  return { ...context, enable, disconnectWallet };
}

export { CardanoProvider, useCardano };
