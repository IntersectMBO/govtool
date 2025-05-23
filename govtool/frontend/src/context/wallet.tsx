import {
  createContext,
  useCallback,
  useContext,
  useMemo,
  useRef,
  useState,
} from "react";
import {
  Address,
  BigNum,
  Certificate,
  CertificatesBuilder,
  Credential,
  DRep,
  DRepDeregistration,
  DRepRegistration,
  DRepUpdate,
  Ed25519KeyHash,
  GovernanceActionId,
  LinearFee,
  PublicKey,
  RewardAddress,
  Transaction,
  TransactionBuilder,
  TransactionBuilderConfigBuilder,
  TransactionHash,
  TransactionUnspentOutput,
  TransactionUnspentOutputs,
  TransactionWitnessSet,
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
  ChangeConfig,
  PlutusScript,
  ProtocolParamUpdate,
  ParameterChangeAction,
  Costmdls,
  DRepVotingThresholds,
  ExUnitPrices,
  UnitInterval,
  ExUnits,
  PoolVotingThresholds,
  ProtocolVersion,
  HardForkInitiationAction,
  ScriptHash,
  PlutusScripts,
  Redeemers,
  Redeemer,
  RedeemerTag,
  PlutusData,
  PlutusMap,
  PlutusWitness,
  PlutusScriptSource,
  Int,
  CostModel,
  Language,
  TxInputsBuilder,
  NoConfidenceAction,
  Constitution,
  NewConstitutionAction,
  UpdateCommitteeAction,
  Committee,
  Credentials,
} from "@emurgo/cardano-serialization-lib-asmjs";
import { Buffer } from "buffer";
import { useNavigate } from "react-router-dom";
import { Link } from "@mui/material";
import * as Sentry from "@sentry/react";
import { Trans } from "react-i18next";

import { PATHS, COMPILED_GUARDRAIL_SCRIPT } from "@consts";
import { CardanoApiWallet, VoterInfo } from "@models";
import type { StatusModalState } from "@organisms";
import {
  checkIsMaintenanceOn,
  generateAnchor,
  getItemFromLocalStorage,
  getPubDRepID,
  openInNewTab,
  PROTOCOL_PARAMS_KEY,
  removeItemFromLocalStorage,
  NETWORK_INFO_KEY,
  setItemToLocalStorage,
  WALLET_LS_KEY,
  setProtocolParameterUpdate,
} from "@utils";
import { useTranslation } from "@hooks";
import { AutomatedVotingOptionDelegationId } from "@/types/automatedVotingOptions";

import { getUtxos } from "./getUtxos";
import { useAppContext, useModal, useSnackbar } from ".";
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

type VotingAnchor = {
  url: string;
  hash: string;
};

type InfoProps = VotingAnchor;

type NoConfidenceProps = VotingAnchor;

type TreasuryProps = {
  withdrawals: { receivingAddress: string; amount: string }[];
} & VotingAnchor;

type ProtocolParameterChangeProps = {
  prevGovernanceActionHash: string;
  prevGovernanceActionIndex: string;
  protocolParamsUpdate: Partial<ProtocolParamsUpdate>;
} & VotingAnchor;

type HardForkInitiationProps = {
  prevGovernanceActionHash: string;
  prevGovernanceActionIndex: string;
  major: string;
  minor: string;
} & VotingAnchor;

type NewConstitutionProps = {
  prevGovernanceActionHash?: string;
  prevGovernanceActionIndex?: string;
  constitutionUrl: string;
  constitutionHash: string;
  scriptHash?: string;
} & VotingAnchor;

type UpdateCommitteeProps = {
  prevGovernanceActionHash?: string;
  prevGovernanceActionIndex?: string;
  quorumThreshold: QuorumThreshold;
  newCommittee?: CommitteeToAdd[];
  removeCommittee?: string[];
} & VotingAnchor;

export type CommitteeToAdd = {
  expiryEpoch: string;
  committee: string;
};

export type QuorumThreshold = {
  numerator: string;
  denominator: string;
};
type ProtocolParamsUpdate = {
  adaPerUtxo: string;
  collateralPercentage: number;
  committeeTermLimit: number;
  costModels: Costmdls;
  drepDeposit: string;
  drepInactivityPeriod: number;
  drepVotingThresholds: DRepVotingThresholds;
  executionCosts: ExUnitPrices;
  expansionRate: UnitInterval;
  governanceActionDeposit: string;
  governanceActionValidityPeriod: number;
  keyDeposit: string;
  maxBlockBodySize: number;
  maxBlockExUnits: ExUnits;
  maxBlockHeaderSize: number;
  maxCollateralInputs: number;
  maxEpoch: number;
  maxTxExUnits: ExUnits;
  maxTxSize: number;
  maxValueSize: number;
  minCommitteeSize: number;
  minPoolCost: string;
  minFeeA: string;
  minFeeB: string;
  nOpt: number;
  poolDeposit: string;
  poolPledgeInfluence: UnitInterval;
  poolVotingThresholds: PoolVotingThresholds;
  refScriptCoinsPerByte: UnitInterval;
  treasuryGrowthRate: UnitInterval;
};

type BuildSignSubmitConwayCertTxArgs = {
  certBuilder?: CertificatesBuilder | Certificate;
  govActionBuilder?: VotingProposalBuilder;
  votingBuilder?: VotingBuilder;
  voter?: VoterInfo;
} & (
  | Pick<TransactionStateWithoutResource, "type" | "resourceId">
  | Pick<TransactionStateWithResource, "type" | "resourceId">
);

interface CardanoContextType {
  address?: string;
  disconnectWallet: () => Promise<void>;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  getChangeAddress: (enabledApi: CardanoApiWallet) => Promise<any>;
  enable: (walletName: string) => Promise<EnableResponse>;
  isEnableLoading: string | null;
  error?: string;
  isEnabled: boolean;
  pubDRepKey: string;
  dRepID: string;
  isMainnet: boolean;
  stakeKey?: string;
  setStakeKey: (key: string) => void;
  stakeKeys: string[];
  walletApi?: CardanoApiWallet;
  walletName?: string;
  registeredStakeKeysListState: string[];
  buildSignSubmitConwayCertTx: ({
    certBuilder,
    govActionBuilder,
    resourceId,
    type,
    votingBuilder,
    voter,
  }: BuildSignSubmitConwayCertTxArgs) => Promise<string>;
  buildStakeKeyRegCert: () => Promise<Certificate>;
  buildDRepRegCert: (url?: string, hash?: string) => Promise<Certificate>;
  buildVoteDelegationCert: (vote: string) => Promise<Certificate>;
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
  isStakeKeyRegistered: () => boolean;
  buildNewInfoGovernanceAction: (
    infoProps: InfoProps,
  ) => Promise<VotingProposalBuilder | undefined>;
  buildTreasuryGovernanceAction: (
    treasuryProps: TreasuryProps,
  ) => Promise<VotingProposalBuilder | undefined>;
  buildProtocolParameterChangeGovernanceAction: (
    protocolParamsProps: ProtocolParameterChangeProps,
  ) => Promise<VotingProposalBuilder | undefined>;
  buildHardForkGovernanceAction: (
    hardForkInitiationProps: HardForkInitiationProps,
  ) => Promise<VotingProposalBuilder | undefined>;
  buildNewConstitutionGovernanceAction: (
    newConstitutionProps: NewConstitutionProps,
  ) => Promise<VotingProposalBuilder | undefined>;
  buildUpdateCommitteeGovernanceAction: (
    updateCommitteeProps: UpdateCommitteeProps,
  ) => Promise<VotingProposalBuilder | undefined>;
  buildNoConfidenceGovernanceAction: (
    noConfidenceProps: NoConfidenceProps,
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
  const isGuardrailScriptUsed = useRef(false);
  const redeemers = useRef<Redeemer[]>([]);
  const [isEnableLoading, setIsEnableLoading] = useState<string | null>(null);
  const [walletApi, setWalletApi] = useState<CardanoApiWallet | undefined>(
    undefined,
  );

  const [address, setAddress] = useState<string | undefined>(undefined);
  const [pubDRepKey, setPubDRepKey] = useState<string>("");
  const [dRepID, setDRepID] = useState<string>("");
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
  const [walletName, setWalletName] = useState<string | undefined>(undefined);
  const { t } = useTranslation();
  const epochParams = getItemFromLocalStorage(PROTOCOL_PARAMS_KEY);

  const { isPendingTransaction, updateTransaction, pendingTransaction } =
    usePendingTransaction({ isEnabled, stakeKey });

  const getChangeAddress = useCallback(async (enabledApi: CardanoApiWallet) => {
    try {
      const raw = await enabledApi.getChangeAddress();
      const changeAddress = Address.from_bytes(
        Buffer.from(raw, "hex"),
      ).to_bech32();
      setWalletState((prev) => ({ ...prev, changeAddress }));

      return changeAddress;
    } catch (err) {
      console.error(err);
    }
  }, []);

  const getUsedAddresses = async (enabledApi: CardanoApiWallet) => {
    try {
      const raw = await enabledApi.getUsedAddresses();
      const rawFirst = raw[0];
      if (!rawFirst) return [];
      const usedAddress = Address.from_bytes(
        Buffer.from(rawFirst, "hex"),
      ).to_bech32();
      setWalletState((prev) => ({ ...prev, usedAddress }));
    } catch (err) {
      console.error(err);
    }
  };

  /**
   * Checks if there are any registered stake keys.
   * @returns {boolean} True if there are registered stake keys, false otherwise.
   */
  const isStakeKeyRegistered = () => !!registeredStakeKeysListState.length;

  const enable = useCallback(
    async (name: string) => {
      setIsEnableLoading(name);
      await checkIsMaintenanceOn();

      // TODO: use .getSupportedExtensions() to check if wallet supports CIP-95
      if (!isEnabled && name) {
        try {
          // Check that this wallet supports CIP-95 connection
          if (!window.cardano[name].supportedExtensions) {
            throw new Error(t("errors.walletNoCIP30Support"));
          } else if (
            !window.cardano[name].supportedExtensions.some(
              (item) => item.cip === 95,
            )
          ) {
            throw new Error(t("errors.walletNoCIP30Nor90Support"));
          }
          // Enable wallet connection
          const enabledApi: CardanoApiWallet = await window.cardano[name]
            .enable({
              extensions: [{ cip: 95 }],
            })
            .then((enabledWalletApi) => {
              Sentry.addBreadcrumb({
                category: "wallet",
                message: "Wallet connected",
                level: "info",
                data: window.cardano[name],
              });
              return enabledWalletApi;
            })
            .catch((e) => {
              throw e.info;
            });
          await getChangeAddress(enabledApi);
          await getUsedAddresses(enabledApi);

          setIsEnabled(true);
          setWalletApi(enabledApi);
          setWalletName(name);
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
          setItemToLocalStorage(`${WALLET_LS_KEY}_name`, name);

          return { status: t("ok"), stakeKey: stakeKeySet };
        } catch (e) {
          console.error({ e });
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
    [getChangeAddress, isEnabled, t],
  );

  const disconnectWallet = useCallback(async () => {
    removeItemFromLocalStorage(`${WALLET_LS_KEY}_name`);
    removeItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`);
    setWalletApi(undefined);
    setAddress(undefined);
    setStakeKey(undefined);
    setIsEnabled(false);
    setWalletName(undefined);

    Sentry.addBreadcrumb({
      category: "wallet",
      message: "Wallet disconnected",
      level: "info",
    });
  }, []);

  // Create transaction builder
  const initTransactionBuilder = useCallback(async () => {
    if (epochParams) {
      const txBuilder = TransactionBuilder.new(
        TransactionBuilderConfigBuilder.new()
          .fee_algo(
            LinearFee.new(
              BigNum.from_str(String(epochParams.min_fee_a)),
              BigNum.from_str(String(epochParams.min_fee_b)),
            ),
          )
          .pool_deposit(BigNum.from_str(String(epochParams.pool_deposit)))
          .key_deposit(BigNum.from_str(String(epochParams.key_deposit)))
          .coins_per_utxo_byte(
            BigNum.from_str(String(epochParams.coins_per_utxo_size)),
          )
          .max_value_size(epochParams.max_val_size)
          .max_tx_size(epochParams.max_tx_size)
          .prefer_pure_change(true)
          .do_not_burn_extra_change(true)
          .ex_unit_prices(
            ExUnitPrices.new(
              UnitInterval.new(
                BigNum.from_str("577"),
                BigNum.from_str("10000"),
              ),
              UnitInterval.new(
                BigNum.from_str("721"),
                BigNum.from_str("10000000"),
              ),
            ),
          )
          .build(),
      );
      return txBuilder;
    }
  }, [epochParams]);

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
    }: BuildSignSubmitConwayCertTxArgs) => {
      await checkIsMaintenanceOn();
      const isPendingTx = isPendingTransaction();
      if (isPendingTx) return;

      try {
        const txBuilder = await initTransactionBuilder();
        const transactionWitnessSet = TransactionWitnessSet.new();

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

        // register stake key if it is not registered
        if (!certBuilder && !registeredStakeKeysListState.length) {
          const stakeKeyRegCertBuilder = CertificatesBuilder.new();
          const stakeKeyRegCert = await buildStakeKeyRegCert();
          stakeKeyRegCertBuilder.add(stakeKeyRegCert);
          txBuilder.set_certs_builder(stakeKeyRegCertBuilder);
        }

        if (votingBuilder) {
          txBuilder.set_voting_builder(votingBuilder);
        }

        if (govActionBuilder) {
          txBuilder.set_voting_proposal_builder(govActionBuilder);
        }

        if (isGuardrailScriptUsed.current) {
          try {
            const scripts = PlutusScripts.new();
            scripts.add(
              PlutusScript.from_bytes_v3(
                Buffer.from(COMPILED_GUARDRAIL_SCRIPT, "hex"),
              ),
            );
            transactionWitnessSet.set_plutus_scripts(scripts);

            const newRedeemers = Redeemers.new();
            redeemers.current.forEach((redeemer) => {
              newRedeemers.add(redeemer);
            });
            transactionWitnessSet.set_redeemers(newRedeemers);

            const costModels = Costmdls.new();

            const addCostModels = (
              language: "v1" | "v2" | "v3",
              model: "PlutusV1" | "PlutusV2" | "PlutusV3",
            ) => {
              const costModel = CostModel.new();
              (
                epochParams?.cost_model?.costs?.[model] as number[] | undefined
              )?.forEach((val, index) =>
                costModel.set(index, Int.new_i32(val)),
              );
              costModels.insert(
                Language[`new_plutus_${language}`](),
                costModel,
              );
            };

            addCostModels("v1", "PlutusV1");
            addCostModels("v2", "PlutusV2");
            addCostModels("v3", "PlutusV3");

            txBuilder.calc_script_data_hash(costModels);
          } catch (e) {
            console.error(e);
          }
        }

        if (
          !walletState.changeAddress ||
          !walletState.usedAddress ||
          !walletApi
        )
          throw new Error(t("errors.checkIsWalletConnected"));
        const shelleyChangeAddress = Address.from_bech32(
          walletState.changeAddress,
        );

        const utxos = await getUtxos(walletApi);

        if (!utxos) {
          throw new Error(t("errors.appCannotGetUtxos"));
        }
        // Find the available UTXOs in the wallet and use them as Inputs for the transaction
        const txUnspentOutputs = await getTxUnspentOutputs(utxos);

        if (isGuardrailScriptUsed.current) {
          const txInputsBuilder = TxInputsBuilder.new();
          const probableCollaterals = utxos
            .filter((utxo) => !utxo.multiAssetStr)
            .map((utxo) => ({
              ...utxo,
              amount: BigInt(utxo.amount),
            }))
            .sort((a, b) => (a.amount > b.amount ? -1 : 1));

          if (probableCollaterals.length) {
            const unspentOutput =
              probableCollaterals[0].TransactionUnspentOutput;
            const output = unspentOutput.output();
            txInputsBuilder.add_regular_input(
              output.address(),
              unspentOutput.input(),
              output.amount(),
            );
            txBuilder.set_collateral(txInputsBuilder);
          } else {
            throw new Error(t("errors.setCollateral"));
          }
        }

        const changeConfig = ChangeConfig.new(shelleyChangeAddress);
        // Use UTxO selection strategy 3
        try {
          txBuilder.add_inputs_from_and_change(
            txUnspentOutputs,
            3,
            changeConfig,
          );
        } catch (e) {
          console.error(e);
          // Use UTxO selection strategy 2 if strategy 3 fails
          txBuilder.add_inputs_from_and_change(
            txUnspentOutputs,
            2,
            changeConfig,
          );
        }

        // Build transaction body
        const txBody = txBuilder.build();

        // Make a full transaction, passing in empty witness set
        const tx = Transaction.new(txBody, transactionWitnessSet);
        // Ask wallet to to provide signature (witnesses) for the transaction

        // Create witness set object using the witnesses provided by the wallet
        const txVkeyWitnesses = TransactionWitnessSet.from_bytes(
          Buffer.from(await walletApi.signTx(tx.to_hex(), true), "hex"),
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

        isGuardrailScriptUsed.current = false;

        // eslint-disable-next-line no-console
        console.log(signedTx.to_hex(), "signed tx cbor");
        return resultHash;
        // TODO: type error
        // eslint-disable-next-line @typescript-eslint/no-shadow, @typescript-eslint/no-explicit-any
      } catch (error: any) {
        const name = getItemFromLocalStorage(`${WALLET_LS_KEY}_name`);
        const isWalletConnected = await window.cardano[name].isEnabled();

        if (!isWalletConnected) {
          disconnectWallet();
        }

        console.error(error, "error");
        throw error?.info ?? error;
      }
    },
    [
      isPendingTransaction,
      initTransactionBuilder,
      walletState.changeAddress,
      walletState.usedAddress,
      walletApi,
      t,
      updateTransaction,
      epochParams?.cost_model?.costs,
      disconnectWallet,
      registeredStakeKeysListState,
      stakeKey,
    ],
  );

  const buildCredentialFromBech32Key = useCallback(async (key: string) => {
    try {
      const keyHash = Ed25519KeyHash.from_hex(key);
      return Credential.from_keyhash(keyHash);
    } catch (e) {
      console.error(e);
      throw e;
    }
  }, []);

  const buildStakeKeyRegCert = useCallback(async (): Promise<Certificate> => {
    try {
      if (!stakeKey) {
        throw new Error(t("errors.noStakeKeySelected"));
      }
      const stakeCred = await buildCredentialFromBech32Key(
        stakeKey.substring(2),
      );
      const stakeKeyRegCert = StakeRegistration.new_with_explicit_deposit(
        stakeCred,
        BigNum.from_str(`${epochParams.key_deposit}`),
      );
      return Certificate.new_stake_registration(stakeKeyRegCert);
    } catch (e) {
      console.error(e);
      throw e;
    }
  }, [epochParams?.key_deposit, stakeKey, t]);

  const buildVoteDelegationCert = useCallback(
    async (target: string): Promise<Certificate> => {
      try {
        // Build Vote Delegation Certificate
        if (!stakeKey) {
          throw new Error(t("errors.noStakeKeySelected"));
        }
        // Remove network tag from stake key hash
        const stakeCred = await buildCredentialFromBech32Key(
          stakeKey.substring(2),
        );

        // Create correct DRep
        let targetDRep;
        if (target === AutomatedVotingOptionDelegationId.abstain) {
          targetDRep = DRep.new_always_abstain();
        } else if (target === AutomatedVotingOptionDelegationId.no_confidence) {
          targetDRep = DRep.new_always_no_confidence();
        } else if (target.includes("drep1")) {
          targetDRep = DRep.new_key_hash(Ed25519KeyHash.from_bech32(target));
        } else if (target.includes("drep_script1")) {
          targetDRep = DRep.new_script_hash(ScriptHash.from_hex(target));
        } else {
          targetDRep = DRep.new_key_hash(Ed25519KeyHash.from_hex(target));
        }
        // Create cert object
        const voteDelegationCert = VoteDelegation.new(stakeCred, targetDRep);
        // add cert to tbuilder
        return Certificate.new_vote_delegation(voteDelegationCert);
      } catch (e) {
        console.error(e);
        throw e;
      }
    },
    [stakeKey, t],
  );

  // conway alpha
  const buildDRepRegCert = useCallback(
    async (
      cip95MetadataURL?: string,
      cip95MetadataHash?: string,
    ): Promise<Certificate> => {
      try {
        // Get wallet's DRep key
        const dRepCred = await buildCredentialFromBech32Key(dRepID);

        let dRepRegCert;
        // If there is an anchor
        if (cip95MetadataURL && cip95MetadataHash) {
          const anchor = generateAnchor(cip95MetadataURL, cip95MetadataHash);
          // Create cert object using one Ada as the deposit
          dRepRegCert = DRepRegistration.new_with_anchor(
            dRepCred,
            BigNum.from_str(`${epochParams?.drep_deposit}`),
            anchor,
          );
        } else {
          console.error(t("errors.notUsingAnchor"));
          dRepRegCert = DRepRegistration.new(
            dRepCred,
            BigNum.from_str(`${epochParams?.drep_deposit}`),
          );
        }
        return Certificate.new_drep_registration(dRepRegCert);
      } catch (e) {
        console.error(e);
        throw e;
      }
    },
    [dRepID, epochParams?.drep_deposit, t],
  );

  const buildDRepUpdateCert = useCallback(
    async (
      cip95MetadataURL?: string,
      cip95MetadataHash?: string,
    ): Promise<Certificate> => {
      try {
        // Get wallet's DRep key
        const dRepCred = await buildCredentialFromBech32Key(dRepID);

        let dRepUpdateCert;
        // If there is an anchor
        if (cip95MetadataURL && cip95MetadataHash) {
          const anchor = generateAnchor(cip95MetadataURL, cip95MetadataHash);
          // Create cert object using one Ada as the deposit
          dRepUpdateCert = DRepUpdate.new_with_anchor(dRepCred, anchor);
        } else {
          dRepUpdateCert = DRepUpdate.new(dRepCred);
        }
        return Certificate.new_drep_update(dRepUpdateCert);
      } catch (e) {
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
        const dRepCred = await buildCredentialFromBech32Key(dRepID);

        const dRepRetirementCert = DRepDeregistration.new(
          dRepCred,
          BigNum.from_str(voterDeposit),
        );

        return Certificate.new_drep_deregistration(dRepRetirementCert);
      } catch (e) {
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
        // Get wallet's DRep credential
        const dRepCredential = await buildCredentialFromBech32Key(dRepID);
        // Vote credential
        const voter = Voter.new_drep_credential(dRepCredential);
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

  const addVotingProposalToBuilder = useCallback(
    (
      govActionBuilder: VotingProposalBuilder,
      votingProposal: VotingProposal,
      plutusScript: PlutusScript,
    ) => {
      let redeemer: Redeemer;
      if (isGuardrailScriptUsed.current) {
        const redeemerTag = RedeemerTag.new_voting_proposal();
        const plutusData = PlutusData.new_map(PlutusMap.new());
        const exUnits = ExUnits.new(
          BigNum.from_str("402468"),
          BigNum.from_str("89488792"),
        );
        redeemer = Redeemer.new(
          redeemerTag,
          BigNum.from_str("0"),
          plutusData,
          exUnits,
        );

        const witness = PlutusWitness.new_with_ref_without_datum(
          PlutusScriptSource.new(plutusScript),
          redeemer,
        );
        redeemers.current.push(redeemer);
        govActionBuilder.add_with_plutus_witness(votingProposal, witness);
      } else {
        govActionBuilder.add(votingProposal);
      }

      return govActionBuilder;
    },
    [],
  );

  // new constitution action
  const buildNewConstitutionGovernanceAction = useCallback(
    async ({
      prevGovernanceActionHash,
      prevGovernanceActionIndex,
      constitutionUrl,
      constitutionHash,
      url,
      hash,
      scriptHash,
    }: NewConstitutionProps) => {
      const govActionBuilder = VotingProposalBuilder.new();
      try {
        const constitutionAnchor = generateAnchor(
          constitutionUrl,
          constitutionHash,
        );
        const anchor = generateAnchor(url, hash);

        const rewardAddr = await getRewardAddress();
        if (!rewardAddr) throw new Error("Can not get reward address");

        let constitution;
        if (scriptHash) {
          constitution = Constitution.new_with_script_hash(
            constitutionAnchor,
            ScriptHash.from_hex(scriptHash),
          );
        } else {
          constitution = Constitution.new(constitutionAnchor);
        }

        let newConstitution;
        if (prevGovernanceActionHash && prevGovernanceActionIndex) {
          const prevGovernanceActionId = GovernanceActionId.new(
            TransactionHash.from_hex(prevGovernanceActionHash),
            Number(prevGovernanceActionIndex),
          );
          newConstitution = NewConstitutionAction.new_with_action_id(
            prevGovernanceActionId,
            constitution,
          );
        } else {
          newConstitution = NewConstitutionAction.new(constitution);
        }

        const newConstitutionAction =
          GovernanceAction.new_new_constitution_action(newConstitution);

        // Create voting proposal
        const votingProposal = VotingProposal.new(
          newConstitutionAction,
          anchor,
          rewardAddr,
          BigNum.from_str(epochParams?.gov_action_deposit.toString()),
        );

        govActionBuilder.add(votingProposal);

        return govActionBuilder;
      } catch (e) {
        console.error(e);
      }
    },
    [epochParams?.gov_action_deposit, getRewardAddress],
  );

  // update committee action
  const buildUpdateCommitteeGovernanceAction = useCallback(
    async ({
      prevGovernanceActionHash,
      prevGovernanceActionIndex,
      url,
      hash,
      newCommittee,
      removeCommittee,
      quorumThreshold,
    }: UpdateCommitteeProps) => {
      const govActionBuilder = VotingProposalBuilder.new();
      try {
        const anchor = generateAnchor(url, hash);
        const rewardAddr = await getRewardAddress();
        if (!rewardAddr) throw new Error("Can not get reward address");

        const threshold = UnitInterval.new(
          BigNum.from_str(quorumThreshold.numerator.toString()),
          BigNum.from_str(quorumThreshold.denominator.toString()),
        );
        const committeeToAdd = Committee.new(threshold);
        if (newCommittee) {
          newCommittee.forEach(async (member) => {
            const credential = await buildCredentialFromBech32Key(
              member.committee,
            );
            committeeToAdd.add_member(credential, Number(member.expiryEpoch));
          });
        }
        const committeeToRemoveCredentials = Credentials.new();
        if (removeCommittee) {
          removeCommittee.forEach(async (member) => {
            const credential = await buildCredentialFromBech32Key(member);
            committeeToRemoveCredentials.add(credential);
          });
        }

        let updateCommitteeAction;
        if (prevGovernanceActionHash && prevGovernanceActionIndex) {
          const prevGovernanceActionId = GovernanceActionId.new(
            TransactionHash.from_hex(prevGovernanceActionHash),
            Number(prevGovernanceActionIndex),
          );
          updateCommitteeAction = UpdateCommitteeAction.new_with_action_id(
            prevGovernanceActionId,
            committeeToAdd,
            committeeToRemoveCredentials,
          );
        } else {
          updateCommitteeAction = UpdateCommitteeAction.new(
            committeeToAdd,
            committeeToRemoveCredentials,
          );
        }

        const updateCommitteeGovernanceAction =
          GovernanceAction.new_new_committee_action(updateCommitteeAction);

        const votingProposal = VotingProposal.new(
          updateCommitteeGovernanceAction,
          anchor,
          rewardAddr,
          BigNum.from_str(epochParams?.gov_action_deposit.toString()),
        );

        govActionBuilder.add(votingProposal);

        return govActionBuilder;
      } catch (e) {
        console.error(e);
      }
    },
    [
      buildCredentialFromBech32Key,
      epochParams?.gov_action_deposit,
      getRewardAddress,
    ],
  );

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
          BigNum.from_str(epochParams?.gov_action_deposit.toString()),
        );
        govActionBuilder.add(votingProposal);

        return govActionBuilder;
      } catch (err) {
        console.error(err);
      }
    },
    [epochParams, getRewardAddress],
  );

  // no confidence action
  const buildNoConfidenceGovernanceAction = useCallback(
    async ({ hash, url }: NoConfidenceProps) => {
      const govActionBuilder = VotingProposalBuilder.new();
      try {
        // Create new no confidence action
        const noConfidenceAction = NoConfidenceAction.new();
        const noConfidenceGovAct =
          GovernanceAction.new_no_confidence_action(noConfidenceAction);
        // Create an anchor
        const anchor = generateAnchor(url, hash);

        const rewardAddr = await getRewardAddress();
        if (!rewardAddr) throw new Error("Can not get reward address");

        // Create voting proposal
        const votingProposal = VotingProposal.new(
          noConfidenceGovAct,
          anchor,
          rewardAddr,
          BigNum.from_str(epochParams?.gov_action_deposit.toString()),
        );
        govActionBuilder.add(votingProposal);

        return govActionBuilder;
      } catch (err) {
        console.error(err);
      }
    },
    [epochParams?.gov_action_deposit, getRewardAddress],
  );

  // treasury action
  const buildTreasuryGovernanceAction = useCallback(
    async ({ hash, url, withdrawals }: TreasuryProps) => {
      const govActionBuilder = VotingProposalBuilder.new();
      try {
        const mappedWithdrawals: {
          treasuryTarget: RewardAddress;
          amount: BigNum;
        }[] = [];

        withdrawals.forEach((withdrawal) => {
          const treasuryTarget = RewardAddress.from_address(
            Address.from_bech32(withdrawal.receivingAddress),
          );

          if (!treasuryTarget)
            throw new Error(
              `Can not get tresasury target for address: ${withdrawal.receivingAddress}`,
            );

          const amount = BigNum.from_str(withdrawal.amount);
          mappedWithdrawals.push({ treasuryTarget, amount });
        });

        const treasuryWithdrawals = TreasuryWithdrawals.new();
        mappedWithdrawals.forEach((withdrawal) => {
          treasuryWithdrawals.insert(
            withdrawal.treasuryTarget,
            withdrawal.amount,
          );
        });
        const guardrailPlutusScript = PlutusScript.from_bytes_v3(
          Buffer.from(COMPILED_GUARDRAIL_SCRIPT, "hex"),
        );
        const treasuryAction = TreasuryWithdrawalsAction.new_with_policy_hash(
          treasuryWithdrawals,
          guardrailPlutusScript.hash(),
        );
        isGuardrailScriptUsed.current = true;

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
          BigNum.from_str(epochParams?.gov_action_deposit.toString()),
        );

        addVotingProposalToBuilder(
          govActionBuilder,
          votingProposal,
          guardrailPlutusScript,
        );

        return govActionBuilder;
      } catch (err) {
        console.error(err);
      }
    },
    [
      addVotingProposalToBuilder,
      epochParams?.gov_action_deposit,
      getRewardAddress,
    ],
  );

  const buildProtocolParameterChangeGovernanceAction = useCallback(
    async ({
      prevGovernanceActionHash,
      prevGovernanceActionIndex,
      url,
      hash,
      protocolParamsUpdate,
    }: ProtocolParameterChangeProps) => {
      const govActionBuilder = VotingProposalBuilder.new();

      try {
        const protocolParameterUpdate = ProtocolParamUpdate.new();

        // eslint-disable-next-line no-restricted-syntax
        for (const [key, value] of Object.entries(protocolParamsUpdate)) {
          setProtocolParameterUpdate(protocolParameterUpdate, key, value);
        }

        const guardrailPlutusScript = PlutusScript.from_bytes_v3(
          Buffer.from(COMPILED_GUARDRAIL_SCRIPT, "hex"),
        );
        let protocolParamChangeAction;
        if (prevGovernanceActionHash && prevGovernanceActionIndex) {
          const prevGovernanceActionId = GovernanceActionId.new(
            TransactionHash.from_hex(prevGovernanceActionHash),
            Number(prevGovernanceActionIndex),
          );
          protocolParamChangeAction =
            ParameterChangeAction.new_with_policy_hash_and_action_id(
              prevGovernanceActionId,
              protocolParameterUpdate,
              guardrailPlutusScript.hash(),
            );
        } else {
          protocolParamChangeAction =
            ParameterChangeAction.new_with_policy_hash(
              protocolParameterUpdate,
              guardrailPlutusScript.hash(),
            );
        }

        const protocolParamChangeGovAct =
          GovernanceAction.new_parameter_change_action(
            protocolParamChangeAction,
          );

        // Create an anchor
        const anchor = generateAnchor(url, hash);

        const rewardAddr = await getRewardAddress();

        if (!rewardAddr) throw new Error("Can not get reward address");
        // Create voting proposal
        const votingProposal = VotingProposal.new(
          protocolParamChangeGovAct,
          anchor,
          rewardAddr,
          BigNum.from_str(epochParams?.gov_action_deposit.toString()),
        );
        addVotingProposalToBuilder(
          govActionBuilder,
          votingProposal,
          guardrailPlutusScript,
        );

        return govActionBuilder;
      } catch (err) {
        console.error(err);
      }
    },
    [
      addVotingProposalToBuilder,
      epochParams?.gov_action_deposit,
      getRewardAddress,
    ],
  );

  const buildHardForkGovernanceAction = useCallback(
    async ({
      prevGovernanceActionHash,
      prevGovernanceActionIndex,
      url,
      hash,
      major,
      minor,
    }: HardForkInitiationProps) => {
      const govActionBuilder = VotingProposalBuilder.new();
      try {
        const newProtocolVersion = ProtocolVersion.new(
          Number(major),
          Number(minor),
        );

        let hardForkInitiationAction;
        if (prevGovernanceActionHash && prevGovernanceActionIndex) {
          const prevGovernanceActionId = GovernanceActionId.new(
            TransactionHash.from_hex(prevGovernanceActionHash),
            Number(prevGovernanceActionIndex),
          );
          hardForkInitiationAction =
            HardForkInitiationAction.new_with_action_id(
              prevGovernanceActionId,
              newProtocolVersion,
            );
        } else {
          hardForkInitiationAction =
            HardForkInitiationAction.new(newProtocolVersion);
        }

        const hardForkInitiationGovAct =
          GovernanceAction.new_hard_fork_initiation_action(
            hardForkInitiationAction,
          );

        // Create an anchor
        const anchor = generateAnchor(url, hash);

        const rewardAddr = await getRewardAddress();

        if (!rewardAddr) throw new Error("Can not get reward address");

        // Create voting proposal
        const votingProposal = VotingProposal.new(
          hardForkInitiationGovAct,
          anchor,
          rewardAddr,
          BigNum.from_str(epochParams?.gov_action_deposit.toString()),
        );
        govActionBuilder.add(votingProposal);

        return govActionBuilder;
      } catch (err) {
        console.error(err);
      }
    },
    [epochParams?.gov_action_deposit, getRewardAddress],
  );

  const value = useMemo(
    () => ({
      address,
      buildDRepRegCert,
      buildDRepRetirementCert,
      buildDRepUpdateCert,
      buildHardForkGovernanceAction,
      buildNewInfoGovernanceAction,
      buildProtocolParameterChangeGovernanceAction,
      buildNoConfidenceGovernanceAction,
      buildNewConstitutionGovernanceAction,
      buildUpdateCommitteeGovernanceAction,
      buildSignSubmitConwayCertTx,
      buildStakeKeyRegCert,
      buildTreasuryGovernanceAction,
      buildVote,
      buildVoteDelegationCert,
      dRepID,
      disconnectWallet,
      enable,
      error,
      getChangeAddress,
      isEnableLoading,
      isEnabled,
      isMainnet,
      isPendingTransaction,
      isStakeKeyRegistered,
      pendingTransaction,
      pubDRepKey,
      registeredStakeKeysListState,
      setStakeKey,
      stakeKey,
      stakeKeys,
      walletApi,
      walletName,
    }),
    [
      address,
      buildDRepRegCert,
      buildDRepRetirementCert,
      buildDRepUpdateCert,
      buildHardForkGovernanceAction,
      buildNewInfoGovernanceAction,
      buildProtocolParameterChangeGovernanceAction,
      buildNoConfidenceGovernanceAction,
      buildNewConstitutionGovernanceAction,
      buildUpdateCommitteeGovernanceAction,
      buildSignSubmitConwayCertTx,
      buildStakeKeyRegCert,
      buildTreasuryGovernanceAction,
      buildVote,
      buildVoteDelegationCert,
      dRepID,
      disconnectWallet,
      enable,
      error,
      getChangeAddress,
      isEnableLoading,
      isEnabled,
      isMainnet,
      isPendingTransaction,
      isStakeKeyRegistered,
      pendingTransaction,
      pubDRepKey,
      registeredStakeKeysListState,
      setStakeKey,
      stakeKey,
      stakeKeys,
      walletApi,
      walletName,
    ],
  );

  return <CardanoContext.Provider value={value} {...props} />;
};

function useCardano() {
  const { networkName } = useAppContext();

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
          `${NETWORK_INFO_KEY}_${walletName}`,
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
                dataTestId: "info-about-network-modal",
                message: (
                  <Trans
                    i18nKey="system.description"
                    components={[
                      <Link
                        onClick={() => openInNewTab("https://sancho.network/")}
                        sx={{ cursor: "pointer" }}
                      />,
                    ]}
                    values={{
                      networkName,
                    }}
                  />
                ),
                title: t("system.title", {
                  networkName,
                }),
                buttonText: t("ok"),
              },
            });
            setItemToLocalStorage(`${NETWORK_INFO_KEY}_${walletName}`, true);
          }
          return result;
        }
        // TODO: type error
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (e: any) {
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
    [context, closeModal, addSuccessAlert, t, openModal, networkName, navigate],
  );

  const disconnectWallet = useCallback(async () => {
    await context.disconnectWallet();
  }, [context]);

  return { ...context, enable, disconnectWallet };
}

export { CardanoProvider, useCardano };
