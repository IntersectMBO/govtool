/**
 * Chain Data API — `/network/*`
 *
 * Chain state and ledger parameters. Aggregates and computed counts belong to
 * the entity that owns them, not here (SPEC.md §5.1).
 *
 * Epoch and block listings are deliberately absent: GovTool is not a block
 * explorer, and chain data is in scope only where a governance decision
 * depends on it.
 */

import type {
  Bech32,
  ChainPoint,
  EpochNo,
  Envelope,
  Lovelace,
  NetworkId,
  Ratio,
  Timestamp,
} from './common';

export interface NetworkInfo {
  network: NetworkId;
  era: string;
  tip: ChainPoint;
  currentEpoch: EpochNo;
}

/* ------------------------------------------------------------------------- */
/* Protocol parameters                                                         */
/* ------------------------------------------------------------------------- */

/**
 * DRep voting thresholds, as the ledger groups them. Every action detail screen
 * renders one of these, so they are load-bearing rather than reference data.
 *
 * Exact rationals. Some sources serve them as floats (`0.67`); prefer a source
 * that carries the rational, otherwise reconstruct by bounded-denominator
 * continued fractions and say so.
 */
export interface DRepThresholds {
  motionNoConfidence: Ratio;
  committeeNormal: Ratio;
  committeeNoConfidence: Ratio;
  updateToConstitution: Ratio;
  hardForkInitiation: Ratio;
  ppNetworkGroup: Ratio;
  ppEconomicGroup: Ratio;
  ppTechnicalGroup: Ratio;
  ppGovGroup: Ratio;
  treasuryWithdrawal: Ratio;
}

/** Stake pool voting thresholds. */
export interface PoolThresholds {
  motionNoConfidence: Ratio;
  committeeNormal: Ratio;
  committeeNoConfidence: Ratio;
  hardForkInitiation: Ratio;
  ppSecurityGroup: Ratio;
}

/** Plutus execution budget: memory units and CPU steps. */
export interface ExecutionUnits {
  memory: number;
  steps: number;
}

export type PlutusLanguage = 'PlutusV1' | 'PlutusV2' | 'PlutusV3';

/**
 * Per-language cost models, each an array of integers in the ledger's
 * parameter order — the form a transaction's script data hash is computed
 * from. A language absent from the object has no cost model in force.
 */
export type CostModels = Partial<Record<PlutusLanguage, number[]>>;

/**
 * Every protocol parameter the ledger holds in the current era, typed, named
 * and camelCase. NOT a raw source row: source bookkeeping (row ids, cost-model
 * row ids, the epoch nonce) is not a protocol parameter and is not carried.
 * Parameters the ledger no longer has (`decentralisation`, `extraEntropy`,
 * `minUtxoValue`) are not carried either.
 *
 * All of it is required. A wallet building a Plutus-script transaction needs
 * `costModels` and `executionUnitPrices`, and a parameter-change screen shows
 * the current value of whichever parameter an action changes, so a partial
 * set breaks real flows rather than only a display.
 *
 * A provider with more may return a subtype (SPEC.md §3.3).
 */
export interface ProtocolParams {
  epoch: EpochNo;
  protocolVersion: { major: number; minor: number };

  /* Governance */
  /** Epochs a governance action stays live before expiring. */
  govActionLifetime: number;
  govActionDeposit: Lovelace;
  drepDeposit: Lovelace;
  /** Epochs of inactivity after which a DRep becomes inactive. */
  drepActivity: number;
  committeeMinSize: number;
  /** Maximum committee term, in epochs. */
  committeeMaxTermLength: number;
  drepThresholds: DRepThresholds;
  poolThresholds: PoolThresholds;

  /* Fees and deposits */
  /** Fee per transaction byte (`a`). */
  minFeeA: number;
  /** Fixed fee per transaction (`b`). */
  minFeeB: number;
  /** Fee per byte of reference scripts. */
  minFeeRefScriptCostPerByte: Ratio;
  keyDeposit: Lovelace;
  poolDeposit: Lovelace;
  coinsPerUtxoByte: Lovelace;

  /* Size and execution limits */
  maxBlockBodySize: number;
  maxBlockHeaderSize: number;
  maxTxSize: number;
  maxValSize: number;
  maxTxExecutionUnits: ExecutionUnits;
  maxBlockExecutionUnits: ExecutionUnits;
  /** Collateral required, as a percentage of the fee. */
  collateralPercentage: number;
  maxCollateralInputs: number;

  /* Plutus */
  /** Price of one memory unit and one CPU step, in lovelace. */
  executionUnitPrices: { memory: Ratio; steps: Ratio };
  costModels: CostModels;

  /* Stake pools and monetary policy */
  /** Furthest epoch ahead a pool may schedule its retirement (`eMax`). */
  poolRetireMaxEpoch: number;
  /** Target number of pools (`nOpt`). */
  stakePoolTargetNum: number;
  /** Pledge influence (`a0`). */
  poolPledgeInfluence: Ratio;
  /** Monetary expansion (`rho`). */
  monetaryExpansion: Ratio;
  /** Treasury cut (`tau`). */
  treasuryCut: Ratio;
  minPoolCost: Lovelace;
}

/* ------------------------------------------------------------------------- */
/* Genesis parameters                                                          */
/* ------------------------------------------------------------------------- */

/**
 * The Shelley genesis: the network's fixed constants, which no governance
 * action changes. Optional, because not every source keeps the genesis file.
 *
 * `epochLength` and `slotLength` are the SHELLEY values. On a network that
 * began in Byron (mainnet), earlier epochs had other lengths, so converting an
 * epoch to a time needs the era boundaries as well, not these alone.
 */
export interface GenesisParams {
  networkMagic: number;
  networkId: 'Mainnet' | 'Testnet';
  /** Wall-clock time of slot 0. */
  systemStart: Timestamp;
  /** Slots per epoch. */
  epochLength: number;
  /** Seconds per slot. */
  slotLength: number;
  /** Fraction of slots expected to produce a block (`f`). */
  activeSlotsCoefficient: Ratio;
  /** Blocks after which the chain is final (`k`). */
  securityParam: number;
  slotsPerKesPeriod: number;
  maxKesEvolutions: number;
  updateQuorum: number;
  maxLovelaceSupply: Lovelace;
}

/* ------------------------------------------------------------------------- */
/* Stake and treasury                                                          */
/* ------------------------------------------------------------------------- */

export interface StakeDistribution {
  epoch?: EpochNo;
  /** The epoch-boundary snapshot. The ONLY valid tally denominator. */
  totalActiveStake: Lovelace;
  /** Moves within an epoch. Never a tally denominator. */
  totalLiveStake?: Lovelace;
  totalStakeControlledByDReps?: Lovelace;
  totalStakeControlledBySPOs?: Lovelace;
  /**
   * Stake delegated to the predefined targets. Under Conway, `alwaysAbstain`
   * stake is excluded from the DRep denominator and `alwaysNoConfidence` acts
   * as a standing No, so a percentage computed without them is not the number
   * the ledger decides by.
   */
  alwaysAbstainVotingPower?: Lovelace;
  alwaysNoConfidenceVotingPower?: Lovelace;
}

export interface Treasury {
  epoch?: EpochNo;
  balance: Lovelace;
  reserves: Lovelace;
}

/* ------------------------------------------------------------------------- */
/* API                                                                         */
/* ------------------------------------------------------------------------- */

export interface NetworkApi {
  getNetworkInfo(): Promise<Envelope<NetworkInfo>>;

  /**
   * Parameters in force, or — when `epoch` is given and the provider declares
   * `protocolParams.epoch` — the parameters for a past epoch.
   */
  getProtocolParams(q?: { epoch?: EpochNo }): Promise<Envelope<ProtocolParams>>;

  getStakeDistribution(): Promise<Envelope<StakeDistribution>>;

  /** Optional. */
  getTreasury?(q?: { epoch?: EpochNo }): Promise<Envelope<Treasury>>;

  /** Optional. The network's fixed genesis constants. */
  getGenesisParams?(): Promise<Envelope<GenesisParams>>;
}

export type { Bech32 };
