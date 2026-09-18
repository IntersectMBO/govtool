/**
 * Chain Data API — `/network/*`
 *
 * Network identity, epochs, blocks, protocol parameters and stake totals.
 * Nothing governance-specific: this is the ledger context every other module
 * is interpreted against.
 */

import type {
  BlockNo,
  ChainPoint,
  Envelope,
  EpochNo,
  Hex,
  Lovelace,
  NetworkId,
  Ratio,
  StakeBasis,
  Timestamp,
} from './common';

export interface NetworkInfo {
  network: NetworkId;
  /** Optional: db-sync's `meta` table records the network name but not the magic. */
  networkMagic?: number;
  era?: string;
  tip: ChainPoint;
  epoch: {
    no: EpochNo;
    startTime?: Timestamp;
    endTime?: Timestamp;
  };
}

export interface EpochSummary {
  epoch: EpochNo;
  startTime: Timestamp;
  endTime: Timestamp;
  durationSeconds: number;
  firstBlock?: BlockNo;
  lastBlock?: BlockNo;
}

export interface BlockSummary {
  block: BlockNo;
  blockHash: Hex;
  slot: number;
  epoch: EpochNo;
  time: Timestamp;
  txCount?: number;
}

/**
 * Conway protocol parameters for one epoch.
 *
 * `epoch` and `raw` are the contract. The typed fields are a convenience a
 * provider fills when it can read them without interpretation, and leaves
 * `undefined` otherwise. Thresholds (`dvt`, `pvt`) are the usual gap: db-sync
 * stores them as floating-point, and a float cannot honestly be turned back
 * into the on-chain ratio.
 */
export interface ProtocolParams {
  epoch: EpochNo;
  /** Deposits & costs */
  govActionDeposit?: Lovelace;
  drepDeposit?: Lovelace;
  keyDeposit?: Lovelace;
  poolDeposit?: Lovelace;
  minFeeA?: number;
  minFeeB?: number;
  minFeeRefScriptCostPerByte?: number | null;
  coinsPerUtxoByte?: Lovelace | null;
  /** Governance timing */
  govActionLifetime?: number;
  drepActivity?: number;
  committeeMinSize?: number;
  committeeMaxTermLength?: number;
  /** DRep voting thresholds */
  dvt?: {
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
  };
  /** SPO voting thresholds */
  pvt?: {
    motionNoConfidence: Ratio;
    committeeNormal: Ratio;
    committeeNoConfidence: Ratio;
    hardForkInitiation: Ratio;
    ppSecurityGroup: Ratio;
  };
  protocolVersion?: { major: number; minor: number };
  /** The complete parameter set as the provider reports it, for diffing a
   * ParameterChange action against the parameters in force. */
  raw: Record<string, unknown>;
}

export interface StakeDistribution {
  /** Optional: a provider that reads the totals from a view may not have the epoch alongside. */
  epoch?: EpochNo;
  /** Epoch-boundary snapshot — the only valid tally denominator. */
  totalActiveStake?: Lovelace;
  /** Current total; changes within the epoch. Never use as a tally denominator. */
  totalLiveStake?: Lovelace;
  /**
   * The governance breakdown. All four are optional: computing them means
   * aggregating the whole DRep distribution, which db-sync does in one query
   * and a per-entity HTTP API cannot do at all. A provider that serves only
   * `totalActiveStake` is still useful as a tally denominator.
   */
  totalStakeControlledByDReps?: Lovelace;
  totalStakeControlledBySPOs?: Lovelace;
  alwaysAbstainVotingPower?: Lovelace;
  alwaysNoConfidenceVotingPower?: Lovelace;
}

export interface Treasury {
  epoch: EpochNo;
  balance: Lovelace;
  reserves: Lovelace;
  /** Change over the previous epoch. */
  delta?: Lovelace;
}

export interface NetworkApi {
  /** `GET /network` */
  getNetworkInfo(): Promise<Envelope<NetworkInfo>>;
  /** `GET /network/epochs` */
  listEpochs(q?: {
    limit?: number;
    before?: EpochNo;
  }): Promise<Envelope<EpochSummary[]>>;
  /** `GET /network/epochs/{epoch}/params` — omit `epoch` for current. */
  getProtocolParams(q?: { epoch?: EpochNo }): Promise<Envelope<ProtocolParams>>;
  /** `GET /network/blocks` / `GET /network/blocks/{blockNo}` */
  listBlocks(q?: {
    limit?: number;
    block?: BlockNo;
  }): Promise<Envelope<BlockSummary[]>>;
  /** `GET /network/stake-distribution` */
  getStakeDistribution(q?: {
    epoch?: EpochNo;
    basis?: StakeBasis;
  }): Promise<Envelope<StakeDistribution>>;
  /** `GET /network/treasury` */
  getTreasury(q?: { epoch?: EpochNo }): Promise<Envelope<Treasury>>;
}
