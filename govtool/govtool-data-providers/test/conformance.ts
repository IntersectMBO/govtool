/**
 * A worked fixture of every entity, type-checked and never executed.
 *
 * For a types-only package this IS the meaningful test: it proves the contract
 * is inhabitable — that a provider can actually build one of everything — and
 * it fails the build the moment a shape changes underneath it.
 *
 * Add a fixture here when an entity is added.
 */

import type {
  Account,
  ChainDataApiV1,
  Committee,
  Constitution,
  DRep,
  DRepCounts,
  DRepDelegator,
  DRepVoteRow,
  Delegation,
  Envelope,
  GovAction,
  NetworkInfo,
  Page,
  GenesisParams,
  ProtocolParams,
  ProviderCapabilities,
  ProviderIdentity,
  SpoVoter,
  StakeDistribution,
  TransactionState,
  VoteAggregate,
  VoteRecord,
} from '../src/chain-data';
import { ChainDataError, ratioEquals } from '../src/chain-data';
import type { MetadataServiceV1, MetadataResult } from '../src/metadata';
import type { PinningServiceV1 } from '../src/pinning';
import type { GovernanceIndexV1 } from '../src/index-provider';
import type { CommitteeInfoProviderV1 } from '../src/committee-info';
import type { TransactionMonitorV1 } from '../src/tx-monitor';

const HALF = { numerator: 1, denominator: 2 };
const TWO_THIRDS = { numerator: 2, denominator: 3 };
const DREP_ID = 'drep1yfaaaaa270yjt6tu5skndugekprf5ykv5jshanl0c6gqx5qpstskf';
const STAKE = 'stake1uxaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
const TX = 'd'.repeat(64);

/* Ratios are compared by cross-multiplication, never structurally. */
export const ratiosEqual: boolean = ratioEquals(HALF, {
  numerator: 2,
  denominator: 4,
});

/* -- network --------------------------------------------------------------- */

export const networkInfo: NetworkInfo = {
  network: 'mainnet',
  era: 'conway',
  tip: {
    epoch: 580,
    slot: 100,
    block: 11_000_000,
    time: '2026-09-22T00:00:00Z',
  },
  currentEpoch: 580,
};

export const protocolParams: ProtocolParams = {
  epoch: 580,
  protocolVersion: { major: 10, minor: 0 },
  govActionLifetime: 6,
  govActionDeposit: '100000000000',
  drepDeposit: '500000000',
  drepActivity: 20,
  committeeMinSize: 5,
  committeeMaxTermLength: 146,
  drepThresholds: {
    motionNoConfidence: { numerator: 67, denominator: 100 },
    committeeNormal: { numerator: 67, denominator: 100 },
    committeeNoConfidence: { numerator: 60, denominator: 100 },
    updateToConstitution: { numerator: 75, denominator: 100 },
    hardForkInitiation: { numerator: 60, denominator: 100 },
    ppNetworkGroup: { numerator: 67, denominator: 100 },
    ppEconomicGroup: { numerator: 67, denominator: 100 },
    ppTechnicalGroup: { numerator: 67, denominator: 100 },
    ppGovGroup: { numerator: 75, denominator: 100 },
    treasuryWithdrawal: { numerator: 67, denominator: 100 },
  },
  poolThresholds: {
    motionNoConfidence: HALF,
    committeeNormal: HALF,
    committeeNoConfidence: HALF,
    hardForkInitiation: HALF,
    ppSecurityGroup: HALF,
  },
  keyDeposit: '2000000',
  poolDeposit: '500000000',
  coinsPerUtxoByte: '4310',
  minFeeA: 44,
  minFeeB: 155_381,
  maxTxSize: 16_384,
  maxValSize: 5000,
  minFeeRefScriptCostPerByte: { numerator: 15, denominator: 1 },
  maxBlockBodySize: 90_112,
  maxBlockHeaderSize: 1100,
  maxTxExecutionUnits: { memory: 14_000_000, steps: 10_000_000_000 },
  maxBlockExecutionUnits: { memory: 62_000_000, steps: 20_000_000_000 },
  collateralPercentage: 150,
  maxCollateralInputs: 3,
  executionUnitPrices: {
    memory: { numerator: 577, denominator: 10_000 },
    steps: { numerator: 721, denominator: 10_000_000 },
  },
  costModels: { PlutusV1: [100_788, 420, 1], PlutusV3: [100_788, 420, 1, 1] },
  poolRetireMaxEpoch: 18,
  stakePoolTargetNum: 500,
  poolPledgeInfluence: { numerator: 3, denominator: 10 },
  monetaryExpansion: { numerator: 3, denominator: 1000 },
  treasuryCut: { numerator: 1, denominator: 5 },
  minPoolCost: '170000000',
};

export const genesisParams: GenesisParams = {
  networkMagic: 764_824_073,
  networkId: 'Mainnet',
  systemStart: '2017-09-23T21:44:51Z',
  epochLength: 432_000,
  slotLength: 1,
  activeSlotsCoefficient: { numerator: 1, denominator: 20 },
  securityParam: 2160,
  slotsPerKesPeriod: 129_600,
  maxKesEvolutions: 62,
  updateQuorum: 5,
  maxLovelaceSupply: '45000000000000000',
};

export const stakeDistribution: StakeDistribution = {
  epoch: 580,
  totalActiveStake: '21357778069987000',
  totalStakeControlledByDReps: '5000000000000000',
  alwaysAbstainVotingPower: '1000000000000000',
};

/* -- account --------------------------------------------------------------- */

export const account: Account = {
  stakeAddress: STAKE,
  stakeKeyHash: 'a'.repeat(56),
  isRegistered: true,
  balance: {
    total: '1500000000',
    utxo: '1400000000',
    rewards: '90000000',
    rewardsRest: '10000000',
  },
};

export const delegatedToDRep: Delegation = {
  target: { kind: 'drep', drep: { role: 'drep', id: DREP_ID } },
  txRef: { txHash: TX },
  since: { epoch: 570 },
};

/** The predefined targets are NOT DReps and carry no credential. */
export const delegatedToAbstain: Delegation = {
  target: { kind: 'predefined', target: 'alwaysAbstain' },
  txRef: null,
};

/* -- drep ------------------------------------------------------------------ */

export const drep: DRep = {
  role: 'drep',
  id: DREP_ID,
  kind: 'drep',
  anchor: {
    url: 'https://example.invalid/drep.json',
    dataHash: 'b'.repeat(64),
  },
  registration: {
    latest: {
      txRef: { txHash: TX },
      at: { epoch: 540, time: '2026-01-01T00:00:00Z' },
    },
    latestUpdate: null,
  },
  status: 'active',
  expiryEpoch: 600,
  votingPower: { amount: '27412054204974', basis: 'active', epoch: 580 },
  activity: { voted: 7, votable: 12 },
};

/** An anonymous DRep is exactly one that registered with no anchor. */
export const anonymousDRep: DRep = {
  role: 'drep',
  id: DREP_ID,
  kind: 'anonymous',
  anchor: null,
  registration: {
    latest: { txRef: { txHash: TX }, at: { epoch: 545 } },
    latestUpdate: null,
  },
  status: 'active',
  votingPower: null,
};

export const delegator: DRepDelegator = {
  stakeAddress: STAKE,
  activeVotingPower: '1500000000',
  previousDRepId: DREP_ID,
};

export const drepCounts: DRepCounts = {
  totalRegistered: 1684,
  totalActive: 1200,
  totalInactive: 484,
  anonymous: 96,
};

/** The vote listing is a union: voted rows carry a choice, not-voted rows do not. */
export const voteRows: DRepVoteRow[] = [
  {
    voted: true,
    action: { id: 'gov_action1aaa', type: 'InfoAction', title: 'A survey' },
    choice: 'yes',
    anchor: null,
    txRef: { txHash: TX },
  },
  {
    voted: false,
    action: { id: 'gov_action1bbb', type: 'TreasuryWithdrawals' },
  },
];

/* -- proposal -------------------------------------------------------------- */

export const voteAggregate: VoteAggregate = {
  role: 'drep',
  representation: 'stake',
  yes: '4000000000000',
  no: '1000000000000',
  abstain: '500000000000',
  notVoted: '2000000000000',
  totalEligible: '7500000000000',
  threshold: { numerator: 67, denominator: 100 },
  passing: false,
};

/** A head-count aggregate, as a source without stake weighting reports it. */
export const countAggregate: VoteAggregate = {
  role: 'cc',
  representation: 'count',
  yes: '4',
  no: '1',
  abstain: '0',
  notVoted: '2',
  totalEligible: '7',
  threshold: TWO_THIRDS,
  passing: true,
};

export const proposal: GovAction = {
  id: 'gov_action1w2w64uhelz0cg2np7m37hal905tdd7jpzm3fcyc3g7qvkwgfppgqqfsggt5',
  txHash: '7'.repeat(64),
  index: 0,
  type: 'TreasuryWithdrawals',
  body: {
    type: 'TreasuryWithdrawals',
    withdrawals: [{ stakeAddress: STAKE, amount: '1000000000' }],
    totalAmount: '1000000000',
  },
  lifecycle: {
    status: 'live',
    submitted: { epoch: 578, time: '2026-09-01T00:00:00Z' },
    submittedTx: { txHash: '7'.repeat(64) },
    expires: { epoch: 584 },
    ratifiedAt: null,
    enactedAt: null,
    droppedAt: null,
    expiredAt: null,
  },
  anchor: { url: 'ipfs://abc', dataHash: 'c'.repeat(64) },
  deposit: '100000000000',
  depositReturnAddress: STAKE,
  previousAction: null,
  voteAggregates: [voteAggregate],
};

/** Every body variant is constructible — D7 requires all seven. */
export const bodies: GovAction['body'][] = [
  { type: 'InfoAction' },
  { type: 'NoConfidence' },
  { type: 'ParameterChange', changes: { minFeeA: 45 } },
  { type: 'HardForkInitiation', protocolVersion: { major: 11, minor: 0 } },
  proposal.body,
  {
    type: 'UpdateCommittee',
    added: [{ coldCredential: 'cc_cold1aaa', termExpiryEpoch: 700 }],
    removed: [{ coldCredential: 'cc_cold1bbb' }],
    quorum: TWO_THIRDS,
  },
  {
    type: 'NewConstitution',
    anchor: { url: 'ipfs://const', dataHash: 'd'.repeat(64) },
  },
];

/* -- votes, pools, committee ----------------------------------------------- */

/** A committee vote carries the HOT key; cold is the optional resolution. */
export const committeeVote: VoteRecord = {
  voter: { role: 'cc', hot: 'cc_hot1aaa', cold: 'cc_cold1aaa' },
  choice: 'yes',
  anchor: null,
  txRef: { txHash: TX },
};

export const drepVote: VoteRecord = {
  voter: { role: 'drep', id: DREP_ID },
  choice: 'no',
  anchor: { url: 'https://example.invalid/why.json', dataHash: 'e'.repeat(64) },
  txRef: { txHash: TX },
};

export const pool: SpoVoter = {
  role: 'spo',
  id: 'pool1z5uqdk7dzdxaae5633fqfcu2eqzy3a3rgtuvy087fdld7yws0xt',
  poolId: 'pool1z5uqdk7dzdxaae5633fqfcu2eqzy3a3rgtuvy087fdld7yws0xt',
  anchor: {
    url: 'https://example.invalid/pool.json',
    dataHash: 'f'.repeat(64),
  },
  votingPower: { amount: '57382396556341', basis: 'active' },
};

export const committee: Committee = {
  members: [
    {
      role: 'cc',
      coldCredential:
        'cc_cold1zg90nyz8hjgwpkg8x3n4fzse4pggndmxuultspm53g4dxcgjkqykp',
      hotCredential:
        'cc_hot1q2ccqmm64956nc65aw65gcn9354c3ldlfqp03qh2v2y5ueqhzw8g4',
      termStartEpoch: 500,
      termExpiryEpoch: 799,
      hasResigned: false,
    },
  ],
  quorum: TWO_THIRDS,
  enactedBy: null,
};

/** Anchor only — the document is the metadata service's business. */
export const constitution: Constitution = {
  anchor: { url: 'ipfs://constitution', dataHash: '1'.repeat(64) },
  guardrailsScriptHash: null,
  enactedBy: null,
  enactedAt: { epoch: 500 },
};

export const transaction: TransactionState = { txHash: TX, onChain: true };

/* -- declarations ---------------------------------------------------------- */

export const identity: ProviderIdentity = {
  id: 'fixture',
  name: 'Fixture Provider',
};

/**
 * A provider that orders nothing on DReps but honours both required proposal
 * sorts, searches by exact id only, and expresses aggregates as stake.
 */
export const capabilities: ProviderCapabilities = {
  sorts: { dreps: [], proposals: ['newest', 'oldest'] },
  filters: { dreps: ['status'], proposals: ['type'] },
  search: ['exactId'],
  voteAggregate: ['stake'],
  optionalArguments: ['protocolParams.epoch'],
};

/* -- the whole surface ----------------------------------------------------- */

const envelope = <T>(data: T): Envelope<T> => ({
  data,
  meta: { provider: 'fixture', network: 'mainnet' },
});

const page = <T>(elements: T[]): Envelope<Page<T>> =>
  envelope({ elements, total: elements.length });

const unsupported = () =>
  Promise.reject(new ChainDataError('CAPABILITY_UNSUPPORTED', 'fixture'));

/**
 * A minimal conformant provider: every REQUIRED method present, every optional
 * one omitted. Omission is how the interface says "not supported" — there is no
 * declaration for it.
 */
export const minimalProvider: ChainDataApiV1 = {
  network: {
    getNetworkInfo: () => Promise.resolve(envelope(networkInfo)),
    getProtocolParams: () => Promise.resolve(envelope(protocolParams)),
    getStakeDistribution: () => Promise.resolve(envelope(stakeDistribution)),
  },
  accounts: {
    get: () => Promise.resolve(envelope(account)),
    getDelegation: () => Promise.resolve(envelope(delegatedToDRep)),
  },
  governance: {
    dreps: {
      list: () => Promise.resolve(page([drep])),
      get: () => Promise.resolve(envelope(drep)),
    },
    proposals: {
      list: () => Promise.resolve(page([proposal])),
      get: () => Promise.resolve(envelope(proposal)),
      getEnacted: () => Promise.resolve(envelope(null)),
    },
    pools: {
      list: () => Promise.resolve(page([pool])),
      get: () => Promise.resolve(envelope(pool)),
    },
    committee: {
      getCommittee: () => Promise.resolve(envelope(committee)),
      getMember: () => Promise.resolve(envelope(committee.members[0]!)),
      getConstitution: () => Promise.resolve(envelope(constitution)),
    },
  },
  transactions: { get: () => Promise.resolve(envelope(transaction)) },
  system: {
    getIdentity: () => Promise.resolve(envelope(identity)),
    getCapabilities: () => Promise.resolve(envelope(capabilities)),
    getHealth: () => Promise.resolve(envelope({ status: 'healthy' as const })),
  },
};

/* -- the optional components ------------------------------------------------ */

const ok: MetadataResult = {
  ok: true,
  hash: 'b'.repeat(64),
  body: { givenName: 'A DRep' },
  fetchedAt: '2026-09-22T00:00:00Z',
};

export const metadataService: MetadataServiceV1 = {
  getMetadata: () => Promise.resolve(ok),
  getCipMetadata: <TBody>() => Promise.resolve(ok as MetadataResult<TBody>),
  refresh: () => Promise.resolve({ refetched: false, result: ok }),
  getReport: () => Promise.resolve(null),
  listReports: () => Promise.resolve([]),
};

export const pinningService: PinningServiceV1 = {
  pinData: () => Promise.resolve('bafy...'),
  getDataCid: () => Promise.resolve('bafy...'),
  unpin: () => Promise.resolve(),
  fetch: () => Promise.resolve(new Uint8Array()),
  getHealth: () => Promise.resolve({ status: 'healthy' }),
};

/** An indexer that only does DReps — the other halves are simply absent. */
export const drepOnlyIndex: GovernanceIndexV1 = {
  dreps: { searchDReps: () => Promise.resolve(page([drep])) },
};

export const committeeInfo: CommitteeInfoProviderV1 = {
  getMemberInfo: () => Promise.resolve(null),
};

export const txMonitor: TransactionMonitorV1 = {
  add: (_txHash, callback) =>
    callback({ txHash: TX, state: 'confirmed', confirmations: 3 }),
};

void unsupported;
