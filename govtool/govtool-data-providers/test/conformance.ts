/**
 * Conformance checks for the contracts in ../src.
 *
 * This file is never executed and never shipped — it is type-checked by
 * `npm run typecheck`. For an interfaces-only package that is the meaningful
 * test, and it covers the two ways such a package breaks:
 *
 *   1. A service contract that cannot be implemented (a signature that does not
 *      admit an implementation). `notImplementedChainData` and friends below are
 *      annotated with the service interface, so the compiler checks every method.
 *
 *   2. A domain type that cannot be constructed (a field required in a
 *      combination no provider can produce). The fixtures build each central
 *      entity from literals, which is what a provider's mapping layer does.
 *
 * The stubs double as a skeleton: a new provider can copy one and replace the
 * `unsupported` calls route by route, declaring the gaps at
 * `/system/capabilities` until it is complete.
 */

import type {
  ChainDataApiV1,
  DRep,
  DRepVotingPowerEntry,
  GovAction,
  RoleTally,
  VoteRecord,
} from '../src/chain-data';
import type {
  DRepMetadataBody,
  MetadataProjection,
  MetadataServiceV1,
} from '../src/metadata';
import type { PinningServiceV1, PinRecord } from '../src/pinning';

/**
 * Every method resolves the same way: a provider that cannot serve a route
 * fails loudly rather than returning an empty success. The zero-argument arrows
 * below are deliberate — a narrower function satisfies a wider signature, so
 * the stub stays readable without naming parameters it ignores.
 */
const unsupported = (route: string): never => {
  throw new Error(`CAPABILITY_UNSUPPORTED: ${route}`);
};

/* ------------------------------------------------------------------------- */
/* 1. The service contracts admit an implementation                           */
/* ------------------------------------------------------------------------- */

export const notImplementedChainData: ChainDataApiV1 = {
  network: {
    getNetworkInfo: () => unsupported('network.getNetworkInfo'),
    listEpochs: () => unsupported('network.listEpochs'),
    getProtocolParams: () => unsupported('network.getProtocolParams'),
    listBlocks: () => unsupported('network.listBlocks'),
    getStakeDistribution: () => unsupported('network.getStakeDistribution'),
    getTreasury: () => unsupported('network.getTreasury'),
  },
  accounts: {
    get: () => unsupported('accounts.get'),
    listDelegationHistory: () => unsupported('accounts.listDelegationHistory'),
    listStakeEvents: () => unsupported('accounts.listStakeEvents'),
    getVotingPower: () => unsupported('accounts.getVotingPower'),
    getDelegation: () => unsupported('accounts.getDelegation'),
  },
  governance: {
    dreps: {
      list: () => unsupported('governance.dreps.list'),
      get: () => unsupported('governance.dreps.get'),
      listVotes: () => unsupported('governance.dreps.listVotes'),
      listDelegators: () => unsupported('governance.dreps.listDelegators'),
      listDelegationEvents: () =>
        unsupported('governance.dreps.listDelegationEvents'),
      listHistory: () => unsupported('governance.dreps.listHistory'),
      getVotingPower: () => unsupported('governance.dreps.getVotingPower'),
      getVotingPowers: () => unsupported('governance.dreps.getVotingPowers'),
    },
    pools: {
      list: () => unsupported('governance.pools.list'),
      get: () => unsupported('governance.pools.get'),
      listVotes: () => unsupported('governance.pools.listVotes'),
    },
    proposals: {
      list: () => unsupported('governance.proposals.list'),
      get: () => unsupported('governance.proposals.get'),
      listVotes: () => unsupported('governance.proposals.listVotes'),
      getTallies: () => unsupported('governance.proposals.getTallies'),
      listActivity: () => unsupported('governance.proposals.listActivity'),
      getEnacted: () => unsupported('governance.proposals.getEnacted'),
      listByTx: () => unsupported('governance.proposals.listByTx'),
    },
    votes: {
      list: () => unsupported('governance.votes.list'),
      get: () => unsupported('governance.votes.get'),
    },
    committee: {
      getCommittee: () => unsupported('governance.committee.getCommittee'),
      getMember: () => unsupported('governance.committee.getMember'),
      getConstitution: () =>
        unsupported('governance.committee.getConstitution'),
      listConstitutionHistory: () =>
        unsupported('governance.committee.listConstitutionHistory'),
    },
    metrics: {
      get: () => unsupported('governance.metrics.get'),
    },
    voters: {
      resolve: () => unsupported('governance.voters.resolve'),
      list: () => unsupported('governance.voters.list'),
    },
  },
  transactions: {
    get: () => unsupported('transactions.get'),
  },
  // Optional on the contract; a provider with no CIP-179 statement omits it.
  surveys: {
    getDefinition: () => unsupported('surveys.getDefinition'),
  },
  system: {
    getCapabilities: () => unsupported('system.getCapabilities'),
    getHealth: () => unsupported('system.getHealth'),
  },
};

export const notImplementedMetadata: MetadataServiceV1 = {
  get: () => unsupported('metadata.get'),
  getMany: () => unsupported('metadata.getMany'),
  getRaw: () => unsupported('metadata.getRaw'),
  refresh: () => unsupported('metadata.refresh'),
  validate: () => unsupported('metadata.validate'),
  stats: () => unsupported('metadata.stats'),
};

export const notImplementedPinning: PinningServiceV1 = {
  pin: () => unsupported('pinning.pin'),
  prepare: () => unsupported('pinning.prepare'),
  getPin: () => unsupported('pinning.getPin'),
  listPins: () => unsupported('pinning.listPins'),
  repin: () => unsupported('pinning.repin'),
  unpin: () => unsupported('pinning.unpin'),
  getPolicy: () => unsupported('pinning.getPolicy'),
  getHealth: () => unsupported('pinning.getHealth'),
};

/* ------------------------------------------------------------------------- */
/* 2. The domain entities are constructible                                   */
/* ------------------------------------------------------------------------- */

export const drepMetadataFixture: MetadataProjection<DRepMetadataBody> = {
  id: 'c0ffee',
  anchor: { url: 'ipfs://bafy', dataHash: 'deadbeef' },
  standard: 'CIP119',
  status: 'valid',
  body: {
    givenName: 'Example DRep',
    objectives: 'Vote on everything.',
    identityReferences: [
      { '@type': 'Identity', label: 'X', uri: 'https://x.com/example' },
    ],
  },
  fetchedAt: '2026-09-18T00:00:00Z',
};

/** A fully-hydrated DRep, as `governance.dreps.get` would return one. */
export const drepFixture: DRep = {
  role: 'drep',
  id: 'drep1abc',
  hash: 'ab12',
  isScriptBased: false,
  kind: 'drep',
  registration: {
    status: 'active',
    registeredAt: { epoch: 500, time: '2026-01-01T00:00:00Z' },
    registrationTx: { txHash: 'ab12', index: 0 },
    retiredAt: null,
    retirementTx: null,
    deposit: '500000000',
  },
  metadata: drepMetadataFixture,
  isCip119Compliant: true,
  votingPower: { amount: '12500000000', epoch: 500, basis: 'active' },
  liveVotingPower: { amount: '12600000000', epoch: 500, basis: 'live' },
  delegators: { active: 12, live: 13 },
  activity: {
    votesCast: 8,
    notVotedCount: 2,
    lastVotedAt: { epoch: 499, time: '2025-12-20T00:00:00Z' },
    inactiveFromEpoch: 520,
    participationRate: 0.8,
  },
};

/** A direct ("sole") voter — same entity, distinguished only by `kind`. */
export const directVoterFixture: DRep = {
  role: 'drep',
  id: 'drep1xyz',
  hash: 'cd34',
  isScriptBased: false,
  kind: 'directVoter',
  registration: {
    status: 'active',
    registeredAt: { epoch: 501, time: '2026-02-01T00:00:00Z' },
    registrationTx: { txHash: 'cd34' },
    retiredAt: null,
    retirementTx: null,
    deposit: '500000000',
  },
  metadata: null,
  isCip119Compliant: false,
  votingPower: null,
};

/** A predefined option in a voting-power listing: real power, no credential. */
export const predefinedVotingPowerFixture: DRepVotingPowerEntry = {
  subject: {
    kind: 'predefined',
    option: 'alwaysNoConfidence',
    view: 'drep_always_no_confidence',
  },
  votingPower: { amount: '3707653134137', basis: 'active' },
  givenName: null,
};

export const drepVotingPowerFixture: DRepVotingPowerEntry = {
  subject: { kind: 'drep', drep: drepFixture },
  votingPower: { amount: '12500000000', basis: 'active' },
};

export const tallyFixture: RoleTally = {
  role: 'drep',
  stake: { yes: '1000', no: '250', abstain: '10' },
  count: { yes: 4, no: 1, abstain: 1 },
  notVotedStake: '500',
  totalEligibleStake: '1760',
  threshold: { numerator: 67, denominator: 100 },
  passing: false,
};

/** A TreasuryWithdrawals action, exercising the discriminated body union. */
export const govActionFixture: GovAction = {
  id: 'gov_action1abc',
  txHash: 'ef56',
  index: 0,
  type: 'TreasuryWithdrawals',
  body: {
    type: 'TreasuryWithdrawals',
    withdrawals: [{ stakeAddress: 'stake1abc', amount: '1000000000' }],
    totalAmount: '1000000000',
    guardrailsScriptHash: null,
  },
  lifecycle: {
    status: 'live',
    submitted: { epoch: 500, time: '2026-01-05T00:00:00Z' },
    submittedTx: { txHash: 'ef56', index: 0 },
    expires: { epoch: 506, time: '2026-02-05T00:00:00Z' },
    ratifiedAt: null,
    enactedAt: null,
    droppedAt: null,
    expiredAt: null,
  },
  deposit: '100000000000',
  depositReturnAddress: 'stake1abc',
  proposedBy: 'stake1abc',
  previousAction: null,
  metadata: null,
  tallies: [tallyFixture],
};

/** An InfoAction — the body variant with no payload at all. */
export const infoActionBodyFixture: GovAction['body'] = { type: 'InfoAction' };

export const voteFixture: VoteRecord = {
  proposal: { id: 'gov_action1abc', txHash: 'ef56', index: 0 },
  voter: { role: 'drep', id: 'drep1abc', hash: 'ab12', isScriptBased: false },
  vote: 'yes',
  txRef: { txHash: '9a88' },
  at: { epoch: 501, time: '2026-01-10T00:00:00Z' },
  votingPower: { amount: '12500000000', epoch: 500, basis: 'active' },
  rationale: null,
  isCurrent: true,
};

export const pinFixture: PinRecord = {
  cid: 'bafybeigdyrzt',
  url: 'ipfs://bafybeigdyrzt',
  gatewayUrls: ['https://ipfs.io/ipfs/bafybeigdyrzt'],
  dataHash: 'deadbeef',
  byteSize: 512,
  contentType: 'application/ld+json',
  status: 'pinned',
  pinnedAt: '2026-09-18T00:00:00Z',
  replicas: [
    { backend: 'pinata', status: 'pinned', pinnedAt: '2026-09-18T00:00:00Z' },
  ],
};
