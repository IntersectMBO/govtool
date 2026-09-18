/**
 * Live contract-conformance check.
 *
 * `npm run smoke` asks "did the route answer?". This asks the harder
 * question: **is what it answered a valid instance of the contract?** It
 * walks each response and reports
 *
 *   - required fields that are missing,
 *   - lovelace fields returned as numbers instead of decimal strings,
 *   - `EpochStamp`s that carry neither half, which 0.3.0 forbids,
 *   - `Page`/`Envelope` envelopes missing their required keys.
 *
 * A unit test with fixtures cannot find these, because the fixture is written
 * by the same person as the mapper. Run it after touching any mapper.
 *
 *   npm run build && npm run conformance
 */
import { createKoiosProvider } from '../dist/index.js';

const provider = createKoiosProvider({
  network: process.env.KOIOS_NETWORK ?? 'mainnet',
  token: process.env.KOIOS_TOKEN,
  timeoutMs: 45000,
  maxRetries: 1,
});

const problems = [];
let checked = 0;

const isLovelace = (v) => typeof v === 'string' && /^-?\d+$/.test(v);

function note(where, message) {
  problems.push(`${where}: ${message}`);
}

/** Required keys must be present (may be null where the contract says nullable). */
function requireKeys(where, object, keys) {
  if (object === null || object === undefined) {
    note(where, 'entity itself is absent');
    return;
  }
  for (const key of keys) {
    if (!(key in object)) note(where, `missing required key \`${key}\``);
  }
}

/** 0.3.0: both halves optional, at least one always set. */
function checkStamp(where, stamp) {
  if (stamp === null || stamp === undefined) return;
  const hasEpoch = typeof stamp.epoch === 'number';
  const hasTime = typeof stamp.time === 'string';
  if (!hasEpoch && !hasTime) {
    note(where, 'EpochStamp carries neither `epoch` nor `time`');
  }
  if (hasTime && Number.isNaN(Date.parse(stamp.time))) {
    note(where, `EpochStamp.time is not parseable: ${stamp.time}`);
  }
}

function checkLovelace(where, value, { nullable = true } = {}) {
  if (value === null || value === undefined) {
    if (!nullable) note(where, 'required lovelace field is absent');
    return;
  }
  if (!isLovelace(value)) {
    note(where, `lovelace must be a decimal string, got ${typeof value} ${JSON.stringify(value)}`);
  }
  if (typeof value === 'string' && value.startsWith('-')) {
    note(where, `lovelace is negative: ${value}`);
  }
}

function checkVotingPower(where, power) {
  if (power === null || power === undefined) return;
  requireKeys(where, power, ['amount', 'basis']);
  checkLovelace(`${where}.amount`, power.amount, { nullable: false });
  if (!['active', 'live'].includes(power.basis)) {
    note(where, `basis must be active|live, got ${JSON.stringify(power.basis)}`);
  }
  if ('epoch' in power && power.epoch !== undefined && typeof power.epoch !== 'number') {
    note(where, 'epoch must be a number when present');
  }
}

function checkVoterRef(where, ref) {
  requireKeys(where, ref, ['role', 'id', 'hash', 'isScriptBased']);
  if (!ref) return;
  if (!['drep', 'spo', 'cc', 'direct'].includes(ref.role)) {
    note(where, `role must be drep|spo|cc|direct, got ${JSON.stringify(ref.role)}`);
  }
  if (typeof ref.hash === 'string' && ref.hash === '') {
    note(where, 'hash is empty');
  }
}

function checkGovActionRef(where, ref) {
  requireKeys(where, ref, ['id', 'txHash', 'index']);
  if (!ref) return;
  if (typeof ref.id === 'string' && !ref.id.startsWith('gov_action1')) {
    note(where, `id must be CIP-129 gov_action1…, got ${ref.id}`);
  }
  if (typeof ref.txHash === 'string' && !/^[0-9a-f]{64}$/.test(ref.txHash)) {
    note(where, `txHash must be 64 lowercase hex, got ${ref.txHash}`);
  }
}

function checkEnvelope(where, envelope) {
  requireKeys(where, envelope, ['data', 'meta']);
  if (envelope?.meta?.asOf !== undefined) {
    requireKeys(`${where}.meta.asOf`, envelope.meta.asOf, ['epoch', 'block']);
  }
}

function checkPage(where, page) {
  requireKeys(where, page, ['elements', 'nextCursor']);
  if (page && !Array.isArray(page.elements)) {
    note(where, 'elements must be an array');
  }
}

async function check(name, fn) {
  checked += 1;
  try {
    await fn();
  } catch (error) {
    if (error?.code === 'CAPABILITY_UNSUPPORTED') return; // declared gap
    note(name, `threw ${error?.code ?? ''} ${error?.message ?? error}`);
  }
}

/* ------------------------------------------------------------------- */

await check('network.getNetworkInfo', async () => {
  const env = await provider.network.getNetworkInfo();
  checkEnvelope('network.getNetworkInfo', env);
  requireKeys('NetworkInfo', env.data, ['network', 'tip', 'epoch']);
  requireKeys('NetworkInfo.tip', env.data.tip, ['epoch', 'block']);
});

await check('network.getProtocolParams', async () => {
  const { data } = await provider.network.getProtocolParams();
  requireKeys('ProtocolParams', data, ['epoch', 'raw']);
  for (const f of ['govActionDeposit', 'drepDeposit', 'keyDeposit', 'poolDeposit']) {
    checkLovelace(`ProtocolParams.${f}`, data[f]);
  }
});

await check('network.getStakeDistribution', async () => {
  const { data } = await provider.network.getStakeDistribution();
  for (const f of [
    'totalActiveStake', 'totalLiveStake', 'totalStakeControlledByDReps',
    'totalStakeControlledBySPOs', 'alwaysAbstainVotingPower',
    'alwaysNoConfidenceVotingPower',
  ]) checkLovelace(`StakeDistribution.${f}`, data[f]);
});

await check('network.getTreasury', async () => {
  const { data } = await provider.network.getTreasury();
  requireKeys('Treasury', data, ['epoch', 'balance', 'reserves']);
  checkLovelace('Treasury.balance', data.balance, { nullable: false });
  checkLovelace('Treasury.reserves', data.reserves, { nullable: false });
});

let firstDRep;
await check('governance.dreps.list', async () => {
  const env = await provider.governance.dreps.list({ limit: 25, expand: ['metadata', 'activity'] });
  checkEnvelope('dreps.list', env);
  checkPage('dreps.list.data', env.data);
  for (const [i, drep] of env.data.elements.entries()) {
    const at = `DRep[${i}]`;
    checkVoterRef(at, drep);
    requireKeys(at, drep, ['role', 'kind', 'registration', 'metadata', 'votingPower']);
    requireKeys(`${at}.registration`, drep.registration, ['deposit']);
    checkLovelace(`${at}.registration.deposit`, drep.registration?.deposit);
    checkStamp(`${at}.registration.registeredAt`, drep.registration?.registeredAt);
    checkStamp(`${at}.registration.retiredAt`, drep.registration?.retiredAt);
    checkVotingPower(`${at}.votingPower`, drep.votingPower);
    if (drep.activity) {
      requireKeys(`${at}.activity`, drep.activity, ['votesCast']);
      checkStamp(`${at}.activity.lastVotedAt`, drep.activity.lastVotedAt);
    }
    if (!['drep', 'directVoter'].includes(drep.kind)) {
      note(at, `kind must be drep|directVoter, got ${JSON.stringify(drep.kind)}`);
    }
  }
  // Prefer a DRep with real power: a retired one carries amount "0" and
  // leaves the activity and history shapes unexercised.
  firstDRep =
    env.data.elements.find(
      (d) => d.votingPower && BigInt(d.votingPower.amount) > 0n,
    ) ?? env.data.elements[0];
});

if (firstDRep) {
  await check('governance.dreps.get', async () => {
    const { data } = await provider.governance.dreps.get(firstDRep.id);
    checkVoterRef('dreps.get', data);
    requireKeys('dreps.get', data, ['kind', 'registration', 'metadata', 'votingPower']);
    checkVotingPower('dreps.get.votingPower', data.votingPower);
  });
  await check('governance.dreps.listDelegators', async () => {
    const env = await provider.governance.dreps.listDelegators(firstDRep.id, { limit: 3 });
    checkPage('dreps.listDelegators', env.data);
    for (const [i, d] of env.data.elements.entries()) {
      const at = `DRepDelegator[${i}]`;
      requireKeys(at, d, ['stakeAddress', 'basis', 'balance', 'since', 'txRef']);
      checkLovelace(`${at}.balance.total`, d.balance?.total, { nullable: false });
      checkStamp(`${at}.since`, d.since);
    }
  });
  await check('governance.dreps.listHistory', async () => {
    const env = await provider.governance.dreps.listHistory(firstDRep.id, { limit: 3 });
    checkPage('dreps.listHistory', env.data);
    for (const [i, e] of env.data.elements.entries()) {
      requireKeys(`DRepHistoryEvent[${i}]`, e, ['type', 'txRef']);
      checkStamp(`DRepHistoryEvent[${i}].at`, e.at);
    }
  });
}

let firstProposal;
await check('governance.proposals.list', async () => {
  const env = await provider.governance.proposals.list({ limit: 20, expand: ['metadata', 'tallies'] });
  checkEnvelope('proposals.list', env);
  checkPage('proposals.list.data', env.data);
  for (const [i, action] of env.data.elements.entries()) {
    const at = `GovAction[${i}]`;
    checkGovActionRef(at, action);
    requireKeys(at, action, ['type', 'lifecycle', 'previousAction', 'metadata']);
    requireKeys(`${at}.lifecycle`, action.lifecycle, [
      'status', 'submittedTx', 'expires', 'ratifiedAt', 'enactedAt',
      'droppedAt', 'expiredAt',
    ]);
    for (const k of ['submitted', 'expires', 'ratifiedAt', 'enactedAt', 'droppedAt', 'expiredAt']) {
      checkStamp(`${at}.lifecycle.${k}`, action.lifecycle?.[k]);
    }
    checkLovelace(`${at}.deposit`, action.deposit);
    if (action.previousAction) checkGovActionRef(`${at}.previousAction`, action.previousAction);
    if (action.body?.type === 'TreasuryWithdrawals') {
      checkLovelace(`${at}.body.totalAmount`, action.body.totalAmount, { nullable: false });
      for (const [j, w] of (action.body.withdrawals ?? []).entries()) {
        checkLovelace(`${at}.body.withdrawals[${j}].amount`, w.amount, { nullable: false });
      }
    }
    for (const [j, t] of (action.tallies ?? []).entries()) {
      const tat = `${at}.tallies[${j}]`;
      requireKeys(tat, t, ['role']);
      for (const choice of ['yes', 'no', 'abstain']) {
        if (t.stake) checkLovelace(`${tat}.stake.${choice}`, t.stake[choice], { nullable: false });
      }
      checkLovelace(`${tat}.notVotedStake`, t.notVotedStake);
      checkLovelace(`${tat}.totalEligibleStake`, t.totalEligibleStake);
    }
  }
  firstProposal = env.data.elements[0];
});

if (firstProposal) {
  await check('governance.proposals.listVotes', async () => {
    const env = await provider.governance.proposals.listVotes(firstProposal.id, { limit: 5 });
    checkPage('proposals.listVotes', env.data);
    for (const [i, v] of env.data.elements.entries()) {
      const at = `VoteRecord[${i}]`;
      requireKeys(at, v, ['proposal', 'voter', 'vote', 'txRef', 'votingPower', 'rationale', 'isCurrent']);
      checkGovActionRef(`${at}.proposal`, v.proposal);
      checkVoterRef(`${at}.voter`, v.voter);
      checkStamp(`${at}.at`, v.at);
      checkVotingPower(`${at}.votingPower`, v.votingPower);
      if (!['yes', 'no', 'abstain'].includes(v.vote)) {
        note(at, `vote must be yes|no|abstain, got ${JSON.stringify(v.vote)}`);
      }
    }
  });
  await check('governance.proposals.getTallies', async () => {
    const { data } = await provider.governance.proposals.getTallies(firstProposal.id);
    for (const [i, t] of data.entries()) requireKeys(`RoleTally[${i}]`, t, ['role']);
  });
}

await check('governance.committee.getCommittee', async () => {
  const { data } = await provider.governance.committee.getCommittee();
  requireKeys('Committee', data, ['members', 'quorum', 'enactedBy']);
  requireKeys('Committee.quorum', data.quorum, ['numerator', 'denominator']);
  for (const [i, m] of (data.members ?? []).entries()) {
    const at = `CommitteeMember[${i}]`;
    checkVoterRef(at, m);
    requireKeys(at, m, ['coldCredential', 'hotCredential', 'termStartEpoch', 'termExpiryEpoch', 'hasResigned']);
  }
});

await check('governance.committee.getConstitution', async () => {
  const { data } = await provider.governance.committee.getConstitution();
  requireKeys('Constitution', data, ['anchor', 'guardrailsScriptHash', 'enactedBy', 'enactedAt', 'document']);
  checkStamp('Constitution.enactedAt', data.enactedAt);
});

await check('governance.votes.list', async () => {
  const env = await provider.governance.votes.list({ limit: 5 });
  checkPage('votes.list', env.data);
  for (const [i, v] of env.data.elements.entries()) {
    requireKeys(`VoteRecord[${i}]`, v, ['proposal', 'voter', 'vote', 'txRef', 'votingPower', 'rationale', 'isCurrent']);
    checkStamp(`VoteRecord[${i}].at`, v.at);
  }
});

const STAKE = process.env.KOIOS_STAKE_ADDRESS
  ?? 'stake1u9ya9ajt7gmmdqlr503t2s72duvtmc4kwdck299568x4xjqqd22nl';

await check('accounts.get', async () => {
  const { data } = await provider.accounts.get(STAKE, {
    expand: ['balance', 'votingPower', 'delegation', 'poolDelegation'],
  });
  requireKeys('Account', data, ['stakeAddress', 'stakeKeyHash', 'isRegistered', 'isScriptBased']);
  if (data.balance) {
    checkLovelace('Account.balance.total', data.balance.total, { nullable: false });
    for (const f of ['utxo', 'rewards', 'rewardsRest']) {
      checkLovelace(`Account.balance.${f}`, data.balance[f]);
    }
  }
  checkVotingPower('Account.votingPower', data.votingPower);
  if (data.delegation) {
    requireKeys('Account.delegation', data.delegation, ['target', 'txRef']);
    checkStamp('Account.delegation.since', data.delegation.since);
  }
});

await check('accounts.listDelegationHistory', async () => {
  const env = await provider.accounts.listDelegationHistory(STAKE, { limit: 3 });
  checkPage('accounts.listDelegationHistory', env.data);
  for (const [i, e] of env.data.elements.entries()) {
    requireKeys(`DelegationHistoryEvent[${i}]`, e, ['kind', 'txRef', 'to']);
    checkStamp(`DelegationHistoryEvent[${i}].at`, e.at);
  }
});

await check('accounts.listStakeEvents', async () => {
  const env = await provider.accounts.listStakeEvents(STAKE, { limit: 3 });
  checkPage('accounts.listStakeEvents', env.data);
  for (const [i, e] of env.data.elements.entries()) {
    requireKeys(`StakeRegistrationEvent[${i}]`, e, ['action', 'txRef']);
    checkStamp(`StakeRegistrationEvent[${i}].at`, e.at);
  }
});

await check('transactions.get', async () => {
  const { data } = await provider.transactions.get(
    process.env.KOIOS_TX ?? 'c9b11588e508c325a260754086a6c5f40fe0ba4daa1c92a77ee59c8b55949755',
  );
  requireKeys('TransactionState', data, ['txHash', 'status']);
  checkStamp('TransactionState.includedAt', data.includedAt);
});

await check('governance.pools.get', async () => {
  const { data } = await provider.governance.pools.get(
    'pool1m83drqwlugdt9jn7jkz8hx3pne53acfkd539d9cj8yr92dr4k9y',
  );
  checkVoterRef('SpoVoter', data);
  requireKeys('SpoVoter', data, ['poolId', 'votingPower']);
  checkVotingPower('SpoVoter.votingPower', data.votingPower);
  for (const f of ['liveStake', 'activeStake', 'pledge']) {
    checkLovelace(`SpoVoter.${f}`, data[f]);
  }
});

if (firstDRep) {
  await check('governance.dreps.listVotes', async () => {
    const env = await provider.governance.dreps.listVotes(firstDRep.id, { limit: 3 });
    checkPage('dreps.listVotes', env.data);
    for (const [i, voted] of env.data.elements.entries()) {
      const at = `VotedGovAction[${i}]`;
      requireKeys(at, voted, ['vote', 'proposal']);
      checkGovActionRef(`${at}.proposal`, voted.proposal);
      requireKeys(`${at}.proposal.lifecycle`, voted.proposal?.lifecycle, [
        'status', 'submittedTx', 'expires', 'ratifiedAt', 'enactedAt',
        'droppedAt', 'expiredAt',
      ]);
      checkStamp(`${at}.proposal.lifecycle.submitted`, voted.proposal?.lifecycle?.submitted);
      if (voted.vote) {
        checkVoterRef(`${at}.vote.voter`, voted.vote.voter);
        checkStamp(`${at}.vote.at`, voted.vote.at);
        checkVotingPower(`${at}.vote.votingPower`, voted.vote.votingPower);
      }
    }
  });
}

if (firstProposal) {
  await check('governance.proposals.listActivity', async () => {
    const env = await provider.governance.proposals.listActivity(firstProposal.id, { limit: 5 });
    checkPage('proposals.listActivity', env.data);
    for (const [i, e] of env.data.elements.entries()) {
      const at = `GovActionActivityEvent[${i}]`;
      requireKeys(at, e, ['type', 'at', 'txRef']);
      checkStamp(`${at}.at`, e.at);
      if (e.voter) checkVoterRef(`${at}.voter`, e.voter);
    }
  });
  await check('governance.proposals.listByTx', async () => {
    const { data } = await provider.governance.proposals.listByTx(firstProposal.txHash);
    for (const [i, ref] of data.entries()) checkGovActionRef(`listByTx[${i}]`, ref);
  });
}

await check('governance.votes.get', async () => {
  const first = (await provider.governance.votes.list({ limit: 1 })).data.elements[0];
  if (!first) return;
  const { data } = await provider.governance.votes.get(first.txRef.txHash);
  requireKeys('votes.get', data, ['proposal', 'voter', 'vote', 'txRef', 'votingPower', 'rationale', 'isCurrent']);
  checkVoterRef('votes.get.voter', data.voter);
  checkStamp('votes.get.at', data.at);
});

await check('governance.pools.listVotes', async () => {
  const env = await provider.governance.pools.listVotes(
    'pool1m83drqwlugdt9jn7jkz8hx3pne53acfkd539d9cj8yr92dr4k9y', { limit: 3 });
  checkPage('pools.listVotes', env.data);
  for (const [i, v] of env.data.elements.entries()) {
    checkVoterRef(`pools.listVotes[${i}].voter`, v.voter);
    checkStamp(`pools.listVotes[${i}].at`, v.at);
  }
});

await check('governance.committee.getMember', async () => {
  const members = (await provider.governance.committee.getCommittee()).data.members;
  if (!members?.length) return;
  const { data } = await provider.governance.committee.getMember(members[0].id);
  checkVoterRef('committee.getMember', data);
  requireKeys('committee.getMember', data, [
    'coldCredential', 'hotCredential', 'termStartEpoch', 'termExpiryEpoch', 'hasResigned',
  ]);
});

await check('network.listBlocks', async () => {
  const { data } = await provider.network.listBlocks({ limit: 3 });
  for (const [i, b] of data.entries()) {
    requireKeys(`BlockSummary[${i}]`, b, ['block', 'blockHash', 'slot', 'epoch', 'time']);
  }
});

await check('surveys.getDefinition', async () => {
  if (provider.surveys === undefined) return;
  const { data } = await provider.surveys.getDefinition(
    process.env.KOIOS_TX ?? 'c9b11588e508c325a260754086a6c5f40fe0ba4daa1c92a77ee59c8b55949755');
  if (data !== null) requireKeys('SurveyDefinition', data, ['txHash', 'metadataLabel', 'payloadCborHex']);
});

/* ------------------------------------------------------------------- */

console.log(`conformance: ${checked} routes checked\n`);
if (problems.length === 0) {
  console.log('no contract violations found');
} else {
  console.log(`${problems.length} contract violation(s):\n`);
  for (const p of problems) console.log(`  ✗ ${p}`);
}
process.exit(problems.length === 0 ? 0 : 1);
