/**
 * The declaration against the structure: which methods exist (availability is
 * the interface's to say) and which option values are honoured (the capability
 * document's). A declared option must work; an undeclared one must be refused,
 * never silently ignored. Runs against ./dist.
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { capabilities, createKoiosProvider } from '../dist/index.js';
import { EPOCH, chainRoutes, drepId, filterRows, hash, paramsRow, provider, txHash, at, actionId, stakeAddr } from './helpers.mjs';

const { chainData } = createKoiosProvider({ network: 'mainnet', fetch: async () => new Response('[]') });

test('required surface exists', () => {
  const required = {
    network: ['getNetworkInfo', 'getProtocolParams', 'getStakeDistribution'],
    accounts: ['get', 'getDelegation'],
    transactions: ['get'],
    system: ['getIdentity', 'getCapabilities', 'getHealth'],
  };
  for (const [area, methods] of Object.entries(required)) for (const m of methods) assert.equal(typeof chainData[area][m], 'function', `${area}.${m}`);
  const g = chainData.governance;
  for (const m of ['list', 'get']) assert.equal(typeof g.dreps[m], 'function');
  for (const m of ['list', 'get', 'getEnacted']) assert.equal(typeof g.proposals[m], 'function');
  for (const m of ['list', 'get']) assert.equal(typeof g.pools[m], 'function');
  for (const m of ['getCommittee', 'getMember', 'getConstitution']) assert.equal(typeof g.committee[m], 'function');
});

test('optional members: present exactly where Koios serves them', () => {
  const present = [
    chainData.network.getTreasury,
    chainData.network.getGenesisParams,
    chainData.accounts.getPoolDelegation,
    chainData.accounts.getVotingPower,
    chainData.governance.dreps.listVotes,
    chainData.governance.dreps.listUpdateHistory,
    chainData.governance.dreps.getCounts,
    chainData.governance.proposals.listVotes,
    chainData.governance.proposals.listActivity,
    chainData.governance.pools.listVotes,
  ];
  for (const fn of present) assert.equal(typeof fn, 'function');
  // Omitted, with the reason in the module header: no per-delegator snapshot, no delegation targets on account_updates.
  assert.equal(chainData.governance.dreps.listDelegators, undefined);
  assert.equal(chainData.accounts.listDelegationHistory, undefined);
});

test('the declaration is well-formed', async () => {
  const caps = capabilities();
  assert.deepEqual((await chainData.system.getCapabilities()).data, caps);
  assert.ok(caps.sorts.proposals.includes('newest') && caps.sorts.proposals.includes('oldest'));
  assert.ok(caps.search.includes('exactId'));
  assert.ok(caps.voteAggregate.length > 0);
  assert.ok(!('available' in caps) && !Object.values(caps).some((v) => typeof v === 'boolean'), 'no availability booleans');
  assert.deepEqual([...caps.optionalArguments].sort(), ['proposals.voterContextOnList', 'protocolParams.epoch']);
});

/* A fake Koios rich enough for every declared option to run end to end. */
const d1 = drepId(1);
const proposal = {
  block_time: at(EPOCH - 1),
  proposal_id: actionId(1),
  proposal_tx_hash: txHash(1),
  proposal_index: 0,
  proposal_type: 'InfoAction',
  proposal_description: { tag: 'InfoAction' },
  previous_gov_action_proposal_id: null,
  deposit: '1',
  return_address: stakeAddr(1),
  proposed_epoch: EPOCH - 1,
  ratified_epoch: null,
  enacted_epoch: null,
  dropped_epoch: null,
  expired_epoch: null,
  expiration: EPOCH + 5,
  meta_url: null,
  meta_hash: null,
};
const summary = {
  proposal_type: 'InfoAction',
  epoch_no: EPOCH,
  drep_active_yes_vote_power: '1',
  drep_yes_vote_power: '1',
  drep_active_no_vote_power: '0',
  drep_no_vote_power: '9',
  drep_active_abstain_vote_power: '0',
  drep_always_no_confidence_vote_power: '0',
  pool_active_yes_vote_power: '1',
  pool_yes_vote_power: '1',
  pool_active_no_vote_power: '0',
  pool_no_vote_power: '9',
  pool_active_abstain_vote_power: '0',
  pool_passive_always_abstain_vote_power: '0',
  pool_passive_always_no_confidence_vote_power: '0',
};
const rich = () =>
  provider({
    ...chainRoutes,
    drep_list: (url) => filterRows([{ drep_id: d1, hex: hash(1), has_script: false, registered: true }], url),
    drep_info: [{ drep_id: d1, hex: hash(1), has_script: false, drep_status: 'registered', deposit: '1', active: true, expires_epoch_no: EPOCH + 1, amount: '5', meta_url: null, meta_hash: null, live_delegator_count: 0 }],
    drep_updates: [{ drep_id: d1, hex: hash(1), has_script: false, update_tx_hash: txHash(2), cert_index: 0, block_time: at(600), action: 'registered', deposit: '1', meta_url: null, meta_hash: null }],
    proposal_list: (url) => filterRows([proposal], url),
    proposal_voting_summary: [summary],
    epoch_params: (url) => [paramsRow(Number(url.searchParams.get('_epoch_no') ?? EPOCH))],
    committee_info: [{ proposal_id: null, quorum_numerator: 2, quorum_denominator: 3, members: [] }],
    vote_list: [],
  });

test('every declared DRep sort and filter is honoured; undeclared ones are refused', async () => {
  const { chainData: c } = rich();
  const caps = capabilities();
  for (const sort of caps.sorts.dreps) await c.governance.dreps.list({ page: 1, size: 5, sort });
  for (const sort of ['votingPower', 'registrationDate', 'activity', 'random'].filter((s) => !caps.sorts.dreps.includes(s))) {
    await assert.rejects(c.governance.dreps.list({ page: 1, size: 5, sort }), (e) => e.code === 'CAPABILITY_UNSUPPORTED');
  }
  if (caps.filters.dreps.includes('status')) await c.governance.dreps.list({ page: 1, size: 5, status: ['active'] });
  if (caps.filters.dreps.includes('kind')) await c.governance.dreps.list({ page: 1, size: 5, kind: ['anonymous'] });
  for (const sort of caps.sorts.votes ?? []) await c.governance.dreps.listVotes(d1, { page: 1, size: 5, sort });
});

test('every declared proposal sort and filter is honoured; undeclared sorts are refused', async () => {
  const { chainData: c } = rich();
  const caps = capabilities();
  for (const sort of caps.sorts.proposals) {
    const { data } = await c.governance.proposals.list({ page: 1, size: 5, sort });
    assert.equal(data.total, 1);
  }
  for (const sort of ['newest', 'oldest', 'soonestToExpire', 'mostYesVotes', 'highestParticipation'].filter((s) => !caps.sorts.proposals.includes(s))) {
    await assert.rejects(c.governance.proposals.list({ page: 1, size: 5, sort }), (e) => e.code === 'CAPABILITY_UNSUPPORTED');
  }
  if (caps.filters.proposals.includes('type')) await c.governance.proposals.list({ page: 1, size: 5, type: ['InfoAction'] });
  if (caps.filters.proposals.includes('status')) await c.governance.proposals.list({ page: 1, size: 5, status: ['live', 'enacted'] });
});

test('declared vote aggregate representations are the ones served', async () => {
  const { chainData: c } = rich();
  const caps = capabilities();
  const { data } = await c.governance.proposals.get(proposal.proposal_id);
  for (const a of data.voteAggregates) assert.ok(caps.voteAggregate.includes(a.representation), a.representation);
});

test('declared optional arguments are honoured', async () => {
  const { chainData: c, calls } = rich();
  const caps = capabilities();
  if (caps.optionalArguments.includes('protocolParams.epoch')) {
    assert.equal((await c.network.getProtocolParams({ epoch: 640 })).data.epoch, 640);
    assert.equal(calls.filter((x) => x.endpoint === 'epoch_params').pop().url.searchParams.get('_epoch_no'), '640');
  }
  if (caps.optionalArguments.includes('proposals.voterContextOnList')) {
    const { data } = await c.governance.proposals.list({ page: 1, size: 5, voterId: d1 });
    assert.ok(data.elements.every((a) => 'myVote' in a));
    const none = await c.governance.proposals.list({ page: 1, size: 5, voterId: d1, voted: true });
    assert.equal(none.data.total, 0);
  }
});
