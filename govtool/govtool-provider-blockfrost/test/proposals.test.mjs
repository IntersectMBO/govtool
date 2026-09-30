/**
 * Proposals, committee, pools and capabilities over a fake Blockfrost:
 * bodies, lifecycle, anchors from CBOR, aggregates, vote listing quirks,
 * lineage, and the declaration versus the structure.
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { capabilities } from '../dist/index.js';
import {
  EPOCH,
  PARAMS,
  PREDEFINED,
  VOTE,
  VOTER,
  coldId,
  committeeRow,
  drepId,
  drepRow,
  govActionId,
  hash28,
  hash32,
  hotId,
  memberRow,
  paged,
  poolId,
  provider,
  rejectsWith,
  txCbor,
  txRow,
} from './fake.mjs';

const P = (n) => hash32(0x80 + n);
const prev = (n) => ({ txId: P(n), govActionIx: 0 });

/** [governance_type, description, epochs] per proposal, in chain order. */
const SPECS = [
  ['parameter_change', { tag: 'ParameterChange', contents: [null, { txFeePerByte: 45, dRepVotingThresholds: null }, null] }, { enacted_epoch: 560, ratified_epoch: 559 }],
  ['parameter_change', { tag: 'ParameterChange', contents: [prev(0), { maxTxSize: 20000, govActionDeposit: 5 }, hash28(0xfa)] }, { enacted_epoch: 570, ratified_epoch: 569 }],
  [
    'new_committee',
    {
      tag: 'UpdateCommittee',
      contents: [null, [{ keyHash: hash28(0x31) }], { [`scriptHash-${hash28(0x32)}`]: 700 }, { numerator: 2, denominator: 3 }],
    },
    { enacted_epoch: 575, ratified_epoch: 574 },
  ],
  ['no_confidence', { tag: 'NoConfidence', contents: prev(2) }, { enacted_epoch: 580, ratified_epoch: 579 }],
  [
    'treasury_withdrawals',
    // BIG is spliced into the JSON text: a JS number cannot hold it.
    { tag: 'TreasuryWithdrawals', contents: [[[{ network: 'Mainnet', credential: { scriptHash: hash28(0x41) } }, 'BIG']], hash28(0xfa)] },
    {},
  ],
  [
    'new_constitution',
    { tag: 'NewConstitution', contents: [null, { anchor: { url: 'https://const.example', dataHash: hash32(0x51).toUpperCase() }, script: hash28(0xfa) }] },
    { enacted_epoch: 565, ratified_epoch: 564 },
  ],
  ['info_action', { tag: 'InfoAction' }, {}],
  ['hard_fork_initiation', { tag: 'HardForkInitiation', contents: [null, { major: 11, minor: 0 }] }, { expired_epoch: 590, dropped_epoch: 591 }],
  ['info_action', { tag: 'InfoAction' }, { dropped_epoch: 585 }],
];

const DREPS = [
  drepRow(1, { amount: '1000' }),
  drepRow(2, { amount: '2000' }),
  drepRow(3, { amount: '4000', expired: true }),
  drepRow(4, { amount: '8000' }),
  drepRow(5, { amount: '16000', retired: true }),
  ...PREDEFINED, // abstain 7000, no confidence 500
];

const POOLS = [
  { pool_id: poolId(1), active_stake: '100' },
  { pool_id: poolId(2), active_stake: '200' },
  { pool_id: poolId(3), active_stake: '300' },
];

const COMMITTEE = committeeRow([
  memberRow(1),
  memberRow(2, { cc_cold_has_script: true, cc_cold_id: coldId(2, true), cc_hot_has_script: true, cc_hot_id: hotId(102, true) }),
  memberRow(3, { status: 'resigned', cc_hot_hex: null, cc_hot_id: null, cc_hot_has_script: null }),
  memberRow(4, { expiration_epoch: 599 }),
  memberRow(5, { status: 'not_authorized', cc_hot_hex: null, cc_hot_id: null, cc_hot_has_script: null }),
]);

/** Votes on P4, the live TreasuryWithdrawals. */
const VOTES_P4 = [
  { tx_hash: hash32(0xd0), cert_index: 0, voter_role: 'drep', voter: drepId(1), vote: 'no', counted: false },
  { tx_hash: hash32(0xd1), cert_index: 0, voter_role: 'drep', voter: drepId(1), vote: 'yes', counted: true },
  { tx_hash: hash32(0xd1), cert_index: 1, voter_role: 'drep', voter: drepId(2), vote: 'abstain', counted: true },
  { tx_hash: hash32(0xd2), cert_index: 0, voter_role: 'drep', voter: drepId(3), vote: 'yes', counted: true }, // expired: no weight
  { tx_hash: hash32(0xd3), cert_index: 0, voter_role: 'spo', voter: poolId(2), vote: 'no', counted: true },
  // The committee: CIP-129 (hosted) and bare hex (blockfrost-ryo) voters.
  { tx_hash: hash32(0xd4), cert_index: 0, voter_role: 'constitutional_committee', voter: hotId(101), vote: 'yes', counted: true },
  { tx_hash: hash32(0xd5), cert_index: 0, voter_role: 'constitutional_committee', voter: hash28(102), vote: 'no', counted: true },
  { tx_hash: hash32(0xd6), cert_index: 0, voter_role: 'constitutional_committee', voter: hotId(104), vote: 'yes', counted: true }, // expired member
];

function world(overrides = {}) {
  const routes = {
    '/epochs/latest': EPOCH,
    '/epochs/latest/parameters': PARAMS,
    '/governance/committee': COMMITTEE,
    '/governance/dreps': paged(DREPS),
    '/pools/extended': paged(POOLS),
    '/governance/proposals': paged(SPECS.map(([type], i) => ({ id: govActionId(P(i), 0), tx_hash: P(i), cert_index: 0, governance_type: type }))),
    [`/governance/proposals/${P(4)}/0/votes`]: paged(VOTES_P4),
    [`/governance/proposals/${P(6)}/0/votes`]: paged([]),
    [`/txs/${hash32(0xd1)}/cbor`]: txCbor({
      votes: [
        { kind: VOTER.drepKey, hash: hash28(1), actionTx: P(4), actionIndex: 0, vote: VOTE.yes, anchor: { url: 'https://why.example/1', hash: hash32(0x77) } },
        { kind: VOTER.drepKey, hash: hash28(2), actionTx: P(4), actionIndex: 0, vote: VOTE.abstain },
      ],
    }),
    [`/txs/${hash32(0xd5)}/cbor`]: txCbor({
      votes: [{ kind: VOTER.ccHotScript, hash: hash28(102), actionTx: P(4), actionIndex: 0, vote: VOTE.no }],
    }),
    [`/txs/${hash32(0xd2)}/cbor`]: txCbor({ votes: [{ kind: VOTER.drepKey, hash: hash28(3), actionTx: P(4), actionIndex: 0, vote: VOTE.yes }] }),
    [`/txs/${hash32(0xd3)}/cbor`]: txCbor({ votes: [{ kind: VOTER.spo, hash: hash28(2), actionTx: P(4), actionIndex: 0, vote: VOTE.no }] }),
    [`/txs/${hash32(0xd4)}/cbor`]: txCbor({ votes: [{ kind: VOTER.ccHotKey, hash: hash28(101), actionTx: P(4), actionIndex: 0, vote: VOTE.yes }] }),
    [`/txs/${hash32(0xd6)}/cbor`]: txCbor({ votes: [{ kind: VOTER.ccHotKey, hash: hash28(104), actionTx: P(4), actionIndex: 0, vote: VOTE.yes }] }),
    ...overrides,
  };
  SPECS.forEach(([type, description, epochs], i) => {
    const record = {
      id: govActionId(P(i), 0),
      tx_hash: P(i),
      cert_index: 0,
      governance_type: type,
      governance_description: description,
      deposit: '100000000000',
      return_address: 'stake1u9ret',
      ratified_epoch: null,
      enacted_epoch: null,
      dropped_epoch: null,
      expired_epoch: null,
      expiration: 550 + i + 7,
      ...epochs,
    };
    routes[`/governance/proposals/${P(i)}/${0}`] ??= { status: 200, body: JSON.stringify(record).replace('"BIG"', '12345678901234567') };
    routes[`/txs/${P(i)}`] ??= txRow(P(i), 550 + i);
    routes[`/txs/${P(i)}/cbor`] ??= txCbor({ proposals: [{ url: `https://p.example/${i}`, hash: hash32(0x90 + i) }] });
  });
  return routes;
}

const all = async (chainData, q = {}) => (await chainData.governance.proposals.list({ page: 1, size: 100, ...q })).data;

test('bodies: all seven types typed from the ledger description, predecessors from it too', async () => {
  const { chainData } = provider(world());
  const { elements } = await all(chainData, { sort: 'oldest' });
  const [pc0, pc1, uc, nc, tw, con, info, hf] = elements;
  assert.deepEqual(pc0.body, { type: 'ParameterChange', changes: { minFeeA: 45 }, guardrailsScriptHash: null });
  assert.deepEqual(pc1.body.changes, { maxTxSize: 20000, govActionDeposit: '5' });
  assert.equal(pc1.body.guardrailsScriptHash, hash28(0xfa));
  assert.deepEqual(pc1.previousAction, { id: govActionId(P(0), 0), txHash: P(0), index: 0 });
  assert.equal(pc0.previousAction, null);
  assert.deepEqual(uc.body, {
    type: 'UpdateCommittee',
    added: [{ coldCredential: coldId(0x32, true), termExpiryEpoch: 700 }],
    removed: [{ coldCredential: coldId(0x31) }],
    quorum: { numerator: 2, denominator: 3 },
  });
  assert.deepEqual(nc.body, { type: 'NoConfidence' });
  assert.equal(nc.previousAction.txHash, P(2));
  assert.equal(tw.body.totalAmount, '12345678901234567', 'lovelace beyond 2^53 kept exact');
  assert.equal(tw.body.withdrawals[0].amount, '12345678901234567');
  assert.match(tw.body.withdrawals[0].stakeAddress, /^stake1/);
  assert.deepEqual(con.body.anchor, { url: 'https://const.example', dataHash: hash32(0x51) });
  assert.deepEqual(info.body, { type: 'InfoAction' });
  assert.deepEqual(hf.body.protocolVersion, { major: 11, minor: 0 });
});

test('lifecycle: enacted > ratified > expired > dropped > live; expired is not dropped', async () => {
  const { chainData } = provider(world());
  const { elements } = await all(chainData, { sort: 'oldest' });
  assert.deepEqual(elements.map((p) => p.lifecycle.status), ['enacted', 'enacted', 'enacted', 'enacted', 'live', 'enacted', 'live', 'expired', 'dropped']);
  const hf = elements[7];
  assert.deepEqual(hf.lifecycle.expiredAt, { epoch: 590 });
  assert.equal(hf.lifecycle.droppedAt, null);
  assert.deepEqual(elements[8].lifecycle.droppedAt, { epoch: 585 });
  const tw = elements[4];
  assert.equal(tw.lifecycle.submitted.epoch, 554, 'dated from the submitting transaction');
  assert.deepEqual(tw.lifecycle.expires, { epoch: 561 }, 'Blockfrost expiration, as db-sync');
  assert.equal(tw.lifecycle.submittedTx.txHash, P(4));
  assert.deepEqual(tw.anchor, { url: 'https://p.example/4', dataHash: hash32(0x94) }, 'anchor from the proposal procedure');
});

test('aggregates: live only; concluded actions carry none', async () => {
  const { chainData } = provider(world());
  const { elements } = await all(chainData, { sort: 'oldest' });
  for (const p of elements) assert.equal(p.voteAggregates !== undefined, p.lifecycle.status === 'live', p.id);
});

test('aggregates: DRep, SPO and committee figures, and yes + no + abstain + notVoted = totalEligible', async () => {
  const { chainData } = provider(world());
  const { data: tw } = await chainData.governance.proposals.get(govActionId(P(4), 0));
  const [drep, spo, cc] = ['drep', 'spo', 'cc'].map((r) => tw.voteAggregates.find((a) => a.role === r));
  // Active DReps: 1 (1000, yes after re-vote), 2 (2000, abstain), 4 (8000, silent); 3 expired, 5 retired.
  // always-no-confidence (500) is a No on everything but NoConfidence.
  assert.deepEqual(drep, {
    role: 'drep',
    representation: 'stake',
    yes: '1000',
    no: '500',
    abstain: '2000',
    notVoted: '8000',
    totalEligible: '11500',
    threshold: { numerator: 67, denominator: 100 },
  });
  assert.equal(spo, undefined, 'SPOs do not vote on treasury withdrawals');
  // Eligible: 1 and 2 (authorised, unexpired). 3 resigned, 4 expired at 599, 5 unauthorised.
  assert.deepEqual(cc, {
    role: 'cc',
    representation: 'count',
    yes: '1',
    no: '1',
    abstain: '0',
    notVoted: '0',
    totalEligible: '2',
    threshold: { numerator: 2, denominator: 3 },
  });
  for (const a of tw.voteAggregates) {
    assert.equal(BigInt(a.yes) + BigInt(a.no) + BigInt(a.abstain) + BigInt(a.notVoted), BigInt(a.totalEligible));
  }
});

test('aggregates: an InfoAction has SPO stake over the epoch total and the unreachable 1/1', async () => {
  const { chainData } = provider(world({
    [`/governance/proposals/${P(6)}/0/votes`]: paged([
      { tx_hash: hash32(0xe1), cert_index: 0, voter_role: 'spo', voter: poolId(3), vote: 'yes', counted: true },
      { tx_hash: hash32(0xe1), cert_index: 1, voter_role: 'spo', voter: poolId(9), vote: 'yes', counted: true }, // not in the distribution
    ]),
  }));
  const { data } = await chainData.governance.proposals.get(govActionId(P(6), 0));
  const spo = data.voteAggregates.find((a) => a.role === 'spo');
  assert.equal(spo.yes, '300');
  assert.equal(spo.totalEligible, EPOCH.active_stake);
  assert.equal(BigInt(spo.notVoted), BigInt(EPOCH.active_stake) - 300n, 'silent pools are notVoted (documented deviation)');
  assert.deepEqual(spo.threshold, { numerator: 1, denominator: 1 });
});

test('aggregates: on a live NoConfidence always-no-confidence is a Yes; a dissolved committee has no cc row', async () => {
  // The NoConfidence, made live, under a committee it has dissolved.
  const live = provider(world({
    '/governance/committee': { ...COMMITTEE, is_dissolved: true },
    [`/governance/proposals/${P(3)}/0`]: {
      tx_hash: P(3), cert_index: 0, governance_type: 'no_confidence', governance_description: { tag: 'NoConfidence', contents: null },
      deposit: '1', return_address: 'stake1u9ret', ratified_epoch: null, enacted_epoch: null, dropped_epoch: null, expired_epoch: null, expiration: 610,
    },
    [`/governance/proposals/${P(3)}/0/votes`]: paged([]),
  })).chainData;
  const { data } = await live.governance.proposals.get(govActionId(P(3), 0));
  const drep = data.voteAggregates.find((a) => a.role === 'drep');
  assert.equal(drep.yes, '500');
  assert.equal(drep.no, '0');
  assert.equal(data.voteAggregates.some((a) => a.role === 'cc'), false);
  assert.deepEqual(data.voteAggregates.find((a) => a.role === 'spo').threshold, { numerator: 51, denominator: 100 });
});

test('listVotes: counted rows only, newest first, rationale anchors from CBOR, committee hex resolved', async () => {
  const { chainData } = provider(world());
  const { data } = await chainData.governance.proposals.listVotes(govActionId(P(4), 0), { page: 1, size: 3 });
  assert.equal(data.total, 7, 'the superseded (counted: false) vote is not a vote');
  assert.equal(data.elements.length, 3);
  const page3 = (await chainData.governance.proposals.listVotes(govActionId(P(4), 0), { page: 3, size: 3 })).data;
  assert.equal(page3.elements.length, 1);
  const allVotes = (await chainData.governance.proposals.listVotes(govActionId(P(4), 0), { page: 1, size: 10 })).data.elements;
  const hexVoter = allVotes.find((v) => v.txRef.txHash === hash32(0xd5));
  assert.deepEqual(hexVoter.voter, { role: 'cc', hot: hotId(102, true), cold: coldId(2, true), isScriptBased: true });
  const yes = allVotes.find((v) => v.txRef.txHash === hash32(0xd1) && v.txRef.index === 0);
  assert.deepEqual(yes.voter, { role: 'drep', id: drepId(1), isScriptBased: false });
  assert.deepEqual(yes.anchor, { url: 'https://why.example/1', dataHash: hash32(0x77) });
  const abstain = allVotes.find((v) => v.txRef.txHash === hash32(0xd1) && v.txRef.index === 1);
  assert.equal(abstain.anchor, null, 'no rationale is a known null, read from the transaction');
});

test('get with voterId: DRep, pool, committee hot and cold; not voted is null', async () => {
  const { chainData } = provider(world());
  const id = govActionId(P(4), 0);
  assert.equal((await chainData.governance.proposals.get(id, { voterId: drepId(1) })).data.myVote.choice, 'yes');
  assert.equal((await chainData.governance.proposals.get(id, { voterId: drepId(4) })).data.myVote, null);
  assert.equal((await chainData.governance.proposals.get(id, { voterId: coldId(2, true) })).data.myVote.choice, 'no');
  assert.equal((await chainData.governance.proposals.get(id, { voterId: hotId(102, true) })).data.myVote.choice, 'no');
  await rejectsWith(chainData.governance.proposals.get(id, { voterId: 'alice' }), 'INVALID_INPUT');
});

test('get with a cold id: a vote under a hot key no current member holds is refused, not reported as "not voted"', async () => {
  const { chainData } = provider(world({
    [`/governance/proposals/${P(4)}/0/votes`]: paged([
      { tx_hash: hash32(0xd7), cert_index: 0, voter_role: 'constitutional_committee', voter: hotId(177), vote: 'yes', counted: true },
    ]),
  }));
  await rejectsWith(chainData.governance.proposals.get(govActionId(P(4), 0), { voterId: coldId(1) }), 'CAPABILITY_UNSUPPORTED');
});

test('getEnacted: by lineage, walked by predecessor; UpdateCommittee and NoConfidence share one', async () => {
  const { chainData, fetch } = provider(world());
  assert.equal((await chainData.governance.proposals.getEnacted('pparamUpdate')).data.txHash, P(1));
  assert.equal((await chainData.governance.proposals.getEnacted('committee')).data.txHash, P(3), 'the NoConfidence after the UpdateCommittee');
  assert.equal((await chainData.governance.proposals.getEnacted('constitution')).data.txHash, P(5));
  assert.equal((await chainData.governance.proposals.getEnacted('hardFork')).data, null, 'expired, never enacted');
  await rejectsWith(chainData.governance.proposals.getEnacted('treasury'), 'INVALID_INPUT');
  const read = fetch.calls.filter((c) => /^\/governance\/proposals\/[0-9a-f]{64}\/0$/.test(c.path)).map((c) => c.path.slice(22, 24));
  assert.ok(!read.includes(P(4).slice(0, 2)), 'a lineage reads only its own types');
});

test('list: filters and sorts over the whole set, exact totals, refusals', async () => {
  const { chainData } = provider(world());
  const live = await all(chainData, { status: ['live'] });
  assert.equal(live.total, 2);
  assert.deepEqual(live.elements.map((p) => p.txHash), [P(6), P(4)], 'newest first by default');
  const byType = await all(chainData, { type: ['InfoAction'], sort: 'oldest' });
  assert.deepEqual(byType.elements.map((p) => p.txHash), [P(6), P(8)]);
  const soon = await all(chainData, { sort: 'soonestToExpire' });
  assert.deepEqual(soon.elements.map((p) => p.txHash), SPECS.map((_, i) => P(i)), 'expiration rises with chain order here');
  const page = (await chainData.governance.proposals.list({ page: 2, size: 4, sort: 'oldest' })).data;
  assert.equal(page.total, 9);
  assert.deepEqual(page.elements.map((p) => p.txHash), [P(4), P(5), P(6), P(7)]);
  const search = await all(chainData, { search: govActionId(P(5), 0) });
  assert.deepEqual(search.elements.map((p) => p.txHash), [P(5)]);
  assert.deepEqual((await all(chainData, { search: 'hello' })), { elements: [], total: 0 });
  await rejectsWith(chainData.governance.proposals.list({ page: 1, size: 5, sort: 'mostYesVotes' }), 'CAPABILITY_UNSUPPORTED');
  await rejectsWith(chainData.governance.proposals.list({ page: 1, size: 5, voterId: drepId(1) }), 'CAPABILITY_UNSUPPORTED');
  await rejectsWith(chainData.governance.proposals.list({ page: 1, size: 5, status: ['pending'] }), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.proposals.get(`${P(4)}#0`), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.proposals.get(govActionId(hash32(0x01), 0)), 'NOT_FOUND');
});

test('list: newest/oldest read records for the page only', async () => {
  const { chainData, fetch } = provider(world());
  await chainData.governance.proposals.list({ page: 1, size: 2, sort: 'oldest' });
  const records = fetch.calls.filter((c) => /^\/governance\/proposals\/[0-9a-f]{64}\/0$/.test(c.path));
  assert.equal(records.length, 2);
});

test('listActivity: the lifecycle as a feed', async () => {
  const { chainData } = provider(world());
  const { data } = await chainData.governance.proposals.listActivity(govActionId(P(1), 0), { page: 1, size: 10 });
  assert.deepEqual(data.elements.map((e) => e.status), ['live', 'ratified', 'enacted']);
  const expired = (await chainData.governance.proposals.listActivity(govActionId(P(7), 0), { page: 1, size: 10 })).data;
  assert.deepEqual(expired.elements.map((e) => e.status), ['live', 'expired']);
});

/* -- committee, constitution --------------------------------------------------- */

test('committee: ledger state from /governance/committee; hot key only while authorised', async () => {
  const { chainData } = provider(world());
  const { data } = await chainData.governance.committee.getCommittee();
  assert.deepEqual(data.quorum, { numerator: 2, denominator: 3 });
  assert.deepEqual(data.enactedBy, { id: govActionId(hash32(0x70), 0), txHash: hash32(0x70), index: 0 });
  assert.equal(data.isDissolved, false);
  const byCold = new Map(data.members.map((m) => [m.coldCredential, m]));
  assert.equal(byCold.get(coldId(1)).hotCredential, hotId(101));
  assert.equal(byCold.get(coldId(2, true)).hotCredential, hotId(102, true));
  assert.equal(byCold.get(coldId(3)).hasResigned, true);
  assert.equal(byCold.get(coldId(3)).hotCredential, null);
  assert.equal(byCold.get(coldId(5)).hotCredential, null);
  assert.equal(byCold.get(coldId(1)).termStartEpoch, null, 'not served, so null rather than guessed');
  assert.equal((await chainData.governance.committee.getMember(coldId(4))).data.termExpiryEpoch, 599);
  await rejectsWith(chainData.governance.committee.getMember(coldId(9)), 'NOT_FOUND');
  await rejectsWith(chainData.governance.committee.getMember(hotId(101)), 'INVALID_INPUT');
});

test('constitution: derived from the enacted head of the constitution lineage', async () => {
  const { chainData } = provider(world());
  const { data } = await chainData.governance.committee.getConstitution();
  assert.deepEqual(data, {
    anchor: { url: 'https://const.example', dataHash: hash32(0x51) },
    guardrailsScriptHash: hash28(0xfa),
    enactedBy: { id: govActionId(P(5), 0), txHash: P(5), index: 0 },
    enactedAt: { epoch: 565 },
  });
});

/* -- pools ------------------------------------------------------------------------ */

test('pools: pages straight off /pools/extended; no total until the end is seen', async () => {
  const rows = Array.from({ length: 230 }, (_, i) => ({
    pool_id: poolId(i + 1),
    hex: hash28(i + 1),
    active_stake: String(1000 + i),
    live_stake: String(2000 + i),
    declared_pledge: '5',
    metadata: i % 2 ? null : { url: `https://pool.example/${i}`, hash: hash32(i), ticker: 'X' },
  }));
  const { chainData, fetch } = provider({ '/pools/extended': paged(rows) });
  const p2 = (await chainData.governance.pools.list({ page: 2, size: 150 })).data;
  assert.equal(p2.elements.length, 80);
  assert.equal(p2.total, 230, 'the short page is the end, so the total is known');
  assert.equal(p2.elements[0].poolId, poolId(151));
  const p1 = (await chainData.governance.pools.list({ page: 1, size: 150 })).data;
  assert.equal(p1.elements.length, 150);
  assert.equal(p1.total, undefined, 'not counted: that needs every page');
  assert.deepEqual(p1.elements[0].votingPower, { amount: '1000', basis: 'active' });
  assert.deepEqual(p1.elements[0].anchor, { url: 'https://pool.example/0', dataHash: hash32(0) });
  assert.equal(p1.elements[1].anchor, null);
  const pages = fetch.calls.map((c) => new URL(c.url).searchParams.get('page'));
  assert.deepEqual(pages, ['2', '3', '1', '2']);
  assert.equal(chainData.governance.pools.listVotes, undefined, 'omitted: pool vote rows do not name the action');
});

test('pools: get and exact-id search', async () => {
  const id = poolId(7);
  const { chainData } = provider({
    [`/pools/${id}`]: { pool_id: id, hex: hash28(7), active_stake: '10', live_stake: '11', declared_pledge: '3' },
    [`/pools/${id}/metadata`]: { pool_id: id, url: 'https://p.example', hash: hash32(7) },
  });
  const { data } = await chainData.governance.pools.get(id);
  assert.deepEqual(data, {
    role: 'spo',
    id,
    poolId: id,
    isScriptBased: false,
    anchor: { url: 'https://p.example', dataHash: hash32(7) },
    votingPower: { amount: '10', basis: 'active' },
    activeStake: '10',
    liveStake: '11',
    pledge: '3',
  });
  assert.equal((await chainData.governance.pools.list({ page: 1, size: 5, search: id })).data.total, 1);
  assert.equal((await chainData.governance.pools.list({ page: 1, size: 5, search: 'TICKR' })).data.total, 0);
  await rejectsWith(chainData.governance.pools.get(poolId(8)), 'NOT_FOUND');
  await rejectsWith(chainData.governance.pools.get('pool1xyz'), 'INVALID_INPUT');
});

/* -- the declaration against the structure ---------------------------------------- */

test('capabilities: only option arrays, and each declared option has its method', () => {
  const caps = capabilities();
  const { chainData } = provider({});
  assert.deepEqual(caps, {
    sorts: { dreps: ['votingPower', 'random'], proposals: ['newest', 'oldest', 'soonestToExpire'], votes: ['newest', 'oldest'] },
    filters: { dreps: ['status', 'kind'], proposals: ['type', 'status'] },
    search: ['exactId'],
    voteAggregate: ['stake', 'count'],
    optionalArguments: ['protocolParams.epoch'],
  });
  assert.equal(typeof chainData.governance.dreps.listVotes, 'function', 'votes sorts are honoured by dreps.listVotes');
  for (const [area, method] of [
    ['dreps', 'listUpdateHistory'],
    ['dreps', 'getCounts'],
    ['proposals', 'listVotes'],
    ['proposals', 'listActivity'],
  ]) {
    assert.equal(typeof chainData.governance[area][method], 'function', `${area}.${method}`);
  }
  assert.equal(typeof chainData.network.getTreasury, 'function');
  assert.equal(typeof chainData.network.getGenesisParams, 'function');
  assert.equal(typeof chainData.accounts.getPoolDelegation, 'function');
});

test('system: identity and health', async () => {
  const { chainData } = provider({
    '/health': { is_healthy: true },
    '/blocks/latest': { time: Math.floor(Date.now() / 1000) - 30, height: 5, slot: 6, epoch: 600 },
  });
  assert.deepEqual((await chainData.system.getIdentity()).data, { id: 'blockfrost', name: 'Blockfrost' });
  const { data } = await chainData.system.getHealth();
  assert.equal(data.status, 'healthy');
  assert.equal(data.tip.epoch, 600);
  const down = provider({ '/health': { status: 403, body: { message: 'Invalid project token.' } } }).chainData;
  const health = (await down.system.getHealth()).data;
  assert.equal(health.status, 'unavailable');
  assert.equal(health.message, 'Blockfrost refused the project credentials');
});
