/**
 * The DRep area over a fake Blockfrost: the directory, status and kind,
 * sorting, paging, registration dating, counts, and the vote listing.
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import {
  EPOCH,
  PARAMS,
  PREDEFINED,
  VOTE,
  VOTER,
  drepCip105,
  drepId,
  drepRow,
  hash28,
  hash32,
  paged,
  provider,
  rejectsWith,
  txCbor,
  txRow,
} from './fake.mjs';

const regTx = (n) => hash32(0x10 + n);
const updTx = (n) => hash32(0x60 + n);

/** 250 DReps across three Blockfrost pages, plus the rows that are not DReps. */
function directory() {
  const rows = [];
  for (let n = 1; n <= 250; n++) {
    rows.push(
      drepRow(n % 200 || 200, {
        drep_id: drepId(n % 200 || 200, n > 200),
        hex: `${n > 200 ? '23' : '22'}${hash28(n % 200 || 200)}`,
        has_script: n > 200,
        amount: String(n * 1000),
        retired: n % 10 === 0,
        expired: n % 10 === 5,
        metadata: n % 4 === 0 ? null : { url: `https://e.example/${n}`, hash: hash32(n % 256), json_metadata: null },
      }),
    );
  }
  // Never registered: delegated to, no certificate (found on mainnet).
  rows.splice(120, 0, drepRow(222, { last_active_epoch: null, metadata: null, amount: '0' }));
  rows.push(...PREDEFINED);
  return rows;
}

function routesFor(dir, extra = {}) {
  const routes = { '/epochs/latest': EPOCH, '/governance/dreps': paged(dir), ...extra };
  for (const row of dir) {
    if (!row.drep_id.startsWith('drep1') || row.last_active_epoch === null) continue;
    const n = parseInt(row.hex.slice(2, 4), 16) + (row.has_script ? 300 : 0);
    routes[`/governance/dreps/${row.drep_id}/updates`] ??= [
      { tx_hash: regTx(n % 200), cert_index: 0, action: 'registered', deposit: '500000000' },
      ...(n % 3 === 0 ? [{ tx_hash: updTx(n % 30), cert_index: 1, action: 'updated', deposit: null }] : []),
      ...(row.retired ? [{ tx_hash: hash32(0xee), cert_index: 0, action: 'deregistered', deposit: '-500000000' }] : []),
    ];
  }
  for (let i = 0; i < 200; i++) routes[`/txs/${regTx(i)}`] = txRow(regTx(i), 560);
  for (let i = 0; i < 30; i++) routes[`/txs/${updTx(i)}`] = txRow(updTx(i), 580);
  routes[`/txs/${hash32(0xee)}`] = txRow(hash32(0xee), 590);
  return routes;
}

test('list: the predefined targets and never-registered credentials are not DReps', async () => {
  const { chainData } = provider(routesFor(directory()));
  const { data } = await chainData.governance.dreps.list({ page: 1, size: 1000, sort: 'votingPower' });
  assert.equal(data.total, 250);
  assert.equal(data.elements.length, 250);
  assert.ok(data.elements.every((d) => /^drep1/.test(d.id) && d.id !== drepId(222) || d.votingPower.amount !== '0'));
  assert.equal(new Set(data.elements.map((d) => d.id)).size, 250);
});

test('list: status from retired/expired, kind from the anchor, voting power on the active basis', async () => {
  const { chainData } = provider(routesFor(directory()));
  const { data } = await chainData.governance.dreps.list({ page: 1, size: 1000, sort: 'votingPower' });
  const byAmount = new Map(data.elements.map((d) => [Number(d.votingPower.amount) / 1000, d]));
  assert.equal(byAmount.get(10).status, 'retired');
  assert.equal(byAmount.get(15).status, 'inactive');
  assert.equal(byAmount.get(11).status, 'active');
  assert.equal(byAmount.get(12).kind, 'anonymous');
  assert.equal(byAmount.get(12).anchor, null);
  assert.deepEqual(byAmount.get(11).anchor, { url: 'https://e.example/11', dataHash: hash32(11) });
  assert.equal(byAmount.get(11).kind, 'drep');
  assert.deepEqual(byAmount.get(11).votingPower, { amount: '11000', basis: 'active' });
  assert.equal(byAmount.get(201).isScriptBased, true);
  assert.equal(byAmount.get(201).id, drepId(1, true));
  for (const d of data.elements) assert.equal(d.kind === 'anonymous', d.anchor === null, 'kind is derived from the anchor');
});

test('list: votingPower order, pages exact across Blockfrost pages, total = filtered count', async () => {
  const { chainData } = provider(routesFor(directory()));
  const p1 = (await chainData.governance.dreps.list({ page: 1, size: 7, sort: 'votingPower', status: ['active'] })).data;
  const p2 = (await chainData.governance.dreps.list({ page: 2, size: 7, sort: 'votingPower', status: ['active'] })).data;
  assert.equal(p1.total, 200, 'active = not retired (25) and not expired (25)');
  assert.deepEqual(p1.elements.map((d) => d.votingPower.amount), ['249000', '248000', '247000', '246000', '244000', '243000', '242000']);
  assert.equal(p2.elements[0].votingPower.amount, '241000');
  const last = (await chainData.governance.dreps.list({ page: 29, size: 7, sort: 'votingPower', status: ['active'] })).data;
  assert.equal(last.elements.length, 4, 'only the last page is short');
  const anon = (await chainData.governance.dreps.list({ page: 1, size: 1, sort: 'votingPower', kind: ['anonymous'] })).data;
  assert.equal(anon.total, 62);
});

test('list: registration is dated from the certificate transactions', async () => {
  const { chainData } = provider(routesFor(directory()));
  const { data } = await chainData.governance.dreps.list({ page: 1, size: 3, sort: 'votingPower' });
  const d = data.elements.find((x) => x.registration.latestUpdate !== null) ?? data.elements[0];
  assert.equal(d.registration.latest.at.epoch, 560);
  assert.equal(d.registration.latest.deposit, '500000000');
  assert.equal(typeof d.registration.latest.at.time, 'string');
  assert.equal(d.registration.latest.txRef.index, 0);
  if (d.registration.latestUpdate) {
    assert.equal(d.registration.latestUpdate.at.epoch, 580);
    assert.equal(d.registration.latestUpdate.deposit, null);
  }
  assert.equal('retiredAt' in d.registration, false, 'not served, so omitted rather than null');
  assert.equal('expiryEpoch' in d, false);
});

test('list: random is the default, returns size rows, and refuses page 2', async () => {
  const { chainData } = provider(routesFor(directory()));
  const { data } = await chainData.governance.dreps.list({ page: 1, size: 5 });
  assert.equal(data.elements.length, 5);
  await rejectsWith(chainData.governance.dreps.list({ page: 2, size: 5 }), 'INVALID_INPUT');
});

test('list: undeclared sorts are refused, never approximated', async () => {
  const { chainData } = provider(routesFor(directory()));
  await rejectsWith(chainData.governance.dreps.list({ page: 1, size: 5, sort: 'registrationDate' }), 'CAPABILITY_UNSUPPORTED');
  await rejectsWith(chainData.governance.dreps.list({ page: 1, size: 5, sort: 'activity' }), 'CAPABILITY_UNSUPPORTED');
  await rejectsWith(chainData.governance.dreps.list({ page: 1, size: 5, sort: 'loudest' }), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.dreps.list({ page: 1, size: 5, status: ['asleep'] }), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.dreps.list({ page: 0, size: 5 }), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.dreps.list({ page: 1, size: 5000 }), 'INVALID_INPUT');
});

test('search: an exact CIP-129 id reads one DRep; CIP-105 and free text match nothing', async () => {
  const id = drepId(33);
  const routes = routesFor(directory(), {
    [`/governance/dreps/${id}`]: { ...drepRow(33, { amount: '33000' }), metadata: undefined, active: true, active_epoch: 560 },
    [`/governance/dreps/${id}/metadata`]: { drep_id: id, hex: `22${hash28(33)}`, url: 'https://e.example/33', hash: hash32(33), json_metadata: null, error: { code: 'HTTP_RESPONSE_ERROR' } },
  });
  const { chainData, fetch } = provider(routes);
  const { data } = await chainData.governance.dreps.list({ page: 1, size: 5, sort: 'votingPower', search: id });
  assert.equal(data.total, 1);
  assert.deepEqual(data.elements[0].anchor, { url: 'https://e.example/33', dataHash: hash32(33) }, 'a failed fetch keeps the anchor');
  assert.ok(!fetch.calls.some((c) => c.path === '/governance/dreps'), 'no directory read for an id search');
  assert.deepEqual((await chainData.governance.dreps.list({ page: 1, size: 5, sort: 'votingPower', search: drepCip105(33) })).data, { elements: [], total: 0 });
  assert.deepEqual((await chainData.governance.dreps.list({ page: 1, size: 5, sort: 'votingPower', search: 'Alice' })).data, { elements: [], total: 0 });
});

test('get: CIP-129 only; CIP-105 is INVALID_INPUT; unknown and never-registered are NOT_FOUND', async () => {
  const id = drepId(44);
  const ghost = drepId(45);
  const { chainData } = provider(
    routesFor([drepRow(44), ...PREDEFINED], {
      [`/governance/dreps/${id}`]: { ...drepRow(44), metadata: undefined, active: true, active_epoch: 560 },
      [`/governance/dreps/${ghost}`]: { ...drepRow(45), metadata: undefined, last_active_epoch: null, active: true, active_epoch: null },
    }),
  );
  const { data } = await chainData.governance.dreps.get(id);
  assert.equal(data.id, id);
  assert.equal(data.kind, 'anonymous', 'no /metadata → no anchor');
  await rejectsWith(chainData.governance.dreps.get(drepCip105(44)), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.dreps.get(ghost), 'NOT_FOUND');
  await rejectsWith(chainData.governance.dreps.get(drepId(46)), 'NOT_FOUND');
  await rejectsWith(chainData.governance.dreps.get('drep_always_abstain'), 'INVALID_INPUT');
});

test('getCounts: all three, plus anonymous, over registered DReps', async () => {
  const { chainData } = provider(routesFor(directory()));
  const { data } = await chainData.governance.dreps.getCounts();
  assert.deepEqual(data, { totalRegistered: 225, totalActive: 200, totalInactive: 25, anonymous: 50 });
});

test('listDelegators is absent: Blockfrost gives live balances, the contract needs active stake', () => {
  const { chainData } = provider({});
  assert.equal(chainData.governance.dreps.listDelegators, undefined);
});

/* -- vote listing ------------------------------------------------------------ */

function voteWorld({ retired = false } = {}) {
  const id = drepId(5);
  const P = (n) => hash32(0xa0 + n);
  const index = [0, 1, 2, 3, 4].map((n) => ({ id: 'x', tx_hash: P(n), cert_index: 0, governance_type: n === 0 ? 'info_action' : 'treasury_withdrawals' }));
  const record = (n, over) => ({
    tx_hash: P(n),
    cert_index: 0,
    governance_type: index[n].governance_type,
    governance_description: n === 0 ? { tag: 'InfoAction' } : { tag: 'TreasuryWithdrawals', contents: [[], null] },
    deposit: '100000000000',
    return_address: 'stake1uxxx',
    ratified_epoch: null,
    enacted_epoch: null,
    dropped_epoch: null,
    expired_epoch: null,
    expiration: 606,
    ...over,
  });
  const voteTx = hash32(0xb1);
  const routes = {
    '/epochs/latest': EPOCH,
    '/epochs/latest/parameters': PARAMS,
    '/governance/proposals': paged(index.map(({ id, ...r }) => r)),
    [`/governance/proposals/${P(0)}/0`]: record(0, { expired_epoch: 540, expiration: 540 }),
    [`/governance/proposals/${P(1)}/0`]: record(1, { ratified_epoch: 575, enacted_epoch: 576 }),
    [`/governance/proposals/${P(2)}/0`]: record(2, {}),
    [`/governance/proposals/${P(3)}/0`]: record(3, {}),
    [`/governance/proposals/${P(4)}/0`]: record(4, { dropped_epoch: 571 }),
    [`/governance/dreps/${id}`]: { ...drepRow(5), metadata: undefined, active: true, active_epoch: 550 },
    [`/governance/dreps/${id}/updates`]: [
      { tx_hash: hash32(0xc1), cert_index: 0, action: 'registered', deposit: '500000000' },
      ...(retired ? [{ tx_hash: hash32(0xc2), cert_index: 0, action: 'deregistered', deposit: '-500000000' }] : []),
    ],
    [`/txs/${hash32(0xc1)}`]: txRow(hash32(0xc1), 550),
    [`/txs/${hash32(0xc2)}`]: txRow(hash32(0xc2), 590),
    [`/txs/${P(2)}`]: txRow(P(2), 585),
    [`/txs/${P(3)}`]: txRow(P(3), 595),
    [`/txs/${P(4)}`]: txRow(P(4), 565),
    [`/governance/dreps/${id}/votes`]: paged([
      { tx_hash: hash32(0xb0), cert_index: 0, proposal_tx_hash: P(1), proposal_cert_index: 0, vote: 'no' },
      // A re-vote: the newer one wins.
      { tx_hash: voteTx, cert_index: 1, proposal_tx_hash: P(1), proposal_cert_index: 0, vote: 'yes' },
    ]),
    [`/txs/${voteTx}/cbor`]: txCbor({
      votes: [
        { kind: VOTER.drepKey, hash: hash28(9), actionTx: P(1), actionIndex: 0, vote: VOTE.no },
        { kind: VOTER.drepKey, hash: hash28(5), actionTx: P(1), actionIndex: 0, vote: VOTE.yes, anchor: { url: 'https://why.example', hash: hash32(7) } },
      ],
    }),
  };
  return { id, routes, P };
}

test('listVotes: voted and not-voted rows, votable since registration, bootstrap excluded', async () => {
  const { id, routes, P } = voteWorld();
  const { chainData } = provider(routes);
  const { data } = await chainData.governance.dreps.listVotes(id, { page: 1, size: 10, sort: 'oldest' });
  // P0: an InfoAction that expired (540) before registration (550): not votable.
  // P1: voted (yes, the re-vote). P2, P3: open, not voted. P4: dropped at 571, votable to 570.
  assert.equal(data.total, 4);
  assert.deepEqual(data.elements.map((r) => [r.action.txHash ?? null, r.voted]).map(([, v]) => v), [true, false, false, false]);
  const voted = data.elements[0];
  assert.equal(voted.choice, 'yes');
  assert.deepEqual(voted.anchor, { url: 'https://why.example', dataHash: hash32(7) }, 'rationale from the vote CBOR');
  assert.deepEqual(voted.txRef, { txHash: hash32(0xb1), index: 1 });
  assert.equal(voted.action.type, 'TreasuryWithdrawals');
  for (const r of data.elements.filter((x) => !x.voted)) {
    assert.equal('choice' in r, false);
    assert.equal('anchor' in r, false);
  }
  const notVoted = (await chainData.governance.dreps.listVotes(id, { page: 1, size: 10, voted: false })).data;
  const onlyVoted = (await chainData.governance.dreps.listVotes(id, { page: 1, size: 10, voted: true })).data;
  assert.equal(notVoted.total + onlyVoted.total, data.total);
  const newest = (await chainData.governance.dreps.listVotes(id, { page: 1, size: 10 })).data;
  assert.equal(newest.elements[0].action.id, data.elements[data.elements.length - 1].action.id, 'newest first by default');
  assert.ok(P);
});

test('listVotes: a retired DRep\'s window closes at retirement; later actions are not votable', async () => {
  const { id, routes } = voteWorld({ retired: true });
  const { chainData } = provider(routes);
  const { data } = await chainData.governance.dreps.listVotes(id, { page: 1, size: 10, sort: 'oldest' });
  // P3 was submitted at 595, after the retirement at 590.
  assert.equal(data.total, 3);
});
