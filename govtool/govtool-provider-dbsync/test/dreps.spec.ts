import { ChainDataError } from '@govtool/data-providers/chain-data';

import { DbSyncChainDataProvider } from '../src';
import { deriveKind, deriveStatus } from '../src/mappers/drep.mapper';
import { drepInfoRow, drepListRow, fakeDb } from './fake-db';

describe('governance.dreps.list', () => {
  it('binds the search term exactly as the legacy statement expects', async () => {
    const db = fakeDb('list-dreps.sql').on('list-dreps.sql', []);
    await new DbSyncChainDataProvider(db).governance.dreps.list({
      search: 'abc',
    });

    // six bare bindings then one wrapped — the statement's own order.
    expect(db.callsTo('list-dreps.sql')[0]!.params).toEqual([
      'abc',
      'abc',
      'abc',
      'abc',
      'abc',
      'abc',
      '%abc%',
    ]);
  });

  it('defaults the search term to empty, as the legacy service did', async () => {
    const db = fakeDb('list-dreps.sql').on('list-dreps.sql', []);
    await new DbSyncChainDataProvider(db).governance.dreps.list();
    expect(db.callsTo('list-dreps.sql')[0]!.params).toEqual([
      '',
      '',
      '',
      '',
      '',
      '',
      '%%',
    ]);
  });

  it('maps a directory row onto the contract, keeping lovelace as a string', async () => {
    const db = fakeDb('list-dreps.sql').on('list-dreps.sql', [drepListRow()]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.dreps.list();

    expect(data.elements).toHaveLength(1);
    const drep = data.elements[0]!;
    expect(drep.hash).toBe('a'.repeat(56));
    expect(drep.cip105Id).toBe('drep1exampleview');
    expect(drep.id).toMatch(/^drep1/);
    expect(drep.kind).toBe('drep');
    expect(drep.registration.status).toBe('active');
    expect(drep.registration.deposit).toBe('500000000');
    expect(drep.registration.registeredAt).toEqual({
      time: '2026-01-01T00:00:00.000Z',
    });
    expect(drep.votingPower).toEqual({
      amount: '12500000000',
      basis: 'active',
    });
    expect(drep.activity).toEqual({ votesCast: 7 });
    expect(drep.metadata?.status).toBe('valid');
    expect(drep.metadata?.body?.givenName).toBe('Example DRep');
    expect(drep.metadata?.anchor).toEqual({
      url: 'https://example.com/drep.jsonld',
      dataHash: 'b'.repeat(64),
    });
    // absent off-chain fields stay absent rather than becoming null
    expect(drep.metadata?.body).not.toHaveProperty('motivations');
  });

  it('reports no metadata when the DRep has no anchor', async () => {
    const db = fakeDb('list-dreps.sql').on('list-dreps.sql', [
      drepListRow({ url: null, metadata_hash: null, given_name: null }),
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.dreps.list();
    expect(data.elements[0]!.metadata).toBeNull();
  });

  it('marks metadata unavailable when the anchor is known but nothing was parsed', async () => {
    const db = fakeDb('list-dreps.sql').on('list-dreps.sql', [
      drepListRow({
        given_name: null,
        objectives: null,
        payment_address: null,
        fetch_error: 'timeout',
      }),
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.dreps.list();
    expect(data.elements[0]!.metadata).toMatchObject({
      status: 'unavailable',
      failureMessage: 'timeout',
    });
  });

  it('keeps parsed data authoritative even when a later fetch failed', async () => {
    const db = fakeDb('list-dreps.sql').on('list-dreps.sql', [
      drepListRow({ fetch_error: 'timeout' }),
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.dreps.list();
    // db-sync only stores off-chain data whose hash matched the anchor, so
    // having it means the document was valid; the error rides alongside.
    expect(data.elements[0]!.metadata).toMatchObject({
      status: 'valid',
      failureMessage: 'timeout',
    });
  });

  it('reports metadata pending when nothing has been fetched or failed yet', async () => {
    const db = fakeDb('list-dreps.sql').on('list-dreps.sql', [
      drepListRow({
        given_name: null,
        objectives: null,
        payment_address: null,
      }),
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.dreps.list();
    expect(data.elements[0]!.metadata?.status).toBe('pending');
  });

  it('filters by status and kind, and reports the pre-paging total', async () => {
    const db = fakeDb('list-dreps.sql').on('list-dreps.sql', [
      drepListRow({ drep_hash: 'a'.repeat(56), active: true }),
      drepListRow({ drep_hash: 'b'.repeat(56), active: false }),
      drepListRow({
        drep_hash: 'c'.repeat(56),
        deposit: '-1',
        latest_deposit: '-1',
      }),
    ]);
    const api = new DbSyncChainDataProvider(db).governance.dreps;

    const active = await api.list({ status: ['active'] });
    expect(active.data.total).toBe(1);

    const retired = await api.list({ status: ['retired'] });
    expect(retired.data.total).toBe(1);
    expect(retired.data.elements[0]!.registration.status).toBe('retired');

    const directVoters = await api.list({ kind: ['directVoter'] });
    expect(
      directVoters.data.elements.every((d) => d.kind === 'directVoter'),
    ).toBe(true);
  });

  it('pages with an opaque cursor, and returns everything when no limit is given', async () => {
    const rows = Array.from({ length: 5 }, (_, i) =>
      drepListRow({ drep_hash: String(i).repeat(56) }),
    );
    const db = fakeDb('list-dreps.sql').on('list-dreps.sql', rows);
    const api = new DbSyncChainDataProvider(db).governance.dreps;

    const all = await api.list();
    expect(all.data.elements).toHaveLength(5);
    expect(all.data.nextCursor).toBeNull();

    const first = await api.list({ limit: 2 });
    expect(first.data.elements).toHaveLength(2);
    expect(first.data.total).toBe(5);
    expect(first.data.nextCursor).not.toBeNull();

    const second = await api.list({ limit: 2, cursor: first.data.nextCursor! });
    expect(second.data.elements[0]!.hash).toBe(rows[2]!.drep_hash);

    const last = await api.list({ limit: 2, offset: 4 });
    expect(last.data.elements).toHaveLength(1);
    expect(last.data.nextCursor).toBeNull();
  });

  it('rejects a cursor it did not issue', async () => {
    const db = fakeDb('list-dreps.sql').on('list-dreps.sql', []);
    await expect(
      new DbSyncChainDataProvider(db).governance.dreps.list({ cursor: 'nope' }),
    ).rejects.toMatchObject({ code: 'INVALID_INPUT' });
  });

  it('orders `random` identically for the same seed and differently across seeds', async () => {
    const rows = Array.from({ length: 6 }, (_, i) =>
      drepListRow({ drep_hash: String(i).repeat(56) }),
    );
    const db = fakeDb('list-dreps.sql').on('list-dreps.sql', rows);
    const api = new DbSyncChainDataProvider(db).governance.dreps;

    const order = async (seed: string) =>
      (await api.list({ sort: 'random', seed })).data.elements.map(
        (d) => d.hash,
      );

    expect(await order('seed-a')).toEqual(await order('seed-a'));
    expect(await order('seed-a')).not.toEqual(await order('seed-b'));
  });

  it('sorts votingPower descending, putting DReps with no power last', async () => {
    const db = fakeDb('list-dreps.sql').on('list-dreps.sql', [
      drepListRow({ drep_hash: 'a'.repeat(56), amount: '5' }),
      drepListRow({ drep_hash: 'b'.repeat(56), amount: null }),
      drepListRow({ drep_hash: 'c'.repeat(56), amount: '9' }),
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.dreps.list({
      sort: 'votingPower',
    });
    expect(data.elements.map((d) => d.votingPower?.amount ?? null)).toEqual([
      '9',
      '5',
      null,
    ]);
  });
});

describe('governance.dreps.get', () => {
  it('accepts a raw hash, a CIP-105 id and a CIP-129 id for the same credential', async () => {
    const hash = 'a'.repeat(56);
    const db = fakeDb('get-drep-info.sql').on('get-drep-info.sql', [
      drepInfoRow(),
    ]);
    const api = new DbSyncChainDataProvider(db).governance.dreps;

    const fromHash = await api.get(hash);
    const cip129 = fromHash.data.id;
    const fromCip129 = await api.get(cip129);

    expect(fromCip129.data.hash).toBe(hash);
    // every call bound the same hex hash
    for (const call of db.callsTo('get-drep-info.sql')) {
      expect(call.params).toEqual([hash]);
    }
  });

  it('splits the two registration kinds the legacy booleans encoded', async () => {
    const db = fakeDb('get-drep-info.sql').on('get-drep-info.sql', [
      drepInfoRow({
        is_registered_as_drep: false,
        was_registered_as_drep: true,
        is_registered_as_sole_voter: true,
        was_registered_as_sole_voter: true,
        sole_voter_register_tx_hash: 'f'.repeat(64),
        drep_retire_tx_hash: 'e'.repeat(64),
      }),
    ]);
    const { data } = await new DbSyncChainDataProvider(db).governance.dreps.get(
      'a'.repeat(56),
    );

    expect(data.kind).toBe('directVoter');
    expect(data.registrationByKind).toEqual({
      drep: {
        isRegistered: false,
        wasRegistered: true,
        registrationTx: { txHash: 'c'.repeat(64) },
        retirementTx: { txHash: 'e'.repeat(64) },
      },
      directVoter: {
        isRegistered: true,
        wasRegistered: true,
        registrationTx: { txHash: 'f'.repeat(64) },
        retirementTx: null,
      },
    });
    // the summary reflects the current kind
    expect(data.registration.registrationTx).toEqual({
      txHash: 'f'.repeat(64),
    });
  });

  it('reports NOT_FOUND for a credential that was never registered', async () => {
    const db = fakeDb('get-drep-info.sql').on('get-drep-info.sql', [
      drepInfoRow({
        is_registered_as_drep: null,
        was_registered_as_drep: null,
        is_registered_as_sole_voter: null,
        was_registered_as_sole_voter: null,
      }),
    ]);
    const error = await new DbSyncChainDataProvider(db).governance.dreps
      .get('a'.repeat(56))
      .catch((e: unknown) => e);

    expect(ChainDataError.is(error)).toBe(true);
    expect(error).toMatchObject({ code: 'NOT_FOUND', retryable: false });
  });

  it('rejects a malformed id before touching the database', async () => {
    const db = fakeDb('get-drep-info.sql');
    await expect(
      new DbSyncChainDataProvider(db).governance.dreps.get('not-an-id'),
    ).rejects.toMatchObject({ code: 'INVALID_INPUT' });
    expect(db.calls).toHaveLength(0);
  });

  it('wraps a database failure as PROVIDER_UNAVAILABLE without leaking it', async () => {
    const db = fakeDb('get-drep-info.sql').failOn(
      'get-drep-info.sql',
      new Error('password authentication failed for user "postgres"'),
    );
    const error = (await new DbSyncChainDataProvider(db).governance.dreps
      .get('a'.repeat(56))
      .then(() => null)
      .catch((e: unknown) => e)) as ChainDataError;

    expect(error).toMatchObject({
      code: 'PROVIDER_UNAVAILABLE',
      message: 'db-sync query failed',
      retryable: true,
    });
    expect(JSON.stringify(error.toJSON())).not.toContain('password');
  });
});

describe('governance.dreps voting power', () => {
  it('returns the latest distribution amount, or nothing when there is no row', async () => {
    const db = fakeDb('get-voting-power.sql').on('get-voting-power.sql', [
      { amount: '123456789' },
    ]);
    const api = new DbSyncChainDataProvider(db).governance.dreps;
    await expect(api.getVotingPower('a'.repeat(56))).resolves.toMatchObject({
      data: [{ amount: '123456789', basis: 'active' }],
    });

    const empty = fakeDb('get-voting-power.sql').on('get-voting-power.sql', []);
    await expect(
      new DbSyncChainDataProvider(empty).governance.dreps.getVotingPower(
        'a'.repeat(56),
      ),
    ).resolves.toMatchObject({ data: [] });
  });

  it('reads the whole directory in one statement when given no ids', async () => {
    const db = fakeDb('get-dreps-voting-power-list.sql').on(
      'get-dreps-voting-power-list.sql',
      [
        {
          view: 'drep1abc',
          hash_raw: 'a'.repeat(56),
          voting_power: '10',
          given_name: 'A',
        },
        {
          view: 'drep_script1xyz',
          hash_raw: 'b'.repeat(56),
          voting_power: '0',
          given_name: null,
        },
      ],
    );
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.dreps.getVotingPowers();

    expect(data).toHaveLength(2);
    expect(data[0]).toMatchObject({
      votingPower: { amount: '10', basis: 'active' },
      givenName: 'A',
    });
    const first = data[0]!.subject;
    const second = data[1]!.subject;
    expect(first.kind).toBe('drep');
    // has_script is not selected; db-sync encodes it in the view's prefix
    if (first.kind === 'drep') expect(first.drep.isScriptBased).toBe(false);
    if (second.kind === 'drep') expect(second.drep.isScriptBased).toBe(true);
  });

  it('reports the predefined options, which have no credential hash', async () => {
    // Regression: `drep_hash.raw` is NULL for the predefined DReps, so this
    // statement returns a row with `hash_raw: null`. It holds real voting
    // power, so it must be reported rather than dropped or hashed.
    const db = fakeDb('get-dreps-voting-power-list.sql').on(
      'get-dreps-voting-power-list.sql',
      [
        {
          view: 'drep_always_no_confidence',
          hash_raw: null,
          voting_power: '3707653134137',
          given_name: null,
        },
      ],
    );
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.dreps.getVotingPowers();

    expect(data).toEqual([
      {
        subject: {
          kind: 'predefined',
          option: 'alwaysNoConfidence',
          view: 'drep_always_no_confidence',
        },
        votingPower: { amount: '3707653134137', basis: 'active' },
        givenName: null,
      },
    ]);
  });

  it('runs the filtered statement once per id, binding it twice', async () => {
    const db = fakeDb('get-filtered-dreps-voting-power.sql').on(
      'get-filtered-dreps-voting-power.sql',
      [],
    );
    await new DbSyncChainDataProvider(db).governance.dreps.getVotingPowers([
      'drep1abc',
      'ff00',
    ]);

    expect(
      db.callsTo('get-filtered-dreps-voting-power.sql').map((c) => c.params),
    ).toEqual([
      ['drep1abc', 'drep1abc'],
      ['ff00', 'ff00'],
    ]);
  });

  it('declines a live basis and an epoch range it has no statement for', async () => {
    const db = fakeDb('get-voting-power.sql');
    const api = new DbSyncChainDataProvider(db).governance.dreps;
    await expect(
      api.getVotingPower('a'.repeat(56), { basis: 'live' }),
    ).rejects.toMatchObject({ code: 'CAPABILITY_UNSUPPORTED' });
    await expect(
      api.getVotingPower('a'.repeat(56), { fromEpoch: 1 }),
    ).rejects.toMatchObject({ code: 'CAPABILITY_UNSUPPORTED' });
  });
});

describe('legacy derivations', () => {
  it('derives status the way the legacy service did', () => {
    expect(deriveStatus(true, 500)).toBe('active');
    expect(deriveStatus(false, 500)).toBe('inactive');
    expect(deriveStatus(true, -1)).toBe('retired');
    expect(deriveStatus(false, -1)).toBe('retired');
  });

  it('derives kind through all four legacy branches', () => {
    expect(deriveKind(0, null, false)).toBe('directVoter');
    expect(deriveKind(0, 'https://x', false)).toBe('drep');
    expect(deriveKind(-1, null, false)).toBe('directVoter');
    expect(deriveKind(-1, null, true)).toBe('drep');
  });
});

describe('routes with no legacy statement', () => {
  it.each([
    [
      'listDelegators',
      (a: DbSyncChainDataProvider) => a.governance.dreps.listDelegators('x'),
    ],
    [
      'listDelegationEvents',
      (a: DbSyncChainDataProvider) =>
        a.governance.dreps.listDelegationEvents('x'),
    ],
    [
      'listHistory',
      (a: DbSyncChainDataProvider) => a.governance.dreps.listHistory('x'),
    ],
    ['pools.list', (a: DbSyncChainDataProvider) => a.governance.pools.list()],
    ['votes.list', (a: DbSyncChainDataProvider) => a.governance.votes.list()],
    [
      'committee.getCommittee',
      (a: DbSyncChainDataProvider) => a.governance.committee.getCommittee(),
    ],
    [
      'voters.resolve',
      (a: DbSyncChainDataProvider) => a.governance.voters.resolve('x'),
    ],
    [
      'network.listEpochs',
      (a: DbSyncChainDataProvider) => a.network.listEpochs(),
    ],
    [
      'network.getTreasury',
      (a: DbSyncChainDataProvider) => a.network.getTreasury(),
    ],
    [
      'accounts.listStakeEvents',
      (a: DbSyncChainDataProvider) => a.accounts.listStakeEvents('x'),
    ],
  ])(
    'declares %s unsupported rather than returning an empty result',
    async (_name, call) => {
      const provider = new DbSyncChainDataProvider(fakeDb());
      await expect(call(provider)).rejects.toMatchObject({
        code: 'CAPABILITY_UNSUPPORTED',
        retryable: false,
      });
    },
  );
});
