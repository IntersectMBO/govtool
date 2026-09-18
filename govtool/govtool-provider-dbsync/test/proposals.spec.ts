import { DbSyncChainDataProvider } from '../src';
import { toContractType, toDbSyncType } from '../src/mappers/proposal.mapper';
import { fakeDb, proposalRow } from './fake-db';

const TX = 'd'.repeat(64);

describe('governance.proposals.list', () => {
  it('binds the search term the way the legacy statement expects', async () => {
    const db = fakeDb('list-proposals.sql').on('list-proposals.sql', []);
    await new DbSyncChainDataProvider(db).governance.proposals.list({
      search: 'ignored by SQL',
    });

    // The list read always passes an empty term; the caller's search is
    // applied in memory, exactly as the legacy service did.
    expect(db.callsTo('list-proposals.sql')[0]!.params).toEqual([
      '',
      '%%',
      '%%',
      '%%',
      '%%',
      '',
    ]);
  });

  it('maps a row, keeping vote stake as strings and CC votes as counts', async () => {
    const db = fakeDb('list-proposals.sql').on('list-proposals.sql', [
      proposalRow(),
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.proposals.list();

    const action = data.elements[0]!;
    expect(action.txHash).toBe(TX);
    expect(action.index).toBe(0);
    expect(action.id).toMatch(/^gov_action1/);
    expect(action.providerId).toBe('42');
    expect(action.type).toBe('InfoAction');
    expect(action.body).toEqual({ type: 'InfoAction' });
    expect(action.lifecycle.status).toBe('live');
    expect(action.lifecycle.submitted).toEqual({
      epoch: 500,
      time: '2026-01-05T00:00:00.000Z',
    });
    expect(action.lifecycle.expires).toEqual({
      epoch: 510,
      time: '2026-03-01T00:00:00.000Z',
    });

    // DReps and SPOs are weighed by stake, the committee by head — and the
    // stake figures stay strings so they never lose precision.
    expect(action.tallies).toEqual([
      {
        role: 'drep',
        stake: { yes: '1000000', no: '2000000', abstain: '3000000' },
      },
      { role: 'spo', stake: { yes: '4000000', no: '0', abstain: '0' } },
      { role: 'cc', count: { yes: 3, no: 1, abstain: 0 } },
    ]);

    expect(action.metadata).toMatchObject({
      standard: 'CIP108',
      status: 'valid',
      body: { title: 'A title', abstract: 'An abstract' },
    });
    expect(action.metadata?.body).not.toHaveProperty('motivation');
  });

  it('renames db-sync NewCommittee to the ledger name UpdateCommittee', async () => {
    const db = fakeDb('list-proposals.sql').on('list-proposals.sql', [
      proposalRow({ type: 'NewCommittee' }),
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.proposals.list();

    expect(data.elements[0]!.type).toBe('UpdateCommittee');
    // The threshold arrives as a float, so no typed body is claimed — the
    // shaped description is handed over raw instead.
    expect(data.elements[0]!.body).toBeUndefined();
    expect(data.elements[0]!.rawBody).toEqual({ data: {} });
  });

  it('maps every unknown type to InfoAction, as the legacy service did', () => {
    expect(toContractType('SomethingNew')).toBe('InfoAction');
    expect(toContractType('NewCommittee')).toBe('UpdateCommittee');
    expect(toDbSyncType('UpdateCommittee')).toBe('NewCommittee');
    expect(toDbSyncType('InfoAction')).toBe('InfoAction');
  });

  it('builds a TreasuryWithdrawals body and totals it without float loss', async () => {
    const db = fakeDb('list-proposals.sql').on('list-proposals.sql', [
      proposalRow({
        type: 'TreasuryWithdrawals',
        description: [
          { receivingAddress: 'stake1a', amount: '9007199254740993' },
          { receivingAddress: 'stake1b', amount: '1' },
        ],
      }),
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.proposals.list();

    expect(data.elements[0]!.body).toEqual({
      type: 'TreasuryWithdrawals',
      withdrawals: [
        { stakeAddress: 'stake1a', amount: '9007199254740993' },
        { stakeAddress: 'stake1b', amount: '1' },
      ],
      // exceeds Number.MAX_SAFE_INTEGER; summed as BigInt
      totalAmount: '9007199254740994',
    });
  });

  it('builds a HardForkInitiation body from the shaped description', async () => {
    const db = fakeDb('list-proposals.sql').on('list-proposals.sql', [
      proposalRow({
        type: 'HardForkInitiation',
        description: { major: 10, minor: 1 },
      }),
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.proposals.list();
    expect(data.elements[0]!.body).toEqual({
      type: 'HardForkInitiation',
      protocolVersion: { major: 10, minor: 1 },
    });
  });

  it('builds a NewConstitution body and keeps the guardrails hash', async () => {
    const db = fakeDb('list-proposals.sql').on('list-proposals.sql', [
      proposalRow({
        type: 'NewConstitution',
        description: {
          anchor: { url: 'ipfs://c', dataHash: 'ab' },
          script: 'cafe',
        },
      }),
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.proposals.list();
    expect(data.elements[0]!.body).toEqual({
      type: 'NewConstitution',
      anchor: { url: 'ipfs://c', dataHash: 'ab' },
      guardrailsScriptHash: 'cafe',
    });
  });

  it('puts ParameterChange values in the body and leaves a malformed one untyped', async () => {
    const withParams = fakeDb('list-proposals.sql').on('list-proposals.sql', [
      proposalRow({
        type: 'ParameterChange',
        proposal_params: { drep_deposit: 500, cost_model: null },
      }),
    ]);
    const { data } = await new DbSyncChainDataProvider(
      withParams,
    ).governance.proposals.list();
    expect(data.elements[0]!.body).toEqual({
      type: 'ParameterChange',
      changes: { drep_deposit: 500, cost_model: null },
    });

    const noParams = fakeDb('list-proposals.sql').on('list-proposals.sql', [
      proposalRow({ type: 'ParameterChange', proposal_params: null }),
    ]);
    const bare = await new DbSyncChainDataProvider(
      noParams,
    ).governance.proposals.list();
    expect(bare.data.elements[0]!.body).toBeUndefined();
  });

  it('searches the same five fields the legacy service searched', async () => {
    const db = fakeDb('list-proposals.sql').on('list-proposals.sql', [
      proposalRow({ tx_hash: 'a'.repeat(64), title: 'Fund the thing' }),
      proposalRow({
        tx_hash: 'b'.repeat(64),
        title: null,
        abstract: 'unrelated',
      }),
      proposalRow({
        tx_hash: 'c'.repeat(64),
        title: null,
        abstract: null,
        motivation: 'the thing matters',
      }),
    ]);
    const api = new DbSyncChainDataProvider(db).governance.proposals;

    await expect(api.list({ search: 'the thing' })).resolves.toMatchObject({
      data: { total: 2 },
    });
    // an exact gov action id matches too
    await expect(
      api.list({ search: `${'b'.repeat(64)}#0` }),
    ).resolves.toMatchObject({ data: { total: 1 } });
  });

  it('filters by type and sorts by the legacy orderings', async () => {
    const db = fakeDb('list-proposals.sql').on('list-proposals.sql', [
      proposalRow({
        tx_hash: 'a'.repeat(64),
        type: 'InfoAction',
        time: new Date('2026-01-01T00:00:00Z'),
        expiry_date: new Date('2026-05-01T00:00:00Z'),
        yes_votes: '1',
        pool_yes_votes: '0',
        cc_yes_votes: '0',
      }),
      proposalRow({
        tx_hash: 'b'.repeat(64),
        type: 'NoConfidence',
        time: new Date('2026-02-01T00:00:00Z'),
        expiry_date: null,
        yes_votes: '100',
        pool_yes_votes: '0',
        cc_yes_votes: '0',
      }),
    ]);
    const api = new DbSyncChainDataProvider(db).governance.proposals;

    const filtered = await api.list({ type: ['NoConfidence'] });
    expect(filtered.data.elements).toHaveLength(1);

    const newest = await api.list({ sort: 'newest' });
    expect(newest.data.elements[0]!.txHash).toBe('b'.repeat(64));

    const oldest = await api.list({ sort: 'oldest' });
    expect(oldest.data.elements[0]!.txHash).toBe('a'.repeat(64));

    // a null expiry sorts last
    const expiring = await api.list({ sort: 'soonestToExpire' });
    expect(expiring.data.elements.map((e) => e.txHash)).toEqual([
      'a'.repeat(64),
      'b'.repeat(64),
    ]);

    const yes = await api.list({ sort: 'mostYesVotes' });
    expect(yes.data.elements[0]!.txHash).toBe('b'.repeat(64));
  });

  it('refuses a status filter the statement cannot honour', async () => {
    const db = fakeDb('list-proposals.sql').on('list-proposals.sql', []);
    const api = new DbSyncChainDataProvider(db).governance.proposals;

    // the statement returns live actions only
    await expect(api.list({ status: ['live'] })).resolves.toBeDefined();
    await expect(api.list({ status: ['enacted'] })).rejects.toMatchObject({
      code: 'CAPABILITY_UNSUPPORTED',
    });
  });
});

describe('governance.proposals.get', () => {
  it('binds the txHash#index form the statement matches on', async () => {
    const db = fakeDb('list-proposals.sql').on('list-proposals.sql', [
      proposalRow(),
    ]);
    await new DbSyncChainDataProvider(db).governance.proposals.get(`${TX}#0`);

    const legacyId = `${TX}#0`;
    expect(db.callsTo('list-proposals.sql')[0]!.params).toEqual([
      legacyId,
      `%${legacyId}%`,
      `%${legacyId}%`,
      `%${legacyId}%`,
      `%${legacyId}%`,
      legacyId,
    ]);
  });

  it('accepts a CIP-129 id for the same action', async () => {
    const db = fakeDb('list-proposals.sql').on('list-proposals.sql', [
      proposalRow(),
    ]);
    const api = new DbSyncChainDataProvider(db).governance.proposals;
    const { data } = await api.get(`${TX}#0`);
    const viaBech32 = await api.get(data.id);
    expect(viaBech32.data.txHash).toBe(TX);
    expect(db.callsTo('list-proposals.sql')[1]!.params[0]).toBe(`${TX}#0`);
  });

  it('reports NOT_FOUND with the legacy message when nothing matches', async () => {
    const db = fakeDb('list-proposals.sql').on('list-proposals.sql', []);
    await expect(
      new DbSyncChainDataProvider(db).governance.proposals.get(`${TX}#0`),
    ).rejects.toMatchObject({
      code: 'NOT_FOUND',
      message: `Proposal with id: ${TX}#0 not found`,
    });
  });

  it('rejects a malformed id before touching the database', async () => {
    const db = fakeDb('list-proposals.sql');
    const api = new DbSyncChainDataProvider(db).governance.proposals;
    for (const bad of ['nope', `${TX}#`, `${TX}#x`, 'zz#0']) {
      await expect(api.get(bad)).rejects.toMatchObject({
        code: 'INVALID_INPUT',
      });
    }
    expect(db.calls).toHaveLength(0);
  });
});

describe('governance.proposals.getEnacted', () => {
  it('binds the db-sync type name, not the contract name', async () => {
    const file = 'get-previous-enacted-governance-action-proposal-details.sql';
    const db = fakeDb(file).on(file, [
      {
        id: '7',
        tx_id: '99',
        index: 1,
        description: { tag: 'x' },
        hash: 'ab'.repeat(32),
      },
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.proposals.getEnacted('ParameterChange');

    expect(db.callsTo(file)[0]!.params).toEqual(['ParameterChange']);
    expect(data).toEqual({
      type: 'ParameterChange',
      action: {
        id: expect.stringMatching(/^gov_action1/),
        txHash: 'ab'.repeat(32),
        index: 1,
        providerId: '7',
      },
      submittedTx: { txHash: 'ab'.repeat(32), providerId: '99' },
      rawBody: { tag: 'x' },
    });
  });

  it('returns null when there is no enacted action of that type', async () => {
    const file = 'get-previous-enacted-governance-action-proposal-details.sql';
    const db = fakeDb(file).on(file, []);
    await expect(
      new DbSyncChainDataProvider(db).governance.proposals.getEnacted(
        'HardForkInitiation',
      ),
    ).resolves.toMatchObject({ data: null });
  });

  it('refuses a type the statement cannot answer instead of substituting one', async () => {
    const file = 'get-previous-enacted-governance-action-proposal-details.sql';
    const db = fakeDb(file).on(file, []);
    // The legacy service silently answered HardForkInitiation for any other
    // type, which returned the wrong action's body.
    await expect(
      new DbSyncChainDataProvider(db).governance.proposals.getEnacted(
        'InfoAction',
      ),
    ).rejects.toMatchObject({ code: 'CAPABILITY_UNSUPPORTED' });
    expect(db.calls).toHaveLength(0);
  });
});
