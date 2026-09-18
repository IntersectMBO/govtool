import { DbSyncChainDataProvider } from '../src';
import { fakeDb } from './fake-db';

const STAKE = 'a'.repeat(56);

describe('accounts.get', () => {
  it('maps registration state and keeps the internal id opaque', async () => {
    const db = fakeDb('get-account-info.sql').on('get-account-info.sql', [
      {
        id: '1234',
        view: 'stake_test1abc',
        is_script_based: false,
        is_registered: true,
      },
    ]);
    const { data } = await new DbSyncChainDataProvider(db).accounts.get(STAKE);

    expect(data).toEqual({
      stakeAddress: 'stake_test1abc',
      stakeKeyHash: STAKE,
      isRegistered: true,
      isScriptBased: false,
      providerId: '1234',
    });
    // the db row id is carried as an opaque providerId, never as `id`
    expect(data).not.toHaveProperty('id');
  });

  it('fails with the legacy message when the account is unknown', async () => {
    const db = fakeDb('get-account-info.sql').on('get-account-info.sql', []);
    await expect(
      new DbSyncChainDataProvider(db).accounts.get(STAKE),
    ).rejects.toMatchObject({
      code: 'INTERNAL',
      message: 'Could not query the account info.',
    });
  });

  it('serves votingPower and delegation only when asked, and refuses fields it has no statement for', async () => {
    const db = fakeDb(
      'get-account-info.sql',
      'get-stake-key-voting-power.sql',
      'get-current-delegation.sql',
    )
      .on('get-account-info.sql', [
        {
          id: '1',
          view: 'stake1abc',
          is_script_based: false,
          is_registered: true,
        },
      ])
      .on('get-stake-key-voting-power.sql', [
        { total_balance: '900000000', stake_address: STAKE },
      ])
      .on('get-current-delegation.sql', [
        {
          drep_raw: 'b'.repeat(56),
          drep_view: 'drep1xyz',
          has_script: false,
          encode: 'c'.repeat(64),
        },
      ]);
    const api = new DbSyncChainDataProvider(db).accounts;

    const bare = await api.get(STAKE);
    expect(bare.data.votingPower).toBeUndefined();
    expect(bare.data.delegation).toBeUndefined();
    expect(db.callsTo('get-stake-key-voting-power.sql')).toHaveLength(0);

    const expanded = await api.get(STAKE, {
      expand: ['votingPower', 'delegation'],
    });
    expect(expanded.data.votingPower).toEqual({
      amount: '900000000',
      basis: 'live',
    });
    expect(expanded.data.delegation?.target).toMatchObject({ kind: 'drep' });

    await expect(api.get(STAKE, { expand: ['balance'] })).rejects.toMatchObject(
      {
        code: 'CAPABILITY_UNSUPPORTED',
      },
    );
  });

  it('accepts a bech32 stake address as well as a raw hash', async () => {
    const db = fakeDb('get-account-info.sql').on('get-account-info.sql', [
      {
        id: '1',
        view: 'stake1abc',
        is_script_based: false,
        is_registered: true,
      },
    ]);
    const api = new DbSyncChainDataProvider(db).accounts;
    await api.get(STAKE);
    await expect(api.get('not-a-key')).rejects.toMatchObject({
      code: 'INVALID_INPUT',
    });
    expect(db.callsTo('get-account-info.sql')[0]!.params).toEqual([STAKE]);
  });
});

describe('accounts.getDelegation', () => {
  it('reads the unaliased tx hash column the statement actually returns', async () => {
    const db = fakeDb('get-current-delegation.sql').on(
      'get-current-delegation.sql',
      [
        {
          drep_raw: 'b'.repeat(56),
          drep_view: 'drep1xyz',
          has_script: true,
          // the statement's last column is `encode(tx.hash,'hex')`, unaliased
          encode: 'c'.repeat(64),
        },
      ],
    );
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).accounts.getDelegation(STAKE);

    expect(data).toEqual({
      target: {
        kind: 'drep',
        drep: {
          role: 'drep',
          id: expect.stringMatching(/^drep1/),
          hash: 'b'.repeat(56),
          isScriptBased: true,
          cip105Id: 'drep1xyz',
        },
      },
      txRef: { txHash: 'c'.repeat(64) },
    });
  });

  it.each([
    ['drep_always_abstain', 'alwaysAbstain'],
    ['drep_always_no_confidence', 'alwaysNoConfidence'],
  ])('recognises the predefined target %s', async (view, option) => {
    const db = fakeDb('get-current-delegation.sql').on(
      'get-current-delegation.sql',
      [{ drep_raw: null, drep_view: view, has_script: false, encode: 'ab' }],
    );
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).accounts.getDelegation(STAKE);
    expect(data?.target).toEqual({ kind: 'predefined', option });
  });

  it('returns null when the account has never delegated', async () => {
    const db = fakeDb('get-current-delegation.sql').on(
      'get-current-delegation.sql',
      [],
    );
    await expect(
      new DbSyncChainDataProvider(db).accounts.getDelegation(STAKE),
    ).resolves.toMatchObject({ data: null });
  });
});

describe('accounts.getVotingPower', () => {
  it('reports the live basis, since the statement sums current UTxO and rewards', async () => {
    const db = fakeDb('get-stake-key-voting-power.sql').on(
      'get-stake-key-voting-power.sql',
      [{ total_balance: '45000000000000000', stake_address: STAKE }],
    );
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).accounts.getVotingPower(STAKE);

    // total supply scale — must stay a string
    expect(data).toEqual({ amount: '45000000000000000', basis: 'live' });
  });

  it('returns null rather than 0 when there is no row', async () => {
    const db = fakeDb('get-stake-key-voting-power.sql').on(
      'get-stake-key-voting-power.sql',
      [],
    );
    await expect(
      new DbSyncChainDataProvider(db).accounts.getVotingPower(STAKE),
    ).resolves.toMatchObject({ data: null });
  });

  it('lets a database failure surface instead of masking it as zero', async () => {
    // The legacy service caught everything here and returned 0, which
    // reported an outage as "no voting power".
    const db = fakeDb('get-stake-key-voting-power.sql').failOn(
      'get-stake-key-voting-power.sql',
      new Error('connection terminated'),
    );
    await expect(
      new DbSyncChainDataProvider(db).accounts.getVotingPower(STAKE),
    ).rejects.toMatchObject({ code: 'PROVIDER_UNAVAILABLE' });
  });
});

describe('transactions.get', () => {
  it('binds the hash twice, as the statement expects', async () => {
    const tx = 'f'.repeat(64);
    const db = fakeDb('get-transaction-status.sql').on(
      'get-transaction-status.sql',
      [{ tx_exists: true, voting_procedures: [{ id: 1 }] }],
    );
    const { data } = await new DbSyncChainDataProvider(db).transactions.get(tx);

    expect(db.callsTo('get-transaction-status.sql')[0]!.params).toEqual([
      tx,
      tx,
    ]);
    expect(data).toEqual({
      txHash: tx,
      status: 'confirmed',
      votingProcedures: [{ id: 1 }],
    });
  });

  it('reports unknown rather than failed for a transaction db-sync has not indexed', async () => {
    const tx = 'f'.repeat(64);
    const db = fakeDb('get-transaction-status.sql').on(
      'get-transaction-status.sql',
      [{ tx_exists: false, voting_procedures: [] }],
    );
    const { data } = await new DbSyncChainDataProvider(db).transactions.get(tx);
    // "not indexed" cannot be told from "still in the mempool"
    expect(data.status).toBe('unknown');
  });

  it('rejects a non-hex hash', async () => {
    const db = fakeDb('get-transaction-status.sql');
    await expect(
      new DbSyncChainDataProvider(db).transactions.get('zzz'),
    ).rejects.toMatchObject({ code: 'INVALID_INPUT' });
  });
});

describe('surveys.getDefinition', () => {
  it('normalises the hash, binds it once and reports label 17', async () => {
    const tx = 'AB'.repeat(32);
    const db = fakeDb('get-survey-definition.sql').on(
      'get-survey-definition.sql',
      [{ payload_cbor_hex: 'a1' }],
    );
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).surveys!.getDefinition(tx);

    expect(db.callsTo('get-survey-definition.sql')[0]!.params).toEqual([
      tx.toLowerCase(),
    ]);
    expect(data).toEqual({
      txHash: tx.toLowerCase(),
      metadataLabel: 17,
      payloadCborHex: 'a1',
    });
  });

  it('returns null when the transaction carries no label-17 metadata', async () => {
    const db = fakeDb('get-survey-definition.sql').on(
      'get-survey-definition.sql',
      [],
    );
    await expect(
      new DbSyncChainDataProvider(db).surveys!.getDefinition('ab'.repeat(32)),
    ).resolves.toMatchObject({ data: null });
  });

  it('requires a full 64-character hash', async () => {
    const db = fakeDb('get-survey-definition.sql');
    for (const bad of ['ab', 'z'.repeat(64), 'ab'.repeat(33)]) {
      await expect(
        new DbSyncChainDataProvider(db).surveys!.getDefinition(bad),
      ).rejects.toMatchObject({ code: 'INVALID_INPUT' });
    }
    expect(db.calls).toHaveLength(0);
  });
});
