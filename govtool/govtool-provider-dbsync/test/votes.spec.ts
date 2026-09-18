import { DbSyncChainDataProvider } from '../src';
import type { DRepVoteTuple } from '../src/rows';
import { drepInfoRow, fakeDb, proposalRow } from './fake-db';

const DREP = 'a'.repeat(56);
const TX = 'd'.repeat(64);
const VOTE_TX = '9'.repeat(64);

/**
 * `get-votes.sql` returns nine positional columns:
 *   gov_action_proposal_id, CONCAT(txHash,'#',index), encode(drep raw),
 *   LOWER(vote), anchor url, encode(anchor hash), epoch_no, time, vote tx hash
 *
 * Its SELECT list leaves four of them unaliased, so by NAME the row is
 * `{ gov_action_proposal_id, concat, encode, lower, url, encode, epoch_no,
 * time, vote_tx_hash }` — the DRep hash is overwritten by the anchor hash and
 * six of the nine fields the legacy TypeScript service reads are absent.
 * That is why this statement is read positionally.
 */
function voteTuple(overrides: Record<number, unknown> = {}): DRepVoteTuple {
  const tuple: DRepVoteTuple = [
    '42',
    `${TX}#0`,
    DREP,
    'yes',
    'https://example.com/rationale.jsonld',
    'f'.repeat(64),
    501,
    new Date('2026-01-10T00:00:00.000Z'),
    VOTE_TX,
  ];
  for (const [i, value] of Object.entries(overrides)) {
    (tuple as unknown[])[Number(i)] = value;
  }
  return tuple;
}

function votesDb() {
  return fakeDb('get-votes.sql', 'get-drep-info.sql', 'list-proposals.sql')
    .onArrays('get-votes.sql', [voteTuple()])
    .on('get-drep-info.sql', [drepInfoRow()])
    .on('list-proposals.sql', [proposalRow()]);
}

describe('governance.dreps.listVotes', () => {
  it('reads the statement positionally, not by column name', async () => {
    const db = votesDb();
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.dreps.listVotes(DREP);

    expect(db.callsTo('get-votes.sql')[0]!.mode).toBe('array');
    expect(db.callsTo('get-votes.sql')[0]!.params).toEqual([DREP]);

    expect(data.elements).toHaveLength(1);
    const { vote, proposal } = data.elements[0]!;

    // Every one of these comes from a column the name-based read loses.
    expect(vote!.voter.hash).toBe(DREP);
    expect(vote!.vote).toBe('yes');
    expect(vote!.proposal.txHash).toBe(TX);
    expect(vote!.proposal.index).toBe(0);
    expect(vote!.proposal.providerId).toBe('42');
    expect(vote!.at).toEqual({ epoch: 501, time: '2026-01-10T00:00:00.000Z' });
    expect(vote!.txRef).toEqual({ txHash: VOTE_TX });
    expect(vote!.rationale).toMatchObject({
      standard: 'CIP100',
      anchor: {
        url: 'https://example.com/rationale.jsonld',
        dataHash: 'f'.repeat(64),
      },
    });
    expect(vote!.isCurrent).toBe(true);
    expect(proposal.txHash).toBe(TX);
  });

  it('carries the voter identity, including script-based credentials', async () => {
    const db = votesDb().on('get-drep-info.sql', [
      drepInfoRow({ is_script_based: true }),
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.dreps.listVotes(DREP);

    expect(data.elements[0]!.vote!.voter).toMatchObject({
      role: 'drep',
      hash: DREP,
      isScriptBased: true,
    });
    // a script credential carries a different CIP-129 header, so a different id
    const keyBased = await new DbSyncChainDataProvider(
      votesDb(),
    ).governance.dreps.listVotes(DREP);
    expect(data.elements[0]!.vote!.voter.id).not.toBe(
      keyBased.data.elements[0]!.vote!.voter.id,
    );
  });

  it('reports no rationale when the vote carried no anchor', async () => {
    const db = votesDb().onArrays('get-votes.sql', [
      voteTuple({ 4: null, 5: null }),
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.dreps.listVotes(DREP);
    expect(data.elements[0]!.vote!.rationale).toBeNull();
  });

  it('accepts all three lowercase vote values the statement produces', async () => {
    for (const choice of ['yes', 'no', 'abstain'] as const) {
      const db = votesDb().onArrays('get-votes.sql', [
        voteTuple({ 3: choice }),
      ]);
      const { data } = await new DbSyncChainDataProvider(
        db,
      ).governance.dreps.listVotes(DREP);
      expect(data.elements[0]!.vote!.vote).toBe(choice);
    }
  });

  it('fails loudly on a vote value outside the ledger enum', async () => {
    const db = votesDb().onArrays('get-votes.sql', [voteTuple({ 3: 'Yes' })]);
    await expect(
      new DbSyncChainDataProvider(db).governance.dreps.listVotes(DREP),
    ).rejects.toMatchObject({ code: 'INTERNAL' });
  });

  it('drops a vote whose action is no longer live, as the legacy path did', async () => {
    // list-proposals.sql returns live actions only, so a vote on a concluded
    // action resolves to nothing and is omitted rather than half-populated.
    const db = votesDb().on('list-proposals.sql', []);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.dreps.listVotes(DREP);
    expect(data.elements).toEqual([]);
    expect(data.total).toBe(0);
  });

  it('resolves each vote against its own action', async () => {
    const other = 'e'.repeat(64);
    const db = votesDb().onArrays('get-votes.sql', [
      voteTuple(),
      voteTuple({ 1: `${other}#2`, 3: 'no' }),
    ]);
    await new DbSyncChainDataProvider(db).governance.dreps.listVotes(DREP);

    expect(
      db.callsTo('list-proposals.sql').map((call) => call.params[0]),
    ).toEqual([`${TX}#0`, `${other}#2`]);
  });

  it('returns nothing when the DRep has never voted', async () => {
    const db = votesDb().onArrays('get-votes.sql', []);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.dreps.listVotes(DREP);
    expect(data.elements).toEqual([]);
    expect(db.callsTo('list-proposals.sql')).toHaveLength(0);
  });
});
