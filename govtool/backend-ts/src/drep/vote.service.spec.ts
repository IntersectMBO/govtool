import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { VoteService } from './vote.service';
import { DRepService } from './drep.service';
import { ProposalService } from '../proposal/proposal.service';
import { ProposalResponse } from '../proposal/proposal.type';
import { CacheService } from '../cache/cache.service';
import { ConfigService } from '../config/config.service';
import { DbService } from '../db/db.service';
import { SqlService } from '../sql/sq.service';

const hash = 'ab'.repeat(32);
const row = {
  proposal_id: '9',
  gov_action_id: `${hash}#0`,
  drep_id: 'ab',
  vote: 'yes',
  url: null,
  doc_hash: null,
  epoch_no: 42,
  date: '2026-09-18T00:00:00Z',
  vote_tx_hash: 'cd',
};
const proposal = {
  txHash: hash,
  index: 0,
  type: 'InfoAction',
  title: 'Example',
} as ProposalResponse;

describe('DRep vote parity', () => {
  const cache = () =>
    new CacheService({
      get: () => ({ cacheMaxEntries: 256, cacheDurationSeconds: 20 }),
    } as ConfigService);
  function setup() {
    const db = { query: jest.fn().mockResolvedValue({ rows: [row] }) };
    const sql = { load: jest.fn().mockReturnValue('SQL') };
    const c = cache();
    const votes = new VoteService(
      db as unknown as DbService,
      sql as unknown as SqlService,
      c,
    );
    const proposals = new ProposalService(
      db as unknown as DbService,
      sql as unknown as SqlService,
      c,
      votes,
    );
    return { db, sql, c, votes, proposals };
  }
  it('SQL exposes every named field needed by the vote mapper', () => {
    const sql = readFileSync(
      resolve(__dirname, '../../sql/get-votes.sql'),
      'utf8',
    );
    for (const key of Object.keys(row))
      expect(sql).toMatch(new RegExp(` AS ${key}(?:,|\\s)`, 'i'));
    expect(sql).toMatch(/block\.time AS date/);
    expect(sql).toMatch(/voting_procedure\.id DESC/);
  });
  it('returns vote details including date and nullable metadata', async () => {
    const { db, sql, c, votes, proposals } = setup();
    jest.spyOn(proposals, 'getProposals').mockResolvedValue([proposal]);
    const dreps = new DRepService(
      db as unknown as DbService,
      sql as unknown as SqlService,
      proposals,
      c,
      votes,
    );
    const result = await dreps.getVotes('ab');
    expect(result).toEqual([
      {
        proposal,
        vote: {
          proposalId: '9',
          drepId: 'ab',
          vote: 'yes',
          url: null,
          metadataHash: null,
          epochNo: 42,
          date: '2026-09-18T00:00:00.000Z',
          txHash: 'cd',
        },
      },
    ]);
  });
  it('excludes votes before total and pagination without leaking between DReps', async () => {
    const { db, proposals } = setup();
    jest
      .spyOn(proposals, 'getProposals')
      .mockResolvedValue([
        proposal,
        { ...proposal, index: 1 },
        { ...proposal, index: 2 },
      ]);
    const page = { type: [], page: 0, pageSize: 1 };
    expect(await proposals.list({ ...page, drepId: 'ab' })).toMatchObject({
      total: 2,
      elements: [{ index: 1 }],
    });
    db.query.mockResolvedValue({ rows: [] });
    expect(await proposals.list({ ...page, drepId: 'cd' })).toMatchObject({
      total: 3,
      elements: [{ index: 0 }],
    });
    expect(await proposals.list(page)).toMatchObject({
      total: 3,
      elements: [{ index: 0 }],
    });
  });
  it('returns the matching own vote only when supplied and present', async () => {
    const { proposals } = setup();
    jest
      .spyOn(proposals, 'getProposals')
      .mockResolvedValue([proposal, { ...proposal, index: 1 }]);
    expect((await proposals.get(`${hash}#0`, 'ab')).vote).toMatchObject({
      vote: 'yes',
    });
    expect((await proposals.get(`${hash}#1`, 'ab')).vote).toBeNull();
    expect((await proposals.get(`${hash}#0`)).vote).toBeNull();
  });
  it('resolves a proposal by id, not by metadata text that quotes the id', async () => {
    const { proposals } = setup();
    const quoting = {
      ...proposal,
      index: 7,
      rationale: `supersedes ${hash}#0`,
    } as ProposalResponse;
    jest
      .spyOn(proposals, 'getProposals')
      .mockResolvedValue([proposal, quoting]);
    expect((await proposals.get(`${hash}#0`)).proposal.index).toBe(0);
    await expect(proposals.get(`${hash}#9`)).rejects.toThrow(/not found/);
  });
  it('reads the proposal snapshot once and never duplicates a vote', async () => {
    const { db, sql, c, votes, proposals } = setup();
    db.query.mockResolvedValue({
      rows: [row, { ...row, gov_action_id: `${hash}#1`, proposal_id: '10' }],
    });
    const quoting = {
      ...proposal,
      index: 7,
      rationale: `supersedes ${hash}#0 and ${hash}#1`,
    } as ProposalResponse;
    const snapshot = jest
      .spyOn(proposals, 'getProposals')
      .mockResolvedValue([proposal, { ...proposal, index: 1 }, quoting]);
    const dreps = new DRepService(
      db as unknown as DbService,
      sql as unknown as SqlService,
      proposals,
      c,
      votes,
    );
    const result = await dreps.getVotes('ab');
    expect(snapshot).toHaveBeenCalledTimes(1);
    expect(result.map((entry) => entry.proposal.index).sort()).toEqual([0, 1]);
  });
});
