import { Test } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import request from 'supertest';
import { AppModule } from '../src/app.module';
import { DbService } from '../src/db/db.service';
import { SqlService } from '../src/sql/sq.service';
import { ConfigService } from '../src/config/config.service';
import type { Server } from 'node:http';

const large = '9007199254740993';
const drep = (hash: string, amount: string) => ({
  drep_hash: hash,
  view: hash,
  has_script: false,
  url: 'https://example.org',
  metadata_hash: null,
  deposit: large,
  amount,
  active: true,
  tx_hash: null,
  last_register_time: '2026-09-18T00:00:00Z',
  latest_deposit: '1',
  has_non_deregister_voting_anchor: true,
  votes_last_year: null,
});
const proposal = {
  id: '1',
  tx_hash: 'ab',
  index: 0,
  type: 'InfoAction',
  expiration: null,
  expiry_date: null,
  time: '2026-09-18T00:00:00Z',
  epoch_no: 1,
  yes_votes: large,
  no_votes: '1',
  abstain_votes: '0',
  pool_yes_votes: '0',
  pool_no_votes: '0',
  pool_abstain_votes: '0',
  cc_yes_votes: '0',
  cc_no_votes: '0',
  cc_abstain_votes: '0',
  prev_gov_action_index: null,
};

describe('exact integers on the wire', () => {
  let app: INestApplication<Server>;
  const query = jest.fn((sql: string) => {
    if (sql.startsWith('SELECT MAX')) {
      return Promise.reject(new Error('No warmup database'));
    }
    const rows: Record<string, unknown[]> = {
      'get-network-metrics.sql': [
        {
          unique_delegators: 0,
          total_delegations: 0,
          total_gov_action_proposals: 0,
          total_drep_votes: 0,
          total_registered_dreps: 0,
          total_drep_distr: null,
          total_active_dreps: 0,
          total_inactive_dreps: 0,
          total_active_cip119_compliant_dreps: 0,
          total_registered_direct_voters: 0,
          no_of_committee_members: 0,
          quorum_numerator: 2,
          quorum_denominator: 3,
        },
      ],
      'get-network-total-stake.sql': [
        {
          total_stake_controlled_by_active_dreps: large,
          total_stake_controlled_by_spos: large,
          always_abstain_voting_power: '1',
          always_no_confidence_voting_power: '0',
        },
      ],
      'get-voting-power.sql': [{ amount: large }],
      'get-stake-key-voting-power.sql': [{ total_balance: large }],
      'list-dreps.sql': [drep('aa', '9007199254740992'), drep('bb', large)],
      'list-proposals.sql': [proposal],
      'get-previous-enacted-governance-action-proposal-details.sql': [],
    };
    return Promise.resolve({ rows: rows[sql] ?? [] });
  });
  beforeAll(async () => {
    const module = await Test.createTestingModule({ imports: [AppModule] })
      .overrideProvider(ConfigService)
      .useValue({
        get: () => ({
          cacheMaxEntries: 256,
          cacheDurationSeconds: 20,
          drepListCacheDurationSeconds: 600,
        }),
      })
      .overrideProvider(SqlService)
      .useValue({ load: (name: string) => name })
      .overrideProvider(DbService)
      .useValue({ query })
      .compile();
    app = module.createNestApplication<INestApplication<Server>>();
    app.useLogger(false);
    await app.init();
  });
  afterAll(async () => {
    await app.close();
  });
  it.each(['/drep/get-voting-power/ab', '/ada-holder/get-voting-power/ab'])(
    'serializes a primitive integer exactly on %s',
    async (url) => {
      const response = await request(app.getHttpServer())
        .get(url)
        .expect(200)
        .expect('Content-Type', /json/);
      expect(response.text).toBe(large);
    },
  );
  it('preserves zero for an empty DRep distribution aggregate', async () => {
    const response = await request(app.getHttpServer())
      .get('/network/metrics')
      .expect(200);
    const body = response.body as { totalDRepDistr: number };
    expect(body.totalDRepDistr).toBe(0);
  });
  it('preserves network totals as unquoted JSON numbers', async () => {
    const response = await request(app.getHttpServer())
      .get('/network/total-stake')
      .expect(200);
    expect(response.text).toContain(`"totalStakeControlledByDReps":${large}`);
    expect(response.text).toContain('"alwaysNoConfidenceVotingPower":0');
  });
  it('sorts DReps precisely and preserves deposit and voting power across cache hits', async () => {
    for (let n = 0; n < 2; n++) {
      const response = await request(app.getHttpServer())
        .get('/drep/list?sort=VotingPower')
        .expect(200);
      const body = response.body as { elements: { drepId: string }[] };
      expect(body.elements.map((item) => item.drepId)).toEqual(['bb', 'aa']);
      expect(response.text).toContain(`"votingPower":${large}`);
      expect(response.text).toContain(`"deposit":${large}`);
      expect(response.text).toContain('"votesLastYear":null');
    }
  });
  it('preserves proposal vote totals', async () => {
    const response = await request(app.getHttpServer())
      .get('/proposal/list')
      .expect(200);
    expect(response.text).toContain(`"dRepYesVotes":${large}`);
  });
  it('defaults enacted type only when absent, retaining other valid types', async () => {
    await request(app.getHttpServer())
      .get('/proposal/enacted-details?type=InfoAction')
      .expect(200);
    expect(query).toHaveBeenLastCalledWith(
      'get-previous-enacted-governance-action-proposal-details.sql',
      ['InfoAction'],
    );
    await request(app.getHttpServer())
      .get('/proposal/enacted-details')
      .expect(200);
    expect(query).toHaveBeenLastCalledWith(
      'get-previous-enacted-governance-action-proposal-details.sql',
      ['HardForkInitiation'],
    );
  });
});
