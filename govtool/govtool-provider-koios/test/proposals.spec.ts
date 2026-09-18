import { FakeKoios } from './fake-http';
import { expectChainDataError } from './expect-error';
import { KoiosProposalsApi } from '../src/api/governance/proposals.api';
import { KoiosVotesApi } from '../src/api/governance/votes.api';

const INFO_ACTION = {
  block_time: 1789647505,
  proposal_id:
    'gov_action17m7nv7839mw93hv889tqzj0umv9ckm780f0nq02fep78f50uedxqq6g5mt9',
  proposal_tx_hash:
    'f6fd3678f12edc58dd8739560149fcdb0b8b6fc77a5f303d49c87c74d1fccb4c',
  proposal_index: 0,
  proposal_type: 'InfoAction',
  proposal_description: { tag: 'InfoAction' },
  previous_gov_action_proposal_id: null,
  deposit: '100000000000',
  return_address: 'stake1u9ya9ajt7gmmdqlr503t2s72duvtmc4kwdck299568x4xjqqd22nl',
  proposed_epoch: 656,
  ratified_epoch: null,
  enacted_epoch: null,
  dropped_epoch: null,
  expired_epoch: null,
  expiration: 663,
  meta_url: 'https://example.test/info.jsonld',
  meta_hash: 'ab85',
  meta_json: { body: { title: 'Raise k to 1000', abstract: 'A poll.' } },
  meta_comment: null,
  meta_language: 'en-us',
  meta_is_valid: true,
  withdrawal: [],
  param_proposal: null,
};

function api(koios: FakeKoios): KoiosProposalsApi {
  const client = koios.client();
  return new KoiosProposalsApi(client, new KoiosVotesApi(client));
}

describe('KoiosProposalsApi', () => {
  it('maps identity, lifecycle and metadata off one proposal_list row', async () => {
    const koios = new FakeKoios().on('proposal_list', [INFO_ACTION]);
    const { data } = await api(koios).get(INFO_ACTION.proposal_id);

    expect(data.id).toBe(INFO_ACTION.proposal_id);
    expect(data.type).toBe('InfoAction');
    expect(data.lifecycle.status).toBe('live');
    expect(data.lifecycle.submitted).toEqual({
      epoch: 656,
      time: '2026-09-17T12:18:25.000Z',
    });
    expect(data.lifecycle.expires).toEqual({ epoch: 663 });
    expect(data.metadata).toMatchObject({
      standard: 'CIP108',
      status: 'valid',
      body: { title: 'Raise k to 1000', abstract: 'A poll.' },
    });
  });

  it('accepts the txHash#index form as well as the CIP-129 id', async () => {
    const koios = new FakeKoios().on('proposal_list', [INFO_ACTION]);
    await api(koios).get(`${INFO_ACTION.proposal_tx_hash}#0`);
    expect(koios.lastCallTo('proposal_list')!.params['proposal_id']).toBe(
      `eq.${INFO_ACTION.proposal_id}`,
    );
  });

  it('marks metadata invalid when Koios rejected the document', async () => {
    const koios = new FakeKoios().on('proposal_list', [
      { ...INFO_ACTION, meta_is_valid: false, meta_comment: 'hash mismatch' },
    ]);
    const { data } = await api(koios).get(INFO_ACTION.proposal_id);
    expect(data.metadata).toMatchObject({
      status: 'invalid',
      failureMessage: 'hash mismatch',
    });
  });

  describe('typed bodies from proposal_description', () => {
    async function bodyOf(row: Record<string, unknown>) {
      const koios = new FakeKoios().on('proposal_list', [
        { ...INFO_ACTION, ...row },
      ]);
      const { data } = await api(koios).get(INFO_ACTION.proposal_id);
      return data.body;
    }

    it('types a ParameterChange from param_proposal', async () => {
      const body = await bodyOf({
        proposal_type: 'ParameterChange',
        param_proposal: { min_pool_cost: 75000000 },
        proposal_description: {
          tag: 'ParameterChange',
          contents: [
            { txId: 'c75b', govActionIx: 0 },
            { minPoolCost: 75000000 },
            'fa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a64',
          ],
        },
      });
      expect(body).toEqual({
        type: 'ParameterChange',
        changes: { min_pool_cost: 75000000 },
        guardrailsScriptHash:
          'fa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a64',
      });
    });

    it('types a HardForkInitiation', async () => {
      const body = await bodyOf({
        proposal_type: 'HardForkInitiation',
        proposal_description: {
          tag: 'HardForkInitiation',
          contents: [
            { txId: '0b19', govActionIx: 0 },
            { major: 11, minor: 0 },
          ],
        },
      });
      expect(body).toEqual({
        type: 'HardForkInitiation',
        protocolVersion: { major: 11, minor: 0 },
      });
    });

    it('types TreasuryWithdrawals from the bech32 withdrawal array', async () => {
      const body = await bodyOf({
        proposal_type: 'TreasuryWithdrawals',
        withdrawal: [
          { amount: '11787063000000', stake_address: 'stake178...' },
          { amount: '1000000', stake_address: 'stake1xyz' },
        ],
        proposal_description: {
          tag: 'TreasuryWithdrawals',
          contents: [[], 'fa24fb30'],
        },
      });
      expect(body).toEqual({
        type: 'TreasuryWithdrawals',
        withdrawals: [
          { stakeAddress: 'stake178...', amount: '11787063000000' },
          { stakeAddress: 'stake1xyz', amount: '1000000' },
        ],
        totalAmount: '11787064000000',
        guardrailsScriptHash: 'fa24fb30',
      });
    });

    it('splits UpdateCommittee members out of the credential-prefixed keys', async () => {
      const body = await bodyOf({
        proposal_type: 'NewCommittee',
        proposal_description: {
          tag: 'UpdateCommittee',
          contents: [
            { txId: '4dab', govActionIx: 0 },
            [{ scriptHash: '349e' }, { keyHash: 'dc0d' }],
            { 'keyHash-0af9': 799, 'scriptHash-16fe': 800 },
            { numerator: 2, denominator: 3 },
          ],
        },
      });
      expect(body).toEqual({
        type: 'UpdateCommittee',
        added: [
          {
            coldCredential: '0af9',
            isScriptBased: false,
            termExpiryEpoch: 799,
          },
          { coldCredential: '16fe', isScriptBased: true, termExpiryEpoch: 800 },
        ],
        removed: [
          { coldCredential: '349e', isScriptBased: true },
          { coldCredential: 'dc0d', isScriptBased: false },
        ],
        quorum: { numerator: 2, denominator: 3 },
      });
    });

    it('renames NewCommittee to the ledger name on the action type', async () => {
      const koios = new FakeKoios().on('proposal_list', [
        {
          ...INFO_ACTION,
          proposal_type: 'NewCommittee',
          proposal_description: { tag: 'UpdateCommittee', contents: [] },
        },
      ]);
      const { data } = await api(koios).get(INFO_ACTION.proposal_id);
      expect(data.type).toBe('UpdateCommittee');
    });

    it('types a NewConstitution anchor', async () => {
      const body = await bodyOf({
        proposal_type: 'NewConstitution',
        proposal_description: {
          tag: 'NewConstitution',
          contents: [
            { txId: '8c65', govActionIx: 0 },
            {
              anchor: { url: 'ipfs://bafk', dataHash: 'b368' },
              script: 'fa24',
            },
          ],
        },
      });
      expect(body).toEqual({
        type: 'NewConstitution',
        anchor: { url: 'ipfs://bafk', dataHash: 'b368' },
        guardrailsScriptHash: 'fa24',
      });
    });

    it('leaves body absent but rawBody present for an unreadable description', async () => {
      const koios = new FakeKoios().on('proposal_list', [
        { ...INFO_ACTION, proposal_description: { tag: 'SomethingNew' } },
      ]);
      const { data } = await api(koios).get(INFO_ACTION.proposal_id);
      expect(data.body).toBeUndefined();
      expect(data.rawBody).toEqual({ tag: 'SomethingNew' });
    });
  });

  describe('lifecycle status', () => {
    it.each([
      ['enacted', { enacted_epoch: 660, ratified_epoch: 659 }],
      ['ratified', { ratified_epoch: 659 }],
      ['dropped', { dropped_epoch: 659 }],
      ['expired', { expired_epoch: 663 }],
      ['live', {}],
    ])('derives %s', async (status, row) => {
      const koios = new FakeKoios().on('proposal_list', [
        { ...INFO_ACTION, ...row },
      ]);
      const { data } = await api(koios).get(INFO_ACTION.proposal_id);
      expect(data.lifecycle.status).toBe(status);
    });
  });

  describe('list', () => {
    it('pushes type and status down as PostgREST filters', async () => {
      const koios = new FakeKoios().on('proposal_list', [INFO_ACTION], {
        total: 1,
      });
      await api(koios).list({
        type: ['UpdateCommittee', 'InfoAction'],
        status: ['live'],
        sort: 'oldest',
        limit: 10,
      });

      const call = koios.lastCallTo('proposal_list')!;
      expect(call.params['proposal_type']).toBe('in.(NewCommittee,InfoAction)');
      expect(call.params['enacted_epoch']).toBe('is.null');
      expect(call.params['order']).toBe('block_time.asc');
      expect(call.params['limit']).toBe('10');
    });

    it('reports the total from content-range', async () => {
      const koios = new FakeKoios().on('proposal_list', [INFO_ACTION], {
        total: 42,
      });
      const { data } = await api(koios).list({ limit: 1 });
      expect(data.total).toBe(42);
      expect(data.nextCursor).toBe('1');
    });

    it.each(['search', 'voterId'] as const)('refuses %s', async (field) => {
      const koios = new FakeKoios().on('proposal_list', []);
      const error = await expectChainDataError(
        api(koios).list({ [field]: 'x' }),
      );
      expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
    });

    it('refuses to sort by a vote weight it cannot join', async () => {
      const koios = new FakeKoios().on('proposal_list', []);
      const error = await expectChainDataError(
        api(koios).list({ sort: 'mostYesVotes' }),
      );
      expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
    });

    it('refuses several statuses at once rather than filtering wrongly', async () => {
      const koios = new FakeKoios().on('proposal_list', []);
      const error = await expectChainDataError(
        api(koios).list({ status: ['live', 'enacted'] }),
      );
      expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
    });
  });

  describe('tallies', () => {
    const SUMMARY = {
      proposal_type: 'InfoAction',
      epoch_no: 656,
      drep_yes_votes_cast: 3,
      drep_active_yes_vote_power: '1971111681234',
      drep_yes_vote_power: '1971111681234',
      drep_yes_pct: 0.04,
      drep_no_votes_cast: 0,
      drep_active_no_vote_power: '0',
      drep_no_vote_power: '5238713164710704',
      drep_no_pct: 99.96,
      drep_abstain_votes_cast: 0,
      drep_active_abstain_vote_power: '0',
      drep_always_no_confidence_vote_power: '139044608085302',
      drep_always_abstain_vote_power: '10302233558851005',
      pool_yes_votes_cast: 1,
      pool_active_yes_vote_power: '2003163539442',
      pool_yes_vote_power: '2003163539442',
      pool_yes_pct: 0.02,
      pool_no_votes_cast: 0,
      pool_active_no_vote_power: '0',
      pool_no_vote_power: '9605665167962540',
      pool_no_pct: 99.98,
      pool_abstain_votes_cast: 0,
      pool_active_abstain_vote_power: '0',
      committee_yes_votes_cast: 0,
      committee_yes_pct: 0,
      committee_no_votes_cast: 0,
      committee_no_pct: 100,
      committee_abstain_votes_cast: 0,
    };

    it('carries the stake that actually voted, not the CIP-1694 counting figure', async () => {
      const koios = new FakeKoios().on('proposal_voting_summary', [SUMMARY]);
      const { data } = await api(koios).getTallies(INFO_ACTION.proposal_id);

      const drep = data.find((tally) => tally.role === 'drep')!;
      expect(drep.stake).toEqual({
        yes: '1971111681234',
        no: '0',
        abstain: '0',
      });
      expect(drep.count).toEqual({ yes: 3, no: 0, abstain: 0 });
      // The gap between drep_no_vote_power and the active figure is stake that
      // did not vote, surfaced separately rather than counted as a `no`.
      expect(drep.notVotedStake).toBe('5238713164710704');
    });

    it('gives the committee counts and no stake, because it votes by head', async () => {
      const koios = new FakeKoios().on('proposal_voting_summary', [SUMMARY]);
      const { data } = await api(koios).getTallies(INFO_ACTION.proposal_id);
      const cc = data.find((tally) => tally.role === 'cc')!;
      expect(cc.stake).toBeUndefined();
      expect(cc.count).toEqual({ yes: 0, no: 0, abstain: 0 });
    });

    it('never reports a threshold, because Koios only has floats', async () => {
      const koios = new FakeKoios().on('proposal_voting_summary', [SUMMARY]);
      const { data } = await api(koios).getTallies(INFO_ACTION.proposal_id);
      for (const tally of data) {
        expect(tally.threshold).toBeUndefined();
        expect(tally.passing).toBeUndefined();
      }
    });

    it('filters to one role when asked', async () => {
      const koios = new FakeKoios().on('proposal_voting_summary', [SUMMARY]);
      const { data } = await api(koios).getTallies(INFO_ACTION.proposal_id, {
        role: 'spo',
      });
      expect(data).toHaveLength(1);
      expect(data[0]!.role).toBe('spo');
    });
  });

  it('finds the currently enacted action of a type', async () => {
    const koios = new FakeKoios().on('proposal_list', [
      { ...INFO_ACTION, proposal_type: 'NewCommittee', enacted_epoch: 600 },
    ]);
    const { data } = await api(koios).getEnacted('UpdateCommittee');

    const call = koios.lastCallTo('proposal_list')!;
    expect(call.params['proposal_type']).toBe('eq.NewCommittee');
    expect(call.params['enacted_epoch']).toBe('not.is.null');
    expect(data!.type).toBe('UpdateCommittee');
    expect(data!.enactedAt).toEqual({ epoch: 600 });
  });

  it('returns null rather than throwing when nothing of a type is enacted', async () => {
    const koios = new FakeKoios().on('proposal_list', []);
    expect((await api(koios).getEnacted('NoConfidence')).data).toBeNull();
  });

  it('resolves actions by submission tx, for post-submission confirmation', async () => {
    const koios = new FakeKoios().on('proposal_list', [INFO_ACTION]);
    const { data } = await api(koios).listByTx(INFO_ACTION.proposal_tx_hash);
    expect(data).toEqual([
      {
        id: INFO_ACTION.proposal_id,
        txHash: INFO_ACTION.proposal_tx_hash,
        index: 0,
      },
    ]);
  });

  it('synthesises an activity feed from the epoch columns and the votes', async () => {
    const koios = new FakeKoios()
      .on('proposal_list', [
        { ...INFO_ACTION, ratified_epoch: 659, enacted_epoch: 660 },
      ])
      .on('vote_list', [
        {
          vote_tx_hash: 'c9b1',
          voter_role: 'DRep',
          voter_id:
            'drep1ytc6867ae0xmkekvmex79r9akyy28eu8nu03jf3xu9fle6c82l4eq',
          proposal_id: INFO_ACTION.proposal_id,
          proposal_tx_hash: INFO_ACTION.proposal_tx_hash,
          proposal_index: 0,
          proposal_type: 'InfoAction',
          epoch_no: 657,
          block_height: 13953841,
          block_time: 1789675385,
          vote: 'Yes',
          meta_url: null,
          meta_hash: null,
          meta_json: null,
        },
      ]);

    const { data } = await api(koios).listActivity(INFO_ACTION.proposal_id);
    expect(data.elements.map((event) => event.type)).toEqual([
      'enacted',
      'ratified',
      'voted',
      'submitted',
    ]);
  });
});
