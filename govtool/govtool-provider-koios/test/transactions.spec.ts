import { FakeKoios } from './fake-http';
import { KoiosTransactionsApi } from '../src/api/transactions.api';

const TX = 'c9b11588e508c325a260754086a6c5f40fe0ba4daa1c92a77ee59c8b55949755';
const DREP = 'drep1ytc6867ae0xmkekvmex79r9akyy28eu8nu03jf3xu9fle6c82l4eq';

function api(koios: FakeKoios): KoiosTransactionsApi {
  return new KoiosTransactionsApi(koios.client());
}

describe('KoiosTransactionsApi', () => {
  it('reports unknown for a hash Koios has not seen, rather than failing', async () => {
    const koios = new FakeKoios().on('tx_status', [
      { tx_hash: TX, num_confirmations: null },
    ]);
    const { data } = await api(koios).get(TX);
    expect(data).toEqual({ txHash: TX, status: 'unknown' });
    expect(koios.callsTo('tx_info')).toHaveLength(0);
  });

  it('classifies a vote transaction', async () => {
    const koios = new FakeKoios()
      .on('tx_status', [{ tx_hash: TX, num_confirmations: 412 }])
      .on('tx_info', [
        {
          tx_hash: TX,
          block_hash: 'aa',
          block_height: 13953841,
          epoch_no: 656,
          absolute_slot: 1,
          tx_timestamp: 1789675385,
          certificates: [],
          voting_procedures: [
            {
              vote: 'Yes',
              voter: DREP,
              voter_hex:
                'f1a3ebddcbcdbb66ccde4de28cbdb108a3e7879f1f192626e153fceb',
              voter_role: 'DRep',
              proposal_index: 0,
              proposal_tx_hash:
                'f6fd3678f12edc58dd8739560149fcdb0b8b6fc77a5f303d49c87c74d1fccb4c',
            },
          ],
          proposal_procedures: [],
        },
      ]);

    const { data } = await api(koios).get(TX);
    expect(data.status).toBe('confirmed');
    expect(data.confirmations).toBe(412);
    expect(data.includedAt).toEqual({
      epoch: 656,
      time: '2026-09-17T20:03:05.000Z',
    });
    expect(data.effects).toHaveLength(1);
    const effect = data.effects![0]!;
    expect(effect.kind).toBe('vote');
    if (effect.kind === 'vote') {
      expect(effect.vote.vote).toBe('yes');
      expect(effect.vote.voter.id).toBe(DREP);
      // The action id is rebuilt as CIP-129 from the tx hash and index.
      expect(effect.vote.proposal.id).toMatch(/^gov_action1/);
    }
  });

  it('classifies a DRep registration certificate', async () => {
    const koios = new FakeKoios()
      .on('tx_status', [{ tx_hash: TX, num_confirmations: 10 }])
      .on('tx_info', [
        {
          tx_hash: TX,
          epoch_no: 650,
          absolute_slot: 1,
          tx_timestamp: 1782150991,
          certificates: [
            {
              index: 0,
              type: 'drep_registration',
              info: {
                deposit: '500000000',
                drep_id: DREP,
                meta_hash: '\\x297cb7d2',
              },
            },
          ],
          voting_procedures: [],
          proposal_procedures: [],
        },
      ]);

    const { data } = await api(koios).get(TX);
    const effect = data.effects![0]!;
    expect(effect.kind).toBe('drepRegistration');
    if (effect.kind === 'drepRegistration') {
      expect(effect.action).toBe('register');
      expect(effect.drep.id).toBe(DREP);
    }
  });

  it('classifies a governance delegation certificate', async () => {
    const koios = new FakeKoios()
      .on('tx_status', [{ tx_hash: TX, num_confirmations: 10 }])
      .on('tx_info', [
        {
          tx_hash: TX,
          epoch_no: 650,
          absolute_slot: 1,
          tx_timestamp: 1782150991,
          certificates: [
            {
              index: 0,
              type: 'delegation_drep',
              info: {
                stake_address: 'stake1abc',
                drep_id: 'drep_always_abstain',
              },
            },
          ],
          voting_procedures: [],
          proposal_procedures: [],
        },
      ]);

    const effect = (await api(koios).get(TX)).data.effects![0]!;
    expect(effect.kind).toBe('delegation');
    if (effect.kind === 'delegation') {
      expect(effect.delegation.target).toEqual({
        kind: 'predefined',
        option: 'alwaysAbstain',
      });
    }
  });

  it('classifies a proposal submission', async () => {
    const koios = new FakeKoios()
      .on('tx_status', [{ tx_hash: TX, num_confirmations: 10 }])
      .on('tx_info', [
        {
          tx_hash: TX,
          epoch_no: 656,
          absolute_slot: 1,
          tx_timestamp: 1789647505,
          certificates: [],
          voting_procedures: [],
          proposal_procedures: [
            { type: 'InfoAction', index: 0, deposit: '100000000000' },
          ],
        },
      ]);

    const effect = (await api(koios).get(TX)).data.effects![0]!;
    expect(effect.kind).toBe('proposal');
    if (effect.kind === 'proposal') {
      expect(effect.proposal.txHash).toBe(TX);
      expect(effect.proposal.id).toMatch(/^gov_action1/);
    }
  });

  it('rejects a hash that is not hex', async () => {
    const koios = new FakeKoios();
    await expect(api(koios).get('not-a-hash')).rejects.toThrow();
  });
});
