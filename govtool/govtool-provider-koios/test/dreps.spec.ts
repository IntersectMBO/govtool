import { FakeKoios } from './fake-http';
import { expectChainDataError } from './expect-error';
import { KoiosDRepsApi } from '../src/api/governance/dreps.api';

const DREP = 'drep1ytc6867ae0xmkekvmex79r9akyy28eu8nu03jf3xu9fle6c82l4eq';
const HEX = 'f1a3ebddcbcdbb66ccde4de28cbdb108a3e7879f1f192626e153fceb';

const INFO = {
  drep_id: DREP,
  hex: HEX,
  has_script: false,
  drep_status: 'registered',
  deposit: '500000000',
  active: true,
  expires_epoch_no: 700,
  amount: '1824524915507',
  meta_url: 'https://example.test/drep.jsonld',
  meta_hash: '297c',
  live_delegator_count: 12,
};

function api(koios: FakeKoios): KoiosDRepsApi {
  return new KoiosDRepsApi(koios.client());
}

describe('KoiosDRepsApi', () => {
  describe('get', () => {
    it('maps identity, power and the registration Koios can derive', async () => {
      const koios = new FakeKoios().on('drep_info', [INFO]).on('drep_updates', [
        {
          drep_id: DREP,
          hex: HEX,
          has_script: false,
          update_tx_hash: '0167',
          cert_index: 0,
          block_time: 1782150991,
          action: 'registered',
          deposit: '500000000',
          meta_url: 'https://example.test/drep.jsonld',
          meta_hash: '297c',
          meta_json: null,
        },
      ]);

      const { data } = await api(koios).get(DREP);

      expect(data.id).toBe(DREP);
      expect(data.hash).toBe(HEX);
      expect(data.cip105Id).toMatch(/^drep1/);
      expect(data.cip105Id).not.toBe(DREP);
      expect(data.votingPower).toEqual({
        amount: '1824524915507',
        basis: 'active',
      });
      expect(data.registration).toMatchObject({
        status: 'active',
        deposit: '500000000',
      });
      expect(data.registration.registrationTx).toMatchObject({
        txHash: '0167',
        index: 0,
      });
      expect(data.registration.retiredAt).toBeNull();
      expect(data.delegators).toEqual({ live: 12 });
    });

    it.each([
      ['active', { drep_status: 'registered', active: true }],
      ['inactive', { drep_status: 'registered', active: false }],
      ['retired', { drep_status: 'deregistered', active: false }],
    ])('derives status %s from Koios own active flag', async (status, row) => {
      const koios = new FakeKoios()
        .on('drep_info', [{ ...INFO, ...row }])
        .on('drep_updates', []);
      const { data } = await api(koios).get(DREP);
      expect(data.registration.status).toBe(status);
    });

    it('returns a retired DRep rather than hiding it behind NOT_FOUND', async () => {
      const koios = new FakeKoios()
        .on('drep_info', [
          { ...INFO, drep_status: 'deregistered', active: false },
        ])
        .on('drep_updates', [
          {
            drep_id: DREP,
            update_tx_hash: 'bb',
            cert_index: 0,
            block_time: 1782150991,
            action: 'deregistered',
            deposit: null,
            meta_url: null,
            meta_hash: null,
            meta_json: null,
          },
        ]);

      const { data } = await api(koios).get(DREP);
      expect(data.registration.status).toBe('retired');
      expect(data.registration.retirementTx).toMatchObject({ txHash: 'bb' });
    });

    it('raises NOT_FOUND only for a credential never registered', async () => {
      const koios = new FakeKoios()
        .on('drep_info', [{ ...INFO, drep_status: 'not_registered' }])
        .on('drep_updates', []);
      const error = await expectChainDataError(api(koios).get(DREP));
      expect(error.code).toBe('NOT_FOUND');
    });

    it('treats the boolean-only form as never registered, having no third state', async () => {
      const koios = new FakeKoios()
        .on('drep_info', [
          { ...INFO, drep_status: undefined, registered: false },
        ])
        .on('drep_updates', []);
      const error = await expectChainDataError(api(koios).get(DREP));
      expect(error.code).toBe('NOT_FOUND');
    });

    it('accepts a raw hash and a CIP-105 id, binding CIP-129 to Koios', async () => {
      const koios = new FakeKoios()
        .on('drep_info', [INFO])
        .on('drep_updates', []);
      await api(koios).get(HEX);
      // The raw hash re-encodes to exactly the CIP-129 id Koios serves.
      expect(koios.lastCallTo('drep_info')!.body).toEqual({
        _drep_ids: [DREP],
      });

      const cip105 = (await api(koios).get(DREP)).data.cip105Id!;
      await api(koios).get(cip105);
      expect(koios.lastCallTo('drep_info')!.body).toEqual({
        _drep_ids: [DREP],
      });
    });

    it('expands metadata into a CIP-119 body with a validation status', async () => {
      const koios = new FakeKoios()
        .on('drep_info', [INFO])
        .on('drep_updates', [])
        .on('drep_metadata', [
          {
            drep_id: DREP,
            hex: HEX,
            has_script: false,
            meta_url: 'https://example.test/drep.jsonld',
            meta_hash: '297c',
            meta_json: {
              body: {
                givenName: 'BKIND',
                objectives: 'Decentralisation.',
                image: { contentUrl: 'https://img.test/l.png', sha256: '3520' },
                references: [
                  { '@type': 'Link', label: 'Site', uri: 'https://a.test' },
                  { '@type': 'Identity', label: 'GH', uri: 'https://b.test' },
                ],
              },
            },
            bytes: null,
            warning: null,
            language: 'en-us',
            comment: null,
            is_valid: true,
          },
        ]);

      const { data } = await api(koios).get(DREP, { expand: ['metadata'] });
      expect(data.metadata).toMatchObject({
        standard: 'CIP119',
        status: 'valid',
        body: {
          givenName: 'BKIND',
          image: { url: 'https://img.test/l.png', contentHash: '3520' },
        },
      });
      expect(data.metadata!.body!.linkReferences).toHaveLength(1);
      expect(data.metadata!.body!.identityReferences).toHaveLength(1);
    });

    it('marks metadata invalid when Koios failed to validate it', async () => {
      const koios = new FakeKoios()
        .on('drep_info', [INFO])
        .on('drep_updates', [])
        .on('drep_metadata', [
          {
            drep_id: DREP,
            meta_url: 'https://example.test/drep.jsonld',
            meta_hash: '297c',
            meta_json: null,
            warning: 'hash mismatch',
            is_valid: false,
          },
        ]);
      const { data } = await api(koios).get(DREP, { expand: ['metadata'] });
      expect(data.metadata).toMatchObject({
        status: 'invalid',
        failureMessage: 'hash mismatch',
      });
    });

    it('counts votes with a row-free exact count', async () => {
      const koios = new FakeKoios()
        .on('drep_info', [INFO])
        .on('drep_updates', [])
        .on(
          'drep_votes',
          [
            {
              proposal_id: 'gov_action1x',
              proposal_tx_hash: 'ab',
              proposal_index: 0,
              vote_tx_hash: 'cd',
              block_time: 1789675385,
              vote: 'Yes',
              meta_url: null,
              meta_hash: null,
            },
          ],
          { total: 37 },
        );

      const { data } = await api(koios).get(DREP, { expand: ['activity'] });
      expect(data.activity).toMatchObject({ votesCast: 37 });
      expect(data.activity!.notVotedCount).toBeUndefined();
      expect(koios.lastCallTo('drep_votes')!.headers['prefer']).toBe(
        'count=exact',
      );
    });

    it('always reports kind drep, never guessing at a direct voter', async () => {
      const koios = new FakeKoios()
        .on('drep_info', [INFO])
        .on('drep_updates', []);
      expect((await api(koios).get(DREP)).data.kind).toBe('drep');
    });

    it('refuses live voting power, which has no epoch snapshot', async () => {
      const koios = new FakeKoios().on('drep_info', [INFO]);
      const error = await expectChainDataError(
        api(koios).get(DREP, { expand: ['liveVotingPower'] }),
      );
      expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
    });
  });

  describe('list', () => {
    it('pages drep_list then hydrates the page with one drep_info', async () => {
      const koios = new FakeKoios()
        .on(
          'drep_list',
          [
            { drep_id: DREP, hex: HEX, has_script: false, registered: true },
            {
              drep_id: 'drep1other',
              hex: 'ab',
              has_script: false,
              registered: true,
            },
          ],
          { total: 900 },
        )
        .on('drep_info', [INFO]);

      const { data } = await api(koios).list({ limit: 2 });

      expect(koios.callsTo('drep_info')).toHaveLength(1);
      expect(data.total).toBe(900);
      expect(data.nextCursor).toBe('2');
      // Only the DRep that /drep_info returned is emitted.
      expect(data.elements).toHaveLength(1);
      expect(data.elements[0]!.id).toBe(DREP);
    });

    it('reads the deployment registered flag as well as the spec enum', async () => {
      const koios = new FakeKoios()
        .on('drep_list', [
          { drep_id: DREP, hex: HEX, has_script: false, registered: true },
        ])
        .on('drep_info', [
          { ...INFO, drep_status: undefined, registered: true },
        ]);
      const { data } = await api(koios).list();
      expect(data.elements[0]!.registration.status).toBe('active');
    });

    it.each([
      ['sort', { sort: 'votingPower' as const }],
      ['search', { search: 'bkind' }],
      ['kind', { kind: ['directVoter' as const] }],
    ])('refuses %s rather than ordering an arbitrary page', async (_l, q) => {
      const koios = new FakeKoios().on('drep_list', []);
      const error = await expectChainDataError(api(koios).list(q));
      expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
    });
  });

  it('returns a real per-epoch power series', async () => {
    const koios = new FakeKoios().on('drep_voting_power_history', [
      { drep_id: DREP, epoch_no: 656, amount: '1824524915507' },
      { drep_id: DREP, epoch_no: 655, amount: '1823707979809' },
      { drep_id: DREP, epoch_no: 654, amount: '1823153224751' },
    ]);

    const { data } = await api(koios).getVotingPower(DREP, {
      fromEpoch: 655,
      toEpoch: 656,
    });
    expect(data).toEqual([
      { amount: '1824524915507', epoch: 656, basis: 'active' },
      { amount: '1823707979809', epoch: 655, basis: 'active' },
    ]);
  });

  it('answers a bare power request from drep_info, without the history read', async () => {
    const koios = new FakeKoios().on('drep_info', [INFO]);
    const { data } = await api(koios).getVotingPower(DREP);
    expect(data).toEqual([{ amount: '1824524915507', basis: 'active' }]);
    expect(koios.callsTo('drep_voting_power_history')).toHaveLength(0);
  });

  it('reports the predefined options as a delegation target, not a credential', async () => {
    const koios = new FakeKoios().on('drep_info', [
      {
        ...INFO,
        drep_id: 'drep_always_abstain',
        hex: null,
        amount: '10302233558851005',
      },
    ]);
    const { data } = await api(koios).getVotingPowers(['drep_always_abstain']);
    expect(data[0]!.subject).toEqual({
      kind: 'predefined',
      option: 'alwaysAbstain',
      view: 'drep_always_abstain',
    });
    expect(data[0]!.votingPower!.amount).toBe('10302233558851005');
  });

  it('lists delegators with the amount but no join time', async () => {
    const koios = new FakeKoios().on('drep_delegators', [
      {
        stake_address: 'stake1ux39',
        stake_address_hex: 'e1a2',
        script_hash: null,
        epoch_no: 656,
        amount: '7841510288',
      },
    ]);
    const { data } = await api(koios).listDelegators(DREP);
    expect(data.elements[0]).toEqual({
      stakeAddress: 'stake1ux39',
      basis: 'active',
      balance: { total: '7841510288' },
      since: null,
      txRef: null,
    });
  });

  it('refuses the delegation event stream it cannot build', async () => {
    const koios = new FakeKoios();
    const error = await expectChainDataError(
      api(koios).listDelegationEvents(DREP),
    );
    expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
  });

  it('maps registration history to the contract event types', async () => {
    const koios = new FakeKoios().on('drep_updates', [
      {
        drep_id: DREP,
        update_tx_hash: 'aa',
        cert_index: 0,
        block_time: 1782150991,
        action: 'deregistered',
        deposit: null,
        meta_url: null,
        meta_hash: null,
        meta_json: null,
      },
      {
        drep_id: DREP,
        update_tx_hash: 'bb',
        cert_index: 0,
        block_time: 1772150991,
        action: 'registered',
        deposit: '500000000',
        meta_url: 'https://example.test/drep.jsonld',
        meta_hash: '297c',
        meta_json: null,
      },
    ]);

    const { data } = await api(koios).listHistory(DREP);
    expect(data.elements.map((event) => event.type)).toEqual([
      'retired',
      'registered',
    ]);
    expect(data.elements[1]!.anchor).toEqual({
      url: 'https://example.test/drep.jsonld',
      dataHash: '297c',
    });
    expect(data.elements[0]!.anchor).toBeNull();
  });
});
