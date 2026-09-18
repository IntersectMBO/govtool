import { FakeKoios } from './fake-http';
import { expectChainDataError } from './expect-error';
import { KoiosCommitteeApi } from '../src/api/governance/committee.api';

const COMMITTEE = {
  proposal_id:
    'gov_action1w2w64uhelz0cg2np7m37hal905tdd7jpzm3fcyc3g7qvkwgfppgqqfsggt5',
  proposal_tx_hash:
    '729daaf2f9f89f842a61f6e3ebf7e57d16d6fa4116e29c13114780cb39090850',
  proposal_index: 0,
  quorum_numerator: 2,
  quorum_denominator: 3,
  members: [
    {
      status: 'authorized',
      cc_hot_id: 'cc_hot1qgsl88anw66s9yg9hqzux6yujm3wwgcd8mjdt92pp7qn7nqcsc3nq',
      cc_cold_id:
        'cc_cold1zd7rfcpypwzq98sfxt673kq67s4x8a2aumdrrutwrxcltdq9jkwn2',
      cc_hot_hex: '21f39fb376b5029105b805c3689c96e2e7230d3ee4d595410f813f4c',
      cc_cold_hex: '7c34e0240b84029e0932f5e8d81af42a63f55de6da31f16e19b1f5b4',
      expiration_epoch: 799,
      cc_hot_has_script: false,
      cc_cold_has_script: true,
    },
    {
      status: 'resigned',
      cc_hot_id: null,
      cc_cold_id:
        'cc_cold1zg90nyz8hjgwpkg8x3n4fzse4pggndmxuultspm53g4dxcgjkqykp',
      cc_hot_hex: null,
      cc_cold_hex: '0af99047bc90e0d9073467548a19a85089b766e73eb807748a2ad361',
      expiration_epoch: 799,
      cc_hot_has_script: null,
      cc_cold_has_script: false,
    },
  ],
};

function api(koios: FakeKoios): KoiosCommitteeApi {
  return new KoiosCommitteeApi(koios.client());
}

describe('KoiosCommitteeApi', () => {
  it('reads membership and quorum from gov-state, not from enacted actions', async () => {
    const koios = new FakeKoios().on('committee_info', [COMMITTEE]);
    const { data } = await api(koios).getCommittee();

    expect(data.quorum).toEqual({ numerator: 2, denominator: 3 });
    expect(data.enactedBy!.id).toBe(COMMITTEE.proposal_id);
    expect(data.members).toHaveLength(2);
    expect(data.members[0]).toMatchObject({
      role: 'cc',
      id: COMMITTEE.members[0]!.cc_hot_id,
      coldCredential: {
        hash: COMMITTEE.members[0]!.cc_cold_hex,
        isScriptBased: true,
      },
      hotCredential: {
        hash: COMMITTEE.members[0]!.cc_hot_hex,
        isScriptBased: false,
      },
      hasResigned: false,
      termExpiryEpoch: 799,
    });
  });

  it('reports a resigned member with no hot credential', async () => {
    const koios = new FakeKoios().on('committee_info', [COMMITTEE]);
    const { data } = await api(koios).getCommittee();
    expect(data.members[1]).toMatchObject({
      hasResigned: true,
      hotCredential: null,
    });
  });

  it('never claims a term start epoch, which Koios does not record', async () => {
    const koios = new FakeKoios().on('committee_info', [COMMITTEE]);
    const { data } = await api(koios).getCommittee();
    expect(data.members.every((m) => m.termStartEpoch === null)).toBe(true);
  });

  it('encodes a CIP-129 hot id when the deployment sends only hex', async () => {
    const koios = new FakeKoios().on('committee_info', [
      {
        ...COMMITTEE,
        members: [{ ...COMMITTEE.members[0], cc_hot_id: undefined }],
      },
    ]);
    const { data } = await api(koios).getCommittee();
    expect(data.members[0]!.id).toMatch(/^cc_hot1/);
  });

  it('matches a member on either credential', async () => {
    const koios = new FakeKoios().on('committee_info', [COMMITTEE]);
    for (const id of [
      COMMITTEE.members[0]!.cc_hot_id,
      COMMITTEE.members[0]!.cc_cold_id,
      COMMITTEE.members[0]!.cc_cold_hex,
    ]) {
      const { data } = await api(koios).getMember(id!);
      expect(data.coldCredential.hash).toBe(COMMITTEE.members[0]!.cc_cold_hex);
    }
  });

  it('raises NOT_FOUND for a credential not on the committee', async () => {
    const koios = new FakeKoios().on('committee_info', [COMMITTEE]);
    const error = await expectChainDataError(
      api(koios).getMember('cc_hot1xyz'),
    );
    expect(error.code).toBe('NOT_FOUND');
  });

  describe('constitution', () => {
    const ENACTED = {
      block_time: 1700000000,
      proposal_id:
        'gov_action133jnaewfsq8x6v08ndd87l2yqryp63r30t2dkceacxx5cply5n7sqzlcyqf',
      proposal_tx_hash:
        '8c653ee5c9800e6d31e79b5a7f7d4400c81d44717ad4db633dc18d4c07e4a4fd',
      proposal_index: 0,
      proposal_type: 'NewConstitution',
      proposal_description: {
        tag: 'NewConstitution',
        contents: [
          null,
          {
            anchor: {
              url: 'ipfs://bafkreieyuknozbtewyurfqoagvplvykadn6a4u6wglupavdz46bbsnnl6e',
              dataHash:
                'b368bdad83c727bbfe86425575233fb914eb76d05d89497f7790cf007fd95f52',
            },
            script: 'fa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a64',
          },
        ],
      },
      previous_gov_action_proposal_id: null,
      deposit: '100000000000',
      return_address: 'stake1abc',
      proposed_epoch: 500,
      ratified_epoch: 505,
      enacted_epoch: 506,
      dropped_epoch: null,
      expired_epoch: null,
      expiration: 510,
      meta_url: null,
      meta_hash: null,
      meta_json: null,
      meta_comment: null,
      meta_language: null,
      meta_is_valid: null,
      withdrawal: [],
      param_proposal: null,
    };

    it('reconstructs it from the latest enacted NewConstitution action', async () => {
      const koios = new FakeKoios().on('proposal_list', [ENACTED]);
      const { data } = await api(koios).getConstitution();

      expect(koios.lastCallTo('proposal_list')!.params).toMatchObject({
        proposal_type: 'eq.NewConstitution',
        enacted_epoch: 'not.is.null',
        order: 'enacted_epoch.desc',
      });
      expect(data.anchor.url).toBe(
        'ipfs://bafkreieyuknozbtewyurfqoagvplvykadn6a4u6wglupavdz46bbsnnl6e',
      );
      expect(data.guardrailsScriptHash).toBe(
        'fa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a64',
      );
      expect(data.enactedAt).toEqual({ epoch: 506 });
    });

    it('leaves the document unresolved: the anchor is on chain, the text is not', async () => {
      const koios = new FakeKoios().on('proposal_list', [ENACTED]);
      const { data } = await api(koios).getConstitution();
      expect(data.document).toMatchObject({ status: 'pending' });
      expect(data.document!.body).toBeUndefined();
    });

    it('raises NOT_FOUND on a network with no enacted constitution', async () => {
      const koios = new FakeKoios().on('proposal_list', []);
      const error = await expectChainDataError(api(koios).getConstitution());
      expect(error.code).toBe('NOT_FOUND');
    });
  });
});
