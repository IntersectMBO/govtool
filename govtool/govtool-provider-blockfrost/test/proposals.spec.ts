import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { BlockfrostChainDataProvider } from '../src';
import {
  deriveStatus,
  toBlockfrostGovernanceType,
  toContractType,
} from '../src/mappers/proposal.mapper';
import { tallyByRole } from '../src/mappers/vote.mapper';
import { bfEpoch, bfProposal, FakeBlockfrost, PROPOSAL_TX } from './fake-http';

const ID = `${PROPOSAL_TX}#0`;
const BASE = `/governance/proposals/${PROPOSAL_TX}/0`;

function provider(bf: FakeBlockfrost) {
  return new BlockfrostChainDataProvider(bf.client());
}

function epochRoutes(bf: FakeBlockfrost) {
  bf.on('/epochs/latest', bfEpoch(656));
  for (const e of [514, 515, 525, 536, 537, 541, 542, 544, 570, 571, 573]) {
    bf.on(`/epochs/${e}`, bfEpoch(e));
  }
  return bf;
}

describe('governance.proposals.get', () => {
  it('prefers the ledger tag over Blockfrost’s snake_case type name', async () => {
    const bf = epochRoutes(new FakeBlockfrost()).on(
      BASE,
      bfProposal({
        governance_type: 'new_committee',
        governance_description: {
          tag: 'UpdateCommittee',
          contents: [null, [], {}, { numerator: 2, denominator: 3 }],
        },
      }),
    );
    bf.onStatus(`${BASE}/metadata`, 404);

    const { data } = await provider(bf).governance.proposals.get(ID);
    // the ledger name, not `new_committee`
    expect(data.type).toBe('UpdateCommittee');
  });

  it('omits `submitted`, which Blockfrost does not report, but keeps submittedTx', async () => {
    const bf = epochRoutes(new FakeBlockfrost()).on(BASE, bfProposal());
    bf.onStatus(`${BASE}/metadata`, 404);

    const { data } = await provider(bf).governance.proposals.get(ID);
    expect(data.lifecycle.submitted).toBeUndefined();
    expect(data.lifecycle.submittedTx).toEqual({
      txHash: PROPOSAL_TX,
      index: 0,
    });
  });

  it('serves every status, not just live', async () => {
    const cases: [Record<string, unknown>, string][] = [
      [{}, 'live'],
      [{ expired_epoch: 514 }, 'expired'],
      [{ dropped_epoch: 515 }, 'dropped'],
      [{ ratified_epoch: 536 }, 'ratified'],
      [{ ratified_epoch: 536, enacted_epoch: 537 }, 'enacted'],
    ];
    for (const [overrides, expected] of cases) {
      const bf = epochRoutes(new FakeBlockfrost()).on(
        BASE,
        bfProposal(overrides),
      );
      bf.onStatus(`${BASE}/metadata`, 404);
      const { data } = await provider(bf).governance.proposals.get(ID);
      expect(data.lifecycle.status).toBe(expected);
    }
  });

  it('resolves each terminal epoch to a dated stamp', async () => {
    const bf = epochRoutes(new FakeBlockfrost()).on(
      BASE,
      bfProposal({
        ratified_epoch: 570,
        enacted_epoch: 571,
        expiration: 573,
      }),
    );
    bf.onStatus(`${BASE}/metadata`, 404);

    const { data } = await provider(bf).governance.proposals.get(ID);
    expect(data.lifecycle.enactedAt).toEqual({
      epoch: 571,
      time: expect.stringMatching(/^\d{4}-/),
    });
    expect(data.lifecycle.expires?.epoch).toBe(573);
  });

  it('builds a TreasuryWithdrawals body from the bech32 sub-resource and totals with BigInt', async () => {
    const bf = epochRoutes(new FakeBlockfrost())
      .on(
        BASE,
        bfProposal({
          governance_type: 'treasury_withdrawals',
          governance_description: {
            tag: 'TreasuryWithdrawals',
            contents: [[]],
          },
        }),
      )
      .on(`${BASE}/withdrawals`, [
        { stake_address: 'stake17xa', amount: '9007199254740993' },
        { stake_address: 'stake17xb', amount: '1' },
      ])
      .onStatus(`${BASE}/metadata`, 404);

    const { data } = await provider(bf).governance.proposals.get(ID);
    expect(data.body).toEqual({
      type: 'TreasuryWithdrawals',
      withdrawals: [
        { stakeAddress: 'stake17xa', amount: '9007199254740993' },
        { stakeAddress: 'stake17xb', amount: '1' },
      ],
      // above Number.MAX_SAFE_INTEGER; summed exactly
      totalAmount: '9007199254740994',
    });
  });

  it('keeps only the changed parameters for a ParameterChange', async () => {
    const bf = epochRoutes(new FakeBlockfrost())
      .on(
        BASE,
        bfProposal({
          governance_type: 'parameter_change',
          governance_description: {
            tag: 'ParameterChange',
            contents: [null, {}],
          },
        }),
      )
      .on(`${BASE}/parameters`, {
        tx_hash: PROPOSAL_TX,
        cert_index: 0,
        // Blockfrost returns every parameter, null for the untouched ones
        parameters: { epoch: null, min_fee_a: null, drep_deposit: 600000000 },
      })
      .onStatus(`${BASE}/metadata`, 404);

    const { data } = await provider(bf).governance.proposals.get(ID);
    expect(data.body).toEqual({
      type: 'ParameterChange',
      changes: { drep_deposit: 600000000 },
    });
  });

  it('builds an UpdateCommittee body with the quorum as an exact ratio', async () => {
    const bf = epochRoutes(new FakeBlockfrost())
      .on(
        BASE,
        bfProposal({
          governance_type: 'new_committee',
          governance_description: {
            tag: 'UpdateCommittee',
            contents: [
              null,
              [{ scriptHash: 'b601' }],
              { 'keyHash-1349': 653, 'scriptHash-1980': 720 },
              { numerator: 2, denominator: 3 },
            ],
          },
        }),
      )
      .onStatus(`${BASE}/metadata`, 404);

    const { data } = await provider(bf).governance.proposals.get(ID);
    expect(data.body).toEqual({
      type: 'UpdateCommittee',
      quorum: { numerator: 2, denominator: 3 },
      removed: [{ coldCredential: 'b601', isScriptBased: true }],
      added: [
        { coldCredential: '1349', isScriptBased: false, termExpiryEpoch: 653 },
        { coldCredential: '1980', isScriptBased: true, termExpiryEpoch: 720 },
      ],
    });
  });

  it('reports rawBody but no typed body when the description is not the expected shape', async () => {
    const bf = epochRoutes(new FakeBlockfrost())
      .on(
        BASE,
        bfProposal({
          governance_type: 'hard_fork_initiation',
          governance_description: {
            tag: 'HardForkInitiation',
            contents: ['odd'],
          },
        }),
      )
      .onStatus(`${BASE}/metadata`, 404);

    const { data } = await provider(bf).governance.proposals.get(ID);
    expect(data.body).toBeUndefined();
    expect(data.rawBody).toEqual({
      tag: 'HardForkInitiation',
      contents: ['odd'],
    });
  });

  it('accepts a CIP-129 id and the txHash#index form', async () => {
    const bf = epochRoutes(new FakeBlockfrost()).on(BASE, bfProposal());
    bf.onStatus(`${BASE}/metadata`, 404);
    const api = provider(bf).governance.proposals;

    const byLegacy = await api.get(ID);
    await api.get(byLegacy.data.id);
    expect(bf.callsTo(BASE)).toHaveLength(2);
  });
});

describe('governance.proposals votes and tallies', () => {
  const votes = [
    {
      tx_hash: 'v1',
      cert_index: 0,
      voter_role: 'spo',
      voter: 'pool1abc',
      vote: 'yes',
    },
    {
      tx_hash: 'v2',
      cert_index: 1,
      voter_role: 'drep',
      voter: 'drep1yfkg5ryqwaleunwqg0dtcj8g8yw8sgd9kskg2zlzdr8355gpghcvv',
      vote: 'no',
    },
    {
      tx_hash: 'v3',
      cert_index: 0,
      voter_role: 'constitutional_committee',
      voter: '07e0eb70a1cfd5de084b5fcc8a9b28ff7772282b57e760d692c75bde',
      vote: 'abstain',
    },
  ];

  it('returns each vote with its voter identity, and no fabricated timestamp', async () => {
    const bf = new FakeBlockfrost().on(`${BASE}/votes`, votes);
    const { data } = await provider(bf).governance.proposals.listVotes(ID, {
      limit: 100,
    });

    expect(data.elements).toHaveLength(3);
    const drepVote = data.elements.find((v) => v.voter.role === 'drep')!;
    expect(drepVote.voter.hash).toHaveLength(56);
    expect(drepVote.voter.cip105Id).toMatch(/^drep1/);
    expect(drepVote.vote).toBe('no');
    expect(drepVote.txRef).toEqual({ txHash: 'v2', index: 1 });
    // Blockfrost dates a vote only via its transaction, so `at` is absent
    expect(drepVote.at).toBeUndefined();
    expect(drepVote.votingPower).toBeNull();
  });

  it('maps constitutional_committee to the contract role, from a raw hex voter', async () => {
    const bf = new FakeBlockfrost().on(`${BASE}/votes`, votes);
    const { data } = await provider(bf).governance.proposals.listVotes(ID, {
      limit: 100,
    });
    const cc = data.elements.find((v) => v.voter.role === 'cc')!;
    // the committee voter is bare hex, not bech32
    expect(cc.voter.hash).toBe(
      '07e0eb70a1cfd5de084b5fcc8a9b28ff7772282b57e760d692c75bde',
    );
  });

  it('tallies by headcount only, never inventing stake', async () => {
    const bf = new FakeBlockfrost().on(`${BASE}/votes`, votes);
    const { data } = await provider(bf).governance.proposals.getTallies(ID);

    expect(data).toEqual(
      expect.arrayContaining([
        { role: 'spo', count: { yes: 1, no: 0, abstain: 0 } },
        { role: 'drep', count: { yes: 0, no: 1, abstain: 0 } },
        { role: 'cc', count: { yes: 0, no: 0, abstain: 1 } },
      ]),
    );
    for (const tally of data) {
      // no stake, and no threshold computed from counts
      expect(tally.stake).toBeUndefined();
      expect(tally.threshold).toBeUndefined();
      expect(tally.passing).toBeUndefined();
    }
  });

  it('rejects a vote value outside the ledger enum', () => {
    expect(() => tallyByRole([{ ...votes[0]!, vote: 'Yes' }])).toThrow(
      /Unexpected vote value/,
    );
  });

  it('rejects an unknown voter_role rather than guessing', () => {
    expect(() =>
      tallyByRole([{ ...votes[0]!, voter_role: 'martian' }]),
    ).toThrow(/Unexpected voter_role/);
  });
});

describe('governance.proposals.getEnacted', () => {
  it('scans the directory for candidates of the type and returns the enacted one', async () => {
    const other = 'a'.repeat(64);
    const bf = epochRoutes(new FakeBlockfrost())
      .on('/governance/proposals', [
        { tx_hash: other, cert_index: 0, governance_type: 'info_action' },
        {
          tx_hash: PROPOSAL_TX,
          cert_index: 0,
          governance_type: 'hard_fork_initiation',
        },
      ])
      .on(
        BASE,
        bfProposal({
          governance_type: 'hard_fork_initiation',
          ratified_epoch: 536,
          enacted_epoch: 537,
        }),
      );

    const { data } =
      await provider(bf).governance.proposals.getEnacted('HardForkInitiation');
    expect(data?.action.txHash).toBe(PROPOSAL_TX);
    expect(data?.enactedAt?.epoch).toBe(537);
    // the non-matching type was filtered out before hydration
    expect(bf.paths()).not.toContain(`/governance/proposals/${other}/0`);
  });

  it('returns null when no action of that type has been enacted', async () => {
    const bf = epochRoutes(new FakeBlockfrost())
      .on('/governance/proposals', [
        { tx_hash: PROPOSAL_TX, cert_index: 0, governance_type: 'info_action' },
      ])
      .on(BASE, bfProposal());
    await expect(
      provider(bf).governance.proposals.getEnacted('InfoAction'),
    ).resolves.toMatchObject({ data: null });
  });
});

describe('type-name translation', () => {
  it('round-trips between the ledger and Blockfrost spellings', () => {
    expect(
      toContractType(
        bfProposal({
          governance_description: null,
          governance_type: 'new_committee',
        }),
      ),
    ).toBe('UpdateCommittee');
    expect(
      toContractType(
        bfProposal({
          governance_description: null,
          governance_type: 'whatever',
        }),
      ),
    ).toBe('InfoAction');
    expect(toBlockfrostGovernanceType('UpdateCommittee')).toBe('new_committee');
    expect(toBlockfrostGovernanceType('TreasuryWithdrawals')).toBe(
      'treasury_withdrawals',
    );
  });

  it('derives status with the ledger’s precedence', () => {
    expect(
      deriveStatus(bfProposal({ ratified_epoch: 1, enacted_epoch: 2 })),
    ).toBe('enacted');
    expect(deriveStatus(bfProposal({ ratified_epoch: 1 }))).toBe('ratified');
    expect(deriveStatus(bfProposal({ dropped_epoch: 1 }))).toBe('dropped');
    expect(deriveStatus(bfProposal({ expired_epoch: 1 }))).toBe('expired');
    expect(deriveStatus(bfProposal())).toBe('live');
  });
});

describe('routes with no Blockfrost resource', () => {
  it.each([
    [
      'transactions.get',
      (p: BlockfrostChainDataProvider) => p.transactions.get('00'.repeat(32)),
    ],
    [
      'governance.metrics.get',
      (p: BlockfrostChainDataProvider) => p.governance.metrics.get(),
    ],
    [
      'governance.committee.getCommittee',
      (p: BlockfrostChainDataProvider) => p.governance.committee.getCommittee(),
    ],
    [
      'governance.committee.getConstitution',
      (p: BlockfrostChainDataProvider) =>
        p.governance.committee.getConstitution(),
    ],
    [
      'governance.votes.list',
      (p: BlockfrostChainDataProvider) => p.governance.votes.list(),
    ],
    [
      'governance.pools.list',
      (p: BlockfrostChainDataProvider) => p.governance.pools.list(),
    ],
    [
      'network.getTreasury',
      (p: BlockfrostChainDataProvider) => p.network.getTreasury(),
    ],
    [
      'accounts.getVotingPower',
      (p: BlockfrostChainDataProvider) => p.accounts.getVotingPower('stake1u'),
    ],
    [
      'proposals.listActivity',
      (p: BlockfrostChainDataProvider) =>
        p.governance.proposals.listActivity(ID),
    ],
  ])('declares %s unsupported, with a reason', async (_name, call) => {
    const p = provider(new FakeBlockfrost());
    const error = (await call(p)
      .then(() => null)
      .catch((e: unknown) => e)) as { code?: string };
    expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
  });

  it('omits the surveys namespace entirely', () => {
    // `surveys` is optional on the contract; CIP-179 definitions are
    // transaction metadata and /txs is unavailable here, so the namespace is
    // absent rather than present-and-always-failing.
    const api: ChainDataApiV1 = provider(new FakeBlockfrost());
    expect(api.surveys).toBeUndefined();
  });
});
