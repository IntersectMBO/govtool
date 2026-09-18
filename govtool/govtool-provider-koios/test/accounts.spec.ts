import { FakeKoios } from './fake-http';
import { KoiosAccountsApi } from '../src/api/accounts.api';
import { expectChainDataError } from './expect-error';

const STAKE = 'stake1u9ya9ajt7gmmdqlr503t2s72duvtmc4kwdck299568x4xjqqd22nl';

/** The live mainnet shape, including the withdrawals-exceed-rewards case. */
const ACCOUNT = {
  stake_address: STAKE,
  status: 'registered',
  delegated_pool: 'pool1qqqqdktl6pq46td0mwut0qn30g7nlue0sete9wxl0hwsq37wrw8',
  delegated_drep: 'drep1y27qjjnz9a7q6zmvpajzqpqylnka6lpkvcm9sgjrdheqt0qq6pchw',
  total_balance: '-98218279141',
  utxo: '1781720859',
  rewards: '38889620049',
  withdrawals: '138889620049',
  rewards_available: '0',
  deposit: '2000000',
  reserves: '0',
  treasury: '0',
  proposal_refund: '100000000000',
};

function api(koios: FakeKoios): KoiosAccountsApi {
  return new KoiosAccountsApi(koios.client());
}

describe('KoiosAccountsApi', () => {
  describe('voting power', () => {
    it("reproduces GovTool's formula rather than echoing total_balance", async () => {
      const koios = new FakeKoios().on('account_info', [ACCOUNT]);
      const { data } = await api(koios).getVotingPower(STAKE);

      // utxo + rewards + rewardsRest, with no withdrawal deduction: this real
      // account's rewards (38889620049) plus its rewardsRest (100000000000 of
      // proposal refund) come to exactly its withdrawals, and the SQL's guard
      // is `>`, not `>=`.
      expect(data).toEqual({ amount: '140671340908', basis: 'live' });
      // Koios' own total_balance is negative here; it must not leak out.
      expect(ACCOUNT.total_balance.startsWith('-')).toBe(true);
      expect(data!.amount).not.toBe(ACCOUNT.total_balance);
    });

    it('does not deduct when rewards exactly equal withdrawals', async () => {
      const koios = new FakeKoios().on('account_info', [
        {
          ...ACCOUNT,
          utxo: '1000',
          rewards: '500',
          reserves: '0',
          treasury: '0',
          proposal_refund: '0',
          withdrawals: '500',
        },
      ]);
      const { data } = await api(koios).getVotingPower(STAKE);
      expect(data).toEqual({ amount: '1500', basis: 'live' });
    });

    it('skips the withdrawal deduction when rewards do not cover it', async () => {
      const koios = new FakeKoios().on('account_info', [
        {
          ...ACCOUNT,
          utxo: '1000',
          rewards: '10',
          reserves: '0',
          treasury: '0',
          proposal_refund: '0',
          withdrawals: '500',
        },
      ]);
      const { data } = await api(koios).getVotingPower(STAKE);
      expect(data).toEqual({ amount: '1010', basis: 'live' });
    });

    it('subtracts withdrawals once the reward total exceeds them', async () => {
      const koios = new FakeKoios().on('account_info', [
        {
          ...ACCOUNT,
          utxo: '1000',
          rewards: '400',
          reserves: '200',
          treasury: '0',
          proposal_refund: '0',
          withdrawals: '500',
        },
      ]);
      const { data } = await api(koios).getVotingPower(STAKE);
      expect(data).toEqual({ amount: '1100', basis: 'live' });
    });

    it('refuses a historical epoch rather than answering with the tip', async () => {
      const koios = new FakeKoios().on('account_info', [ACCOUNT]);
      const error = await expectChainDataError(
        api(koios).getVotingPower(STAKE, { epoch: 500 }),
      );
      expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
    });
  });

  it('splits the balance into the components a wallet does not show', async () => {
    const koios = new FakeKoios().on('account_info', [ACCOUNT]);
    const { data } = await api(koios).get(STAKE, { expand: ['balance'] });

    expect(data.balance).toEqual({
      // Recomputed, not Koios' `total_balance`. This fixture is a real
      // mainnet account: Koios reports total_balance = -98218279141 because
      // it leaves the 100,000 ada proposal refund out of the credit side
      // while still subtracting the full withdrawals. See `mapBalance`.
      total: '140671340908',
      utxo: '1781720859',
      rewards: '0',
      rewardsRest: '100000000000',
    });
  });

  it('never reports a negative balance, whatever Koios totals it as', async () => {
    const koios = new FakeKoios().on('account_info', [ACCOUNT]);
    const { data } = await api(koios).get(STAKE, {
      expand: ['balance', 'votingPower'],
    });

    expect(BigInt(data.balance!.total) >= 0n).toBe(true);
    // The contract defines `total` as the sum that counts toward voting
    // power, so the two must agree rather than being computed differently.
    expect(data.balance!.total).toBe(data.votingPower!.amount);
  });

  it('reads the deployment spelling and the spec spelling of proposal_refund', async () => {
    const koios = new FakeKoios().on('account_info', [
      {
        ...ACCOUNT,
        proposal_refund: undefined,
        'proposal-refund': '7',
        reserves: '0',
        treasury: '0',
      },
    ]);
    const { data } = await api(koios).get(STAKE, { expand: ['balance'] });
    expect(data.balance!.rewardsRest).toBe('7');
  });

  it('resolves the predefined delegation targets by name', async () => {
    const koios = new FakeKoios().on('account_info', [
      { ...ACCOUNT, delegated_drep: 'drep_always_abstain' },
    ]);
    const { data } = await api(koios).getDelegation(STAKE);
    expect(data!.target).toEqual({
      kind: 'predefined',
      option: 'alwaysAbstain',
    });
  });

  it('reports a stake key with no delegation as null, not as an error', async () => {
    const koios = new FakeKoios().on('account_info', [
      { ...ACCOUNT, delegated_drep: null },
    ]);
    expect((await api(koios).getDelegation(STAKE)).data).toBeNull();
  });

  it('rejects a raw hash, because Koios binds only the bech32 form', async () => {
    const koios = new FakeKoios().on('account_info', [ACCOUNT]);
    const error = await expectChainDataError(
      api(koios).getDelegation(
        'a2510c659a5a99595404b38c6b51da45e34121ac67633bdb2f60b1f7',
      ),
    );
    expect(error.code).toBe('INVALID_INPUT');
  });

  it('raises NOT_FOUND when Koios has never seen the account', async () => {
    const koios = new FakeKoios().on('account_info', []);
    const error = await expectChainDataError(api(koios).get(STAKE));
    expect(error.code).toBe('NOT_FOUND');
  });

  describe('delegation history', () => {
    const UPDATES = [
      {
        stake_address: STAKE,
        updates: [
          {
            action_type: 'delegation_drep',
            tx_hash: 'aa',
            epoch_no: 500,
            epoch_slot: 1,
            absolute_slot: 10,
            block_time: 1700000000,
          },
          {
            action_type: 'delegation_drep',
            tx_hash: 'bb',
            epoch_no: 510,
            epoch_slot: 1,
            absolute_slot: 20,
            block_time: 1710000000,
          },
          {
            action_type: 'registration',
            tx_hash: 'cc',
            epoch_no: 490,
            epoch_slot: 1,
            absolute_slot: 5,
            block_time: 1690000000,
          },
        ],
      },
    ];

    const DREP_A = 'drep1y27qjjnz9a7q6zmvpajzqpqylnka6lpkvcm9sgjrdheqt0qq6pchw';

    it('resolves each event target from one batched tx_info', async () => {
      const koios = new FakeKoios()
        .on('account_updates', UPDATES)
        .on('tx_info', [
          {
            tx_hash: 'aa',
            certificates: [
              {
                index: 0,
                type: 'delegation_drep',
                info: { stake_address: STAKE, drep_id: DREP_A },
              },
            ],
          },
          {
            tx_hash: 'bb',
            certificates: [
              {
                index: 0,
                type: 'delegation_drep',
                info: {
                  stake_address: STAKE,
                  drep_id: 'drep_always_no_confidence',
                },
              },
            ],
          },
        ]);

      const { data } = await api(koios).listDelegationHistory(STAKE);

      expect(koios.callsTo('tx_info')).toHaveLength(1);
      expect(koios.lastCallTo('tx_info')!.body).toMatchObject({
        _tx_hashes: ['aa', 'bb'],
        _certs: true,
      });

      // Newest first, and each event's `from` is the previous target.
      expect(data.elements).toHaveLength(2);
      expect(data.elements[0]!.to).toEqual({
        kind: 'predefined',
        option: 'alwaysNoConfidence',
      });
      expect(data.elements[0]!.from).toMatchObject({ kind: 'drep' });
      expect(data.elements[1]!.from).toEqual({ kind: 'none' });
    });

    it('ignores certificates belonging to another account in the same tx', async () => {
      const koios = new FakeKoios()
        .on('account_updates', UPDATES)
        .on('tx_info', [
          {
            tx_hash: 'aa',
            certificates: [
              {
                index: 0,
                type: 'delegation_drep',
                info: { stake_address: 'stake1other', drep_id: DREP_A },
              },
            ],
          },
        ]);
      const { data } = await api(koios).listDelegationHistory(STAKE);
      expect(data.elements).toHaveLength(0);
    });
  });

  it('reports stake events without a block height rather than inventing one', async () => {
    const koios = new FakeKoios().on('account_updates', [
      {
        stake_address: STAKE,
        updates: [
          {
            action_type: 'registration',
            tx_hash: 'cc',
            epoch_no: 490,
            epoch_slot: 1,
            absolute_slot: 5,
            block_time: 1690000000,
          },
          {
            action_type: 'withdrawal',
            tx_hash: 'dd',
            epoch_no: 491,
            epoch_slot: 1,
            absolute_slot: 6,
            block_time: 1690000100,
          },
        ],
      },
    ]);

    const { data } = await api(koios).listStakeEvents(STAKE);
    expect(data.elements).toHaveLength(1);
    expect(data.elements[0]).toMatchObject({ action: 'registered', slot: 5 });
    expect(data.elements[0]).not.toHaveProperty('block');
  });
});
