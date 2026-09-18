import { FakeKoios } from './fake-http';
import { expectChainDataError } from './expect-error';
import { KoiosNetworkApi } from '../src/api/network.api';
import { KoiosSystemApi } from '../src/api/system.api';

const TIP = {
  hash: 'f8099b737a1facd9eb844a7584532dc622d6ed4eaadd91b51cc3f5f394c22854',
  epoch_no: 656,
  era: 'Conway',
  abs_slot: 198150523,
  epoch_slot: 121723,
  block_height: 13955911,
  block_no: 13955911,
  block_time: 1789716814,
};

const EPOCH = {
  epoch_no: 656,
  era: 'Conway',
  out_sum: '1',
  fees: '1',
  tx_count: 1,
  blk_count: 1,
  start_time: 1789600000,
  end_time: 1790032000,
  first_block_time: 1789600100,
  last_block_time: 1790031000,
  active_stake: '21500000000000000',
  total_rewards: null,
  avg_blk_reward: null,
};

const BLOCK = {
  hash: 'b7c0d46154a368a65a74dbf53258bf90b80c84230d50e01a25c83b41367cd617',
  epoch_no: 656,
  abs_slot: 198157228,
  epoch_slot: 128428,
  block_height: 13956244,
  block_size: 1316,
  block_time: 1789723519,
  tx_count: 3,
};

function api(koios: FakeKoios): KoiosNetworkApi {
  return new KoiosNetworkApi(koios.client());
}

describe('KoiosNetworkApi', () => {
  it('reports the magic and the era db-sync cannot serve', async () => {
    const koios = new FakeKoios()
      .on('tip', [TIP])
      .on('genesis', [{ networkmagic: '764824073', networkid: 'Mainnet' }])
      .on('epoch_info', [EPOCH]);

    const { data } = await api(koios).getNetworkInfo();
    expect(data.network).toBe('mainnet');
    expect(data.networkMagic).toBe(764824073);
    expect(data.era).toBe('Conway');
    expect(data.tip).toEqual({
      epoch: 656,
      block: 13955911,
      slot: 198150523,
      blockHash: TIP.hash,
      time: '2026-09-18T07:33:34.000Z',
    });
    expect(data.epoch.startTime).toBe('2026-09-16T23:06:40.000Z');
  });

  it.each([
    ['764824073', 'mainnet'],
    ['1', 'preprod'],
    ['2', 'preview'],
    ['42', 'testnet-42'],
  ])('maps network magic %s to %s', async (magic, expected) => {
    const koios = new FakeKoios()
      .on('tip', [TIP])
      .on('genesis', [{ networkmagic: magic }])
      .on('epoch_info', [EPOCH]);
    expect((await api(koios).getNetworkInfo()).data.network).toBe(expected);
  });

  it('falls back to block_height when the deployment omits block_no', async () => {
    const koios = new FakeKoios()
      .on('tip', [{ ...TIP, block_no: undefined }])
      .on('genesis', [{ networkmagic: '1' }])
      .on('epoch_info', [EPOCH]);
    expect((await api(koios).getNetworkInfo()).data.tip.block).toBe(13955911);
  });

  it('lists historical epochs, which the legacy SQL cannot', async () => {
    const koios = new FakeKoios().on('epoch_info', [
      EPOCH,
      { ...EPOCH, epoch_no: 655 },
    ]);
    const { data } = await api(koios).listEpochs({ limit: 2, before: 657 });

    expect(koios.lastCallTo('epoch_info')!.params).toMatchObject({
      epoch_no: 'lt.657',
      order: 'epoch_no.desc',
      limit: '2',
    });
    expect(data).toHaveLength(2);
    expect(data[0]!.durationSeconds).toBe(432000);
  });

  describe('protocol parameters', () => {
    const PARAMS = {
      epoch_no: 656,
      min_fee_a: 44,
      min_fee_b: 155381,
      key_deposit: '2000000',
      pool_deposit: '500000000',
      coins_per_utxo_size: '4310',
      min_fee_ref_script_cost_per_byte: 15,
      protocol_major: 11,
      protocol_minor: 0,
      gov_action_deposit: '100000000000',
      drep_deposit: '500000000',
      drep_activity: 20,
      gov_action_lifetime: 6,
      committee_min_size: 5,
      committee_max_term_length: 146,
      dvt_motion_no_confidence: 0.67,
      dvt_p_p_gov_group: 0.75,
      pvt_committee_normal: 0.51,
      pvtpp_security_group: 0.51,
    };

    it('serves an arbitrary past epoch', async () => {
      const koios = new FakeKoios().on('epoch_params', [
        { ...PARAMS, epoch_no: 500 },
      ]);
      const { data } = await api(koios).getProtocolParams({ epoch: 500 });
      expect(koios.lastCallTo('epoch_params')!.params['_epoch_no']).toBe('500');
      expect(data.epoch).toBe(500);
    });

    it('maps the governance parameters it can type exactly', async () => {
      const koios = new FakeKoios().on('epoch_params', [PARAMS]);
      const { data } = await api(koios).getProtocolParams();
      expect(data).toMatchObject({
        govActionDeposit: '100000000000',
        drepDeposit: '500000000',
        drepActivity: 20,
        govActionLifetime: 6,
        committeeMinSize: 5,
        coinsPerUtxoByte: '4310',
        protocolVersion: { major: 11, minor: 0 },
      });
    });

    it('omits the thresholds rather than turning 0.67 into a ratio', async () => {
      const koios = new FakeKoios().on('epoch_params', [PARAMS]);
      const { data } = await api(koios).getProtocolParams();
      expect(data.dvt).toBeUndefined();
      expect(data.pvt).toBeUndefined();
      // The floats remain reachable, labelled as the provider's own shape.
      expect(data.raw['dvt_motion_no_confidence']).toBe(0.67);
    });

    it('raises NOT_FOUND for an epoch Koios has no parameters for', async () => {
      const koios = new FakeKoios().on('epoch_params', []);
      const error = await expectChainDataError(
        api(koios).getProtocolParams({ epoch: 99999 }),
      );
      expect(error.code).toBe('NOT_FOUND');
    });
  });

  describe('stake distribution', () => {
    it('assembles the totals from three endpoints', async () => {
      const koios = new FakeKoios()
        .on('epoch_info', [EPOCH])
        .on('drep_epoch_summary', [
          { epoch_no: 656, amount: '15738903638210223', dreps: 866 },
        ])
        .on('drep_info', [
          { drep_id: 'drep_always_abstain', amount: '10302233558851005' },
          {
            drep_id: 'drep_always_no_confidence',
            amount: '139044608085302',
          },
        ]);

      const { data } = await api(koios).getStakeDistribution();
      expect(data).toEqual({
        epoch: 656,
        totalActiveStake: '21500000000000000',
        totalStakeControlledByDReps: '15738903638210223',
        totalStakeControlledBySPOs: '21500000000000000',
        alwaysAbstainVotingPower: '10302233558851005',
        alwaysNoConfidenceVotingPower: '139044608085302',
      });
      expect(data.totalLiveStake).toBeUndefined();
    });

    it('refuses a live basis it has no snapshot-free source for', async () => {
      const koios = new FakeKoios();
      const error = await expectChainDataError(
        api(koios).getStakeDistribution({ basis: 'live' }),
      );
      expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
    });
  });

  it('computes the treasury delta from the adjacent epoch row', async () => {
    const koios = new FakeKoios().on('totals', [
      {
        epoch_no: 656,
        treasury: '1500',
        reserves: '90',
        circulation: '1',
        reward: '1',
        supply: '1',
        fees: '1',
      },
      {
        epoch_no: 655,
        treasury: '1000',
        reserves: '95',
        circulation: '1',
        reward: '1',
        supply: '1',
        fees: '1',
      },
    ]);
    const { data } = await api(koios).getTreasury();
    expect(data).toEqual({
      epoch: 656,
      balance: '1500',
      reserves: '90',
      delta: '500',
    });
  });
});

describe('listBlocks', () => {
  // Regression: `order=block_height.desc` makes Koios sort the entire block
  // table and the request never returns — >70s against mainnet versus ~1s
  // without it. `/blocks` is already newest-first, so the order must not be
  // sent for an unfiltered listing. Found by cross-checking the capability
  // table against live behaviour; no fixture could have shown it.
  it('does not ask Koios to sort the whole block table', async () => {
    const koios = new FakeKoios().on('blocks', [BLOCK]);
    await api(koios).listBlocks({ limit: 3 });

    const call = koios.callsTo('blocks')[0]!;
    expect(call.params.order).toBeUndefined();
    expect(call.params.limit).toBe('3');
  });

  it('pins to one height when asked, where the order is harmless', async () => {
    const koios = new FakeKoios().on('blocks', [BLOCK]);
    await api(koios).listBlocks({ block: 13956244 });

    const call = koios.callsTo('blocks')[0]!;
    expect(call.params.block_height).toBe('eq.13956244');
  });

  it('maps a block onto the contract summary', async () => {
    const koios = new FakeKoios().on('blocks', [BLOCK]);
    const { data } = await api(koios).listBlocks({ limit: 1 });
    expect(data[0]).toEqual({
      block: 13956244,
      blockHash: BLOCK.hash,
      slot: 198157228,
      epoch: 656,
      time: '2026-09-18T09:25:19.000Z',
      txCount: 3,
    });
  });
});

describe('KoiosSystemApi', () => {
  it('measures tip staleness, which db-sync cannot', async () => {
    const now = Date.parse('2026-09-18T07:34:34.000Z');
    jest.spyOn(Date, 'now').mockReturnValue(now);

    const koios = new FakeKoios().on('tip', [TIP]);
    const { data } = await new KoiosSystemApi(koios.client()).getHealth();

    expect(data[0]).toMatchObject({
      provider: 'koios',
      status: 'healthy',
      secondsSinceLastUpdate: 60,
      stalenessThresholdSeconds: 300,
    });
    jest.restoreAllMocks();
  });

  it('degrades once the tip is older than the threshold', async () => {
    jest
      .spyOn(Date, 'now')
      .mockReturnValue(Date.parse('2026-09-18T07:43:34.000Z'));

    const koios = new FakeKoios().on('tip', [TIP]);
    const { data } = await new KoiosSystemApi(koios.client()).getHealth();
    expect(data[0]!.status).toBe('degraded');
    jest.restoreAllMocks();
  });

  it('reports unavailable rather than throwing when Koios is down', async () => {
    const koios = new FakeKoios().failOn('tip', 503);
    const { data } = await new KoiosSystemApi(koios.client()).getHealth();
    expect(data[0]!.status).toBe('unavailable');
  });
});
