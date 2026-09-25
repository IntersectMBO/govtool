/**
 * Wire-compatibility tests.
 *
 * The backend's contract with the world is the legacy GovTool API — the shape
 * the Haskell backend served and the frontend still reads. These specs drive
 * each service with a stubbed `ChainDataApiV1` and assert the response body
 * exactly, key for key, so a change in the data layer that would alter the
 * wire format fails here rather than in the browser.
 *
 * `toEqual` is deliberate throughout: an extra or missing key is a break.
 *
 * Where a legacy field's SOURCE is gone from the contract — resolved metadata,
 * db-sync row ids, a transaction's voting procedures — the assertion pins the
 * field to null or empty rather than being dropped. The shape is the promise;
 * what can fill it is the provider's business.
 */
import type {
  Account,
  ChainDataApiV1,
  DRep,
  DRepListQuery,
  DRepSort,
  DRepVoteRow,
  Delegation,
  Envelope,
  GovAction,
  ProviderCapabilities,
  PagedEnvelope,
  PredefinedDelegation,
  ProtocolParams,
  Ratio,
  VotingPower,
} from '@govtool/data-providers/chain-data';
import { ChainDataError } from '@govtool/data-providers/chain-data';

import { AccountService } from '../src/account/account.service';
import { AdaHolderService } from '../src/ada-holder/ada-holder.service';
import { CacheService } from '../src/cache/cache.service';
import { ConfigService } from '../src/config/config.service';
import { DRepService } from '../src/drep/drep.service';
import { EpochService } from '../src/epoch/epoch.service';
import { NetworkService } from '../src/network/network.service';
import { ProposalService } from '../src/proposal/proposal.service';
import { TransactionService } from '../src/transaction/transaction.service';
import { drepIdToCip105, legacyStakeAddress } from '../src/common/legacy-ids';
import { LegacyNetwork } from '../src/common/legacy-network';
import { actionId, drepId, numberedDRepId } from './ids';

const HASH = 'a'.repeat(56);
const TX = 'd'.repeat(64);
/** CIP-129 key-hash id of HASH: what the contract carries. */
const DREP_ID = drepId('a');
/** Its CIP-105 rendering: the legacy `view`. */
const DREP_VIEW = drepIdToCip105(DREP_ID);
const ACTION_ID = actionId(TX, 0);
/** The legacy reward address the frontend sends: header e0 (testnet key) + HASH. */
const STAKE_KEY = `e0${HASH}`;

const META = { provider: 'stub', network: 'preview' } as const;

/** Every stubbed read answers in an envelope; only `data` differs. */
function env<T>(data: T): Envelope<T> {
  return { data, meta: { ...META } };
}

function page<T>(elements: T[]): PagedEnvelope<T> {
  return env({ elements, total: elements.length });
}

/** A cache that never caches, so each test sees the stub it set up. */
function passthroughCache(): CacheService {
  const config = {
    get: () => ({
      cacheDurationSeconds: 0,
      drepListCacheDurationSeconds: 0,
      cacheMaxEntries: 1_000,
    }),
  } as unknown as ConfigService;
  return new CacheService(config);
}

/**
 * A partial provider for these tests.
 *
 * Deliberately **not** `as unknown as ChainDataApiV1`. That cast switched the
 * type checker off at the one boundary it was most needed: a stub could
 * return `{ elements: [...] }` with no `total`, which is not a valid `Page`,
 * and nothing complained. Every stub then modelled db-sync — one complete
 * page, always — so the suite could not see that Koios caps a page at 1,000
 * rows and Blockfrost at 25. That is how the snapshot-truncation bug reached
 * live code with 35 specs passing.
 *
 * Each namespace is `Partial<…>`, so a test still stubs only the methods it
 * needs, but every stubbed method must match the contract's real signature —
 * including its return shape.
 */
type StubApi = {
  network?: Partial<ChainDataApiV1['network']>;
  accounts?: Partial<ChainDataApiV1['accounts']>;
  transactions?: Partial<ChainDataApiV1['transactions']>;
  system?: Partial<ChainDataApiV1['system']>;
  governance?: {
    dreps?: Partial<ChainDataApiV1['governance']['dreps']>;
    proposals?: Partial<ChainDataApiV1['governance']['proposals']>;
    pools?: Partial<ChainDataApiV1['governance']['pools']>;
    committee?: Partial<ChainDataApiV1['governance']['committee']>;
  };
};

function chain(overrides: StubApi): ChainDataApiV1 {
  return overrides as ChainDataApiV1;
}

beforeEach(() => {
  jest.restoreAllMocks();
});

describe('GET /account/:stakeKey', () => {
  it('returns the legacy body, with a null row id', async () => {
    const account: Account = {
      stakeAddress: 'stake_test1abc',
      stakeKeyHash: HASH,
      isRegistered: true,
      isScriptBased: false,
    };
    const service = new AccountService(
      chain({ accounts: { get: () => Promise.resolve(env(account)) } }),
      passthroughCache(),
    );

    await expect(service.getAccountInfo(STAKE_KEY)).resolves.toEqual({
      // db-sync's internal row id; no provider carries one now.
      id: null,
      view: 'stake_test1abc',
      isRegistered: true,
      isScriptBased: false,
    });
  });

  it('reports isScriptBased as false when the provider omits it', async () => {
    const service = new AccountService(
      chain({
        accounts: {
          get: () =>
            Promise.resolve(
              env({
                stakeAddress: 'stake_test1abc',
                stakeKeyHash: HASH,
                isRegistered: false,
              }),
            ),
        },
      }),
      passthroughCache(),
    );

    await expect(service.getAccountInfo(STAKE_KEY)).resolves.toMatchObject({
      isScriptBased: false,
    });
  });
});

describe('GET /ada-holder/get-current-delegation/:stakeKey', () => {
  function service(delegation: Delegation | null): AdaHolderService {
    return new AdaHolderService(
      chain({
        accounts: { getDelegation: () => Promise.resolve(env(delegation)) },
      }),
      passthroughCache(),
    );
  }

  it('reports a real DRep by the legacy hex hash and CIP-105 view', async () => {
    await expect(
      service({
        target: {
          kind: 'drep',
          drep: { role: 'drep', id: drepId('a', true), isScriptBased: true },
        },
        txRef: { txHash: 'c'.repeat(64) },
      }).getCurrentDelegation(STAKE_KEY),
    ).resolves.toEqual({
      // The raw hash, and CIP-105 with the script prefix.
      drepHash: HASH,
      drepView: drepIdToCip105(drepId('a', true)),
      isDRepScriptBased: true,
      txHash: 'c'.repeat(64),
    });
  });

  it.each<[PredefinedDelegation, string]>([
    ['alwaysAbstain', 'drep_always_abstain'],
    ['alwaysNoConfidence', 'drep_always_no_confidence'],
  ])(
    'maps the predefined target %s back to its db-sync view',
    async (target, view) => {
      await expect(
        service({
          target: { kind: 'predefined', target },
          txRef: { txHash: 'c'.repeat(64) },
        }).getCurrentDelegation(STAKE_KEY),
      ).resolves.toEqual({
        drepHash: null,
        drepView: view,
        isDRepScriptBased: false,
        txHash: 'c'.repeat(64),
      });
    },
  );

  it('returns null when nothing is delegated', async () => {
    await expect(
      service(null).getCurrentDelegation(STAKE_KEY),
    ).resolves.toBeNull();
  });
});

describe('GET /ada-holder/get-voting-power/:stakeKey', () => {
  function service(
    result: () => Promise<Envelope<VotingPower | null>>,
  ): AdaHolderService {
    return new AdaHolderService(
      chain({ accounts: { getVotingPower: result } }),
      passthroughCache(),
    );
  }

  it('returns a bare number', async () => {
    await expect(
      service(() =>
        Promise.resolve(env({ amount: '900000000', basis: 'live' as const })),
      ).getVotingPower(STAKE_KEY),
    ).resolves.toBe(900000000);
  });

  it('returns 0 for no record and 0 for a provider failure, as the legacy service did', async () => {
    await expect(
      service(() => Promise.resolve(env(null))).getVotingPower(STAKE_KEY),
    ).resolves.toBe(0);

    await expect(
      service(() =>
        Promise.reject(new ChainDataError('PROVIDER_UNAVAILABLE', 'down')),
      ).getVotingPower(STAKE_KEY),
    ).resolves.toBe(0);
  });
});

/* ------------------------------------------------------------------------- */
/* Network                                                                    */
/* ------------------------------------------------------------------------- */

const QUORUM: Ratio = { numerator: 2, denominator: 3 };

describe('GET /network/*', () => {
  it('info carries the server clock plus the chain tip', async () => {
    const service = new NetworkService(
      chain({
        network: {
          getNetworkInfo: () =>
            Promise.resolve(
              env({
                network: 'preview',
                era: 'conway',
                tip: { epoch: 500, block: 11_000_000 },
                currentEpoch: 500,
              }),
            ),
        },
      }),
      passthroughCache(),
    );

    const body = await service.getNetworkInfo();
    expect(Object.keys(body).sort()).toEqual([
      'blockNo',
      'currentTime',
      'epochNo',
      'networkName',
    ]);
    expect(body.epochNo).toBe(500);
    expect(body.blockNo).toBe(11_000_000);
    expect(body.networkName).toBe('preview');
    expect(Date.parse(body.currentTime)).not.toBeNaN();
  });

  it('total-stake keeps values above the safe range exact', async () => {
    const service = new NetworkService(
      chain({
        network: {
          getStakeDistribution: () =>
            Promise.resolve(
              env({
                totalActiveStake: '53000000000000000',
                totalStakeControlledByDReps: '31000000000000000',
                totalStakeControlledBySPOs: '22000000000000000',
                alwaysAbstainVotingPower: '4000000000000',
                alwaysNoConfidenceVotingPower: '900000000',
              }),
            ),
        },
      }),
      passthroughCache(),
    );

    await expect(service.getNetworkTotalStake()).resolves.toEqual({
      // Both exceed Number.MAX_SAFE_INTEGER (9.0e15), so they stay bigint and
      // the response interceptor writes them unquoted. Rounding them to a
      // double would move the figure by thousands of ada.
      totalStakeControlledByDReps: 31000000000000000n,
      totalStakeControlledBySPOs: 22000000000000000n,
      alwaysAbstainVotingPower: 4000000000000,
      alwaysNoConfidenceVotingPower: 900000000,
    });
  });

  it('answers 501 rather than 0 when the provider has no DRep breakdown', async () => {
    const service = new NetworkService(
      chain({
        network: {
          getStakeDistribution: () =>
            Promise.resolve(env({ totalActiveStake: '53000000000000000' })),
        },
      }),
      passthroughCache(),
    );

    await expect(service.getNetworkTotalStake()).rejects.toMatchObject({
      status: 501,
    });
  });

  it('metrics returns all thirteen legacy keys, assembled from four resources', async () => {
    const service = new NetworkService(
      chain({
        network: {
          getStakeDistribution: () =>
            Promise.resolve(
              env({
                totalActiveStake: '53000000000000000',
                totalStakeControlledByDReps: '31000000000000000',
              }),
            ),
        },
        governance: {
          dreps: {
            getCounts: () =>
              Promise.resolve(
                env({
                  totalRegistered: 700,
                  totalActive: 400,
                  totalInactive: 300,
                  anonymous: 20,
                }),
              ),
          },
          proposals: {
            list: () => Promise.resolve(env({ elements: [], total: 50 })),
          },
          committee: {
            getCommittee: () =>
              Promise.resolve(
                env({
                  members: Array.from({ length: 7 }, () => committeeMember()),
                  quorum: QUORUM,
                  enactedBy: null,
                }),
              ),
          },
        },
      }),
      passthroughCache(),
    );

    await expect(service.getNetworkMetrics()).resolves.toEqual({
      // The five nothing owns any more. Reported as 0 rather than failing the
      // response: no screen renders them.
      uniqueDelegators: 0,
      totalDelegations: 0,
      totalDRepVotes: 0,
      totalActiveCIP119CompliantDReps: 0,
      totalRegisteredDirectVoters: 0,
      // The eight that moved to the resource that owns them.
      totalGovernanceActions: 50,
      totalRegisteredDReps: 700,
      totalDRepDistr: 31000000000000000n,
      totalActiveDReps: 400,
      totalInactiveDReps: 300,
      noOfCommitteeMembers: 7,
      quorumNumerator: 2,
      quorumDenominator: 3,
    });
  });
});

function committeeMember() {
  return {
    role: 'cc' as const,
    coldCredential: 'cc_cold1abc',
    hotCredential: null,
    termStartEpoch: null,
    termExpiryEpoch: null,
    hasResigned: false,
  };
}

describe('GET /epoch/params', () => {
  it('maps the typed protocol parameters back to the legacy snake_case epoch_param row', async () => {
    const params = protocolParams();
    const service = new EpochService(
      chain({
        network: { getProtocolParams: () => Promise.resolve(env(params)) },
      }),
      passthroughCache(),
    );
    await expect(service.getCurrentEpochParams()).resolves.toEqual({
      // db-sync bookkeeping the contract dropped (D1), and a parameter the
      // ledger no longer has: no source, so null.
      id: null,
      block_id: null,
      cost_model_id: null,
      nonce: null,
      extra_entropy: null,
      // The joined cost model row: the costs the frontend hashes, no row id.
      cost_model: {
        id: null,
        costs: { PlutusV1: [100788, 420, 1], PlutusV3: [100788, 420, 1, 1] },
        hash: null,
      },
      epoch_no: 500,
      protocol_major: 10,
      protocol_minor: 0,
      gov_action_lifetime: 6,
      gov_action_deposit: 100000000000,
      drep_deposit: 500000000,
      drep_activity: 20,
      committee_min_size: 7,
      committee_max_term_length: 146,
      dvt_motion_no_confidence: 0.67,
      dvt_committee_normal: 0.67,
      dvt_committee_no_confidence: 0.67,
      dvt_update_to_constitution: 0.67,
      dvt_hard_fork_initiation: 0.67,
      dvt_p_p_network_group: 0.67,
      dvt_p_p_economic_group: 0.67,
      dvt_p_p_technical_group: 0.67,
      dvt_p_p_gov_group: 0.67,
      dvt_treasury_withdrawal: 0.67,
      pvt_motion_no_confidence: 0.67,
      pvt_committee_normal: 0.67,
      pvt_committee_no_confidence: 0.67,
      pvt_hard_fork_initiation: 0.67,
      pvtpp_security_group: 0.67,
      key_deposit: 2000000,
      pool_deposit: 500000000,
      coins_per_utxo_size: 4310,
      min_fee_a: 44,
      min_fee_b: 155381,
      max_tx_size: 16384,
      max_val_size: 5000,
      max_block_size: 90112,
      max_bh_size: 1100,
      max_epoch: 18,
      optimal_pool_count: 500,
      influence: 0.3,
      monetary_expand_rate: 0.003,
      treasury_growth_rate: 0.2,
      // Removed in Babbage; db-sync has written 0 for both since.
      decentralisation: 0,
      min_utxo_value: 0,
      min_pool_cost: 170000000,
      price_mem: 0.0577,
      price_step: 0.0000721,
      max_tx_ex_mem: 16500000,
      max_tx_ex_steps: 10000000000,
      max_block_ex_mem: 72000000,
      max_block_ex_steps: 20000000000,
      collateral_percent: 150,
      max_collateral_inputs: 3,
      min_fee_ref_script_cost_per_byte: 15,
    });
  });

  it('keeps a deposit above the safe range exact, and reads each threshold from its own ratio', async () => {
    const params = protocolParams();
    params.govActionDeposit = '45000000000000001';
    params.drepThresholds = {
      ...params.drepThresholds,
      ppGovGroup: { numerator: 3, denominator: 4 },
    };
    params.poolThresholds = {
      ...params.poolThresholds,
      ppSecurityGroup: { numerator: 51, denominator: 100 },
    };
    const service = new EpochService(
      chain({
        network: { getProtocolParams: () => Promise.resolve(env(params)) },
      }),
      passthroughCache(),
    );
    const body = await service.getCurrentEpochParams();
    expect(body.gov_action_deposit).toBe(45000000000000001n);
    expect(body.dvt_p_p_gov_group).toBe(0.75);
    expect(body.pvtpp_security_group).toBe(0.51);
  });
});

/** db-sync `param_proposal` columns, plus the joined `cost_model`. */
const PARAM_PROPOSAL_COLUMNS = [
  'id',
  'epoch_no',
  'key',
  'min_fee_a',
  'min_fee_b',
  'max_block_size',
  'max_tx_size',
  'max_bh_size',
  'key_deposit',
  'pool_deposit',
  'max_epoch',
  'optimal_pool_count',
  'influence',
  'monetary_expand_rate',
  'treasury_growth_rate',
  'decentralisation',
  'entropy',
  'protocol_major',
  'protocol_minor',
  'min_utxo_value',
  'min_pool_cost',
  'cost_model_id',
  'price_mem',
  'price_step',
  'max_tx_ex_mem',
  'max_tx_ex_steps',
  'max_block_ex_mem',
  'max_block_ex_steps',
  'max_val_size',
  'collateral_percent',
  'max_collateral_inputs',
  'registered_tx_id',
  'coins_per_utxo_size',
  'pvt_motion_no_confidence',
  'pvt_committee_normal',
  'pvt_committee_no_confidence',
  'pvt_hard_fork_initiation',
  'dvt_motion_no_confidence',
  'dvt_committee_normal',
  'dvt_committee_no_confidence',
  'dvt_update_to_constitution',
  'dvt_hard_fork_initiation',
  'dvt_p_p_network_group',
  'dvt_p_p_economic_group',
  'dvt_p_p_technical_group',
  'dvt_p_p_gov_group',
  'dvt_treasury_withdrawal',
  'committee_min_size',
  'committee_max_term_length',
  'gov_action_lifetime',
  'gov_action_deposit',
  'drep_deposit',
  'drep_activity',
  'pvtpp_security_group',
  'min_fee_ref_script_cost_per_byte',
  'cost_model',
].sort();

function protocolParams(): ProtocolParams {
  const ratio = { numerator: 67, denominator: 100 };
  return {
    epoch: 500,
    protocolVersion: { major: 10, minor: 0 },
    govActionLifetime: 6,
    govActionDeposit: '100000000000',
    drepDeposit: '500000000',
    drepActivity: 20,
    committeeMinSize: 7,
    committeeMaxTermLength: 146,
    drepThresholds: {
      motionNoConfidence: ratio,
      committeeNormal: ratio,
      committeeNoConfidence: ratio,
      updateToConstitution: ratio,
      hardForkInitiation: ratio,
      ppNetworkGroup: ratio,
      ppEconomicGroup: ratio,
      ppTechnicalGroup: ratio,
      ppGovGroup: ratio,
      treasuryWithdrawal: ratio,
    },
    poolThresholds: {
      motionNoConfidence: ratio,
      committeeNormal: ratio,
      committeeNoConfidence: ratio,
      hardForkInitiation: ratio,
      ppSecurityGroup: ratio,
    },
    keyDeposit: '2000000',
    poolDeposit: '500000000',
    coinsPerUtxoByte: '4310',
    minFeeA: 44,
    minFeeB: 155381,
    maxTxSize: 16384,
    maxValSize: 5000,
    minFeeRefScriptCostPerByte: { numerator: 15, denominator: 1 },
    maxBlockBodySize: 90112,
    maxBlockHeaderSize: 1100,
    maxTxExecutionUnits: { memory: 16500000, steps: 10000000000 },
    maxBlockExecutionUnits: { memory: 72000000, steps: 20000000000 },
    collateralPercentage: 150,
    maxCollateralInputs: 3,
    executionUnitPrices: {
      memory: { numerator: 577, denominator: 10000 },
      steps: { numerator: 721, denominator: 10000000 },
    },
    costModels: { PlutusV1: [100788, 420, 1], PlutusV3: [100788, 420, 1, 1] },
    poolRetireMaxEpoch: 18,
    stakePoolTargetNum: 500,
    poolPledgeInfluence: { numerator: 3, denominator: 10 },
    monetaryExpansion: { numerator: 3, denominator: 1000 },
    treasuryCut: { numerator: 1, denominator: 5 },
    minPoolCost: '170000000',
  };
}

describe('GET /transaction/status/:txId', () => {
  it('reports an on-chain transaction as confirmed', async () => {
    const service = new TransactionService(
      chain({
        transactions: {
          get: () => Promise.resolve(env({ txHash: TX, onChain: true })),
        },
      }),
    );
    await expect(service.getTransactionStatus(TX)).resolves.toEqual({
      transactionConfirmed: true,
      // A vote is reachable only through its DRep or its action, so there is
      // no way to ask what a transaction hash voted on.
      votingProcedure: [],
    });
  });

  it('reports an unindexed transaction as unconfirmed', async () => {
    const service = new TransactionService(
      chain({
        transactions: {
          get: () => Promise.resolve(env({ txHash: TX, onChain: false })),
        },
      }),
    );
    await expect(service.getTransactionStatus(TX)).resolves.toEqual({
      transactionConfirmed: false,
      votingProcedure: [],
    });
  });
});

/* ------------------------------------------------------------------------- */
/* DReps                                                                      */
/* ------------------------------------------------------------------------- */

function fullDRep(overrides: Partial<DRep> = {}): DRep {
  return {
    role: 'drep',
    id: DREP_ID,
    isScriptBased: false,
    kind: 'drep',
    anchor: { url: 'https://x/drep.jsonld', dataHash: 'b'.repeat(64) },
    registration: {
      latest: {
        txRef: { txHash: 'c'.repeat(64) },
        at: { epoch: 500, time: '2026-01-01T00:00:00.000Z' },
        deposit: '500000000',
      },
      latestUpdate: null,
    },
    status: 'active',
    votingPower: { amount: '12500000000', basis: 'active' },
    activity: { voted: 7, votable: 9 },
    ...overrides,
  };
}

function capabilitiesWith(dreps: DRepSort[]): ProviderCapabilities {
  return {
    sorts: { dreps, proposals: ['newest', 'oldest'] },
    filters: { dreps: [], proposals: [] },
    search: ['exactId'],
    voteAggregate: ['stake'],
    optionalArguments: [],
  };
}

function drepService(chainStub: StubApi): DRepService {
  const cache = passthroughCache();
  const api = chain({
    ...chainStub,
    system: {
      getCapabilities: () =>
        Promise.resolve(env(capabilitiesWith(['registrationDate', 'random']))),
      ...chainStub.system,
    },
  });
  const proposals = new ProposalService(api, cache, null);
  return new DRepService(api, proposals, cache, null);
}

describe('GET /drep/info/:drepId', () => {
  it('returns all twenty legacy keys, with the four registration booleans', async () => {
    const service = drepService({
      governance: {
        dreps: { get: () => Promise.resolve(env(fullDRep())) },
      },
    });

    await expect(service.getInfo(DREP_ID)).resolves.toEqual({
      isScriptBased: false,
      isRegisteredAsDRep: true,
      wasRegisteredAsDRep: true,
      // Direct voters are not a ledger concept and are gone from the contract.
      isRegisteredAsSoleVoter: false,
      wasRegisteredAsSoleVoter: false,
      deposit: 500000000,
      url: 'https://x/drep.jsonld',
      dataHash: 'b'.repeat(64),
      votingPower: 12500000000,
      dRepRegisterTxHash: 'c'.repeat(64),
      dRepRetireTxHash: null,
      soleVoterRegisterTxHash: null,
      soleVoterRetireTxHash: null,
      // The CIP-119 body: chain data emits the anchor and never resolves it.
      paymentAddress: null,
      givenName: null,
      objectives: null,
      motivations: null,
      qualifications: null,
      imageUrl: null,
      imageHash: null,
    });
  });

  it('reports a retired DRep as no longer registered', async () => {
    const service = drepService({
      governance: {
        dreps: {
          get: () => Promise.resolve(env(fullDRep({ status: 'retired' }))),
        },
      },
    });

    await expect(service.getInfo(DREP_ID)).resolves.toMatchObject({
      isRegisteredAsDRep: false,
      wasRegisteredAsDRep: true,
    });
  });

  it('returns the all-empty record for an unregistered credential', async () => {
    const service = drepService({
      governance: {
        dreps: {
          get: () =>
            Promise.reject(new ChainDataError('NOT_FOUND', 'DRep not found')),
        },
      },
    });

    const body = await service.getInfo(DREP_ID);
    expect(body).toEqual({
      isScriptBased: false,
      isRegisteredAsDRep: false,
      wasRegisteredAsDRep: false,
      isRegisteredAsSoleVoter: false,
      wasRegisteredAsSoleVoter: false,
      deposit: null,
      url: null,
      dataHash: null,
      votingPower: null,
      dRepRegisterTxHash: null,
      dRepRetireTxHash: null,
      soleVoterRegisterTxHash: null,
      soleVoterRetireTxHash: null,
      paymentAddress: null,
      givenName: null,
      objectives: null,
      motivations: null,
      qualifications: null,
      imageUrl: null,
      imageHash: null,
    });
  });

  it('propagates a real failure instead of hiding it as an empty record', async () => {
    const service = drepService({
      governance: {
        dreps: {
          get: () =>
            Promise.reject(new ChainDataError('PROVIDER_UNAVAILABLE', 'down')),
        },
      },
    });
    await expect(service.getInfo(DREP_ID)).rejects.toMatchObject({
      status: 503,
    });
  });
});

describe('GET /drep/list', () => {
  it('returns the legacy page envelope and item shape', async () => {
    const service = drepService({
      governance: {
        dreps: { list: () => Promise.resolve(page([fullDRep()])) },
      },
    });

    const body = await service.list({ status: [], page: 0, pageSize: 10 });
    expect(body.page).toBe(0);
    expect(body.pageSize).toBe(10);
    expect(body.total).toBe(1);
    expect(body.elements[0]).toEqual({
      isScriptBased: false,
      // The legacy forms: raw hex hash and CIP-105.
      drepId: HASH,
      view: DREP_VIEW,
      url: 'https://x/drep.jsonld',
      metadataHash: 'b'.repeat(64),
      deposit: 500000000,
      votingPower: 12500000000,
      status: 'Active',
      type: 'DRep',
      latestTxHash: 'c'.repeat(64),
      latestRegistrationDate: '2026-01-01T00:00:00.000Z',
      // Every field below is metadata-derived, and chain data carries none.
      metadataError: null,
      paymentAddress: null,
      givenName: null,
      objectives: null,
      motivations: null,
      qualifications: null,
      imageUrl: null,
      imageHash: null,
      votesLastYear: 7,
      // `list-dreps.sql` COALESCEs them to `[]` regardless of the anchor, so
      // the legacy field is never null.
      identityReferences: [],
      linkReferences: [],
    });
  });

  it('reports a DRep with no anchor with null url and hash, not undefined', async () => {
    const service = drepService({
      governance: {
        dreps: {
          list: () =>
            Promise.resolve(
              page([fullDRep({ kind: 'anonymous', anchor: null })]),
            ),
        },
      },
    });

    const body = await service.list({
      status: [],
      page: 0,
      pageSize: 10,
      search: DREP_ID,
    });
    expect(body.elements[0]).toMatchObject({
      url: null,
      metadataHash: null,
      identityReferences: [],
      linkReferences: [],
    });
  });

  it('uses the legacy TitleCase status and type values', async () => {
    const service = drepService({
      governance: {
        dreps: {
          list: () =>
            Promise.resolve(
              page([
                fullDRep({ status: 'inactive' }),
                fullDRep({
                  id: drepId('b'),
                  kind: 'anonymous',
                  anchor: null,
                  status: 'retired',
                }),
              ]),
            ),
        },
      },
    });

    // search '' hides anonymous DReps, so ask with a term matching one
    const all = await service.list({
      status: [],
      page: 0,
      pageSize: 10,
      search: drepId('b'),
    });
    // The default ordering is seeded-random, so the pair set is what matters.
    expect(all.elements.map((e) => [e.status, e.type]).sort()).toEqual([
      ['Inactive', 'DRep'],
      ['Retired', 'SoleVoter'],
    ]);
  });

  it('drops rows whose id is not CIP-129, such as the fixture’s predefined targets', async () => {
    const service = drepService({
      governance: {
        dreps: {
          list: () =>
            Promise.resolve(
              page([fullDRep(), fullDRep({ id: 'drep_always_abstain' })]),
            ),
        },
      },
    });
    const body = await service.list({ status: [], page: 0, pageSize: 10 });
    expect(body.elements.map((e) => e.drepId)).toEqual([HASH]);
  });

  it('hides anonymous DReps with no search term, and reveals one on an exact id', async () => {
    const elements = [
      fullDRep(),
      fullDRep({ id: drepId('b'), kind: 'anonymous', anchor: null }),
    ];
    const service = drepService({
      governance: { dreps: { list: () => Promise.resolve(page(elements)) } },
    });

    const bare = await service.list({ status: [], page: 0, pageSize: 10 });
    expect(bare.total).toBe(1);
    expect(bare.elements[0].type).toBe('DRep');

    const exact = await service.list({
      status: [],
      page: 0,
      pageSize: 10,
      search: drepId('b'),
    });
    expect(exact.total).toBe(2);

    // The frontend searches by the raw hex hash; that is an exact id too.
    const byHex = await service.list({
      status: [],
      page: 0,
      pageSize: 10,
      search: 'b'.repeat(56),
    });
    expect(byHex.total).toBe(2);
  });

  it('filters on the legacy status values and pages the result', async () => {
    const elements = Array.from({ length: 5 }, (_, i) =>
      fullDRep({ id: numberedDRepId(i) }),
    );
    const service = drepService({
      governance: { dreps: { list: () => Promise.resolve(page(elements)) } },
    });

    await expect(
      service.list({ status: ['Active'], page: 0, pageSize: 2 }),
    ).resolves.toMatchObject({ total: 5, page: 0, pageSize: 2 });

    // `sort` defaults to 'Random' (seeded), so paging is asserted with an
    // explicit deterministic order instead of insertion order.
    const paged = async (pageNo: number) =>
      (
        await service.list({
          status: [],
          page: pageNo,
          pageSize: 2,
          sort: 'RegistrationDate',
        })
      ).elements.map((e) => e.drepId);

    const [first, second, third] = [
      await paged(0),
      await paged(1),
      await paged(2),
    ];
    expect(second).toHaveLength(2);
    expect(third).toHaveLength(1);
    // the three pages partition the set with no overlap
    expect(new Set([...first, ...second, ...third]).size).toBe(5);

    await expect(
      service.list({ status: ['Retired'], page: 0, pageSize: 10 }),
    ).resolves.toMatchObject({ total: 0 });
  });
});

describe('GET /drep/list against a provider that pages', () => {
  /**
   * The regression the suite could not see before.
   *
   * `/drep/list` is served from a snapshot: fetch the whole directory, then
   * filter, sort and page in memory. Every other stub here returns one
   * complete page, which is db-sync's behaviour — so the suite passed while
   * the backend silently used the first page as the whole set. Koios caps a
   * page at 1,000 rows of 1,684 and Blockfrost at 25; both report the real
   * `total`, and the backend ignored it.
   */
  function pagingProvider(total: number) {
    const calls: DRepListQuery[] = [];
    const list = (q: DRepListQuery): Promise<PagedEnvelope<DRep>> => {
      calls.push(q);
      const offset = (q.page - 1) * q.size;
      const elements = Array.from(
        { length: Math.max(0, Math.min(q.size, total - offset)) },
        (_, i) => fullDRep({ id: numberedDRepId(offset + i) }),
      );
      return Promise.resolve(env({ elements, total }));
    };
    return { calls, list };
  }

  it('pages the whole directory in, instead of taking the first page', async () => {
    const { calls, list } = pagingProvider(1684);
    const service = drepService({ governance: { dreps: { list } } });

    const body = await service.list({ status: [], page: 0, pageSize: 10 });

    // 1,684 DReps, not one page of them — and `total` is the real total.
    expect(body.total).toBe(1684);
    expect(calls.map((call) => call.page)).toEqual([1, 2, 3, 4]);
  });

  it('is unchanged for a provider that returns everything at once', async () => {
    const { calls, list } = pagingProvider(9);
    const service = drepService({ governance: { dreps: { list } } });

    await expect(
      service.list({ status: [], page: 0, pageSize: 10 }),
    ).resolves.toMatchObject({ total: 9 });
    expect(calls).toHaveLength(1);
  });

  it('never asks the provider for the random ordering it cannot page', async () => {
    // A randomly ordered read returns `size` rows and the provider rejects any
    // page beyond the first, so a snapshot read must name a sort.
    const { calls, list } = pagingProvider(9);
    const service = drepService({ governance: { dreps: { list } } });

    await service.list({ status: [], page: 0, pageSize: 10, sort: 'Random' });

    expect(calls).toHaveLength(1);
    expect(calls[0].sort).toBe('registrationDate');
  });

  it('reads the snapshot in a sort the provider declares', async () => {
    // Blockfrost declares votingPower and random, not registrationDate.
    const { calls, list } = pagingProvider(9);
    const service = drepService({
      governance: { dreps: { list } },
      system: {
        getCapabilities: () =>
          Promise.resolve(env(capabilitiesWith(['votingPower', 'random']))),
      },
    });

    await service.list({ status: [], page: 0, pageSize: 10 });

    expect(calls.map((call) => call.sort)).toEqual(['votingPower']);
  });

  it('refuses rather than snapshot one random page', async () => {
    const { calls, list } = pagingProvider(9);
    const service = drepService({
      governance: { dreps: { list } },
      system: {
        getCapabilities: () =>
          Promise.resolve(env(capabilitiesWith(['random']))),
      },
    });

    await expect(
      service.list({ status: [], page: 0, pageSize: 10 }),
    ).rejects.toMatchObject({ status: 501 });
    expect(calls).toHaveLength(0);
  });
});

describe('GET /drep/voting-power-list and /drep/get-voting-power/:drepId', () => {
  it('returns the four legacy keys per entry, in the legacy id forms', async () => {
    const scriptId = drepId('b', true);
    const dreps: Record<string, DRep> = {
      [DREP_ID]: fullDRep({ votingPower: { amount: '5000', basis: 'active' } }),
      [scriptId]: fullDRep({
        id: scriptId,
        isScriptBased: true,
        votingPower: null,
      }),
    };
    const asked: string[] = [];
    const service = drepService({
      governance: {
        dreps: {
          get: (id) => {
            asked.push(id);
            return dreps[id] === undefined
              ? Promise.reject(new ChainDataError('NOT_FOUND', id))
              : Promise.resolve(env(dreps[id]));
          },
        },
      },
    });

    await expect(
      service.getVotingPowerList([
        // What pdf-ui sends: the raw hex hash.
        HASH,
        // A script DRep by its bare hash: found on the script-id fallback.
        'b'.repeat(56),
        // Unknown and malformed ids match no row, as in the legacy statement.
        'c'.repeat(56),
        'not-an-id',
      ]),
    ).resolves.toEqual([
      {
        view: DREP_VIEW,
        // pdf-ui matches this against the hex DRep id its comments store.
        hashRaw: HASH,
        votingPower: 5000,
        // Was the CIP-119 name; no metadata is resolved here.
        givenName: null,
      },
      {
        view: drepIdToCip105(scriptId),
        hashRaw: 'b'.repeat(56),
        votingPower: 0,
        givenName: null,
      },
    ]);
    // Only decodable ids reach the provider, and only in CIP-129 form. The
    // identifiers are read concurrently, so the order is not asserted.
    expect([...asked].sort()).toEqual(
      [DREP_ID, drepId('b'), scriptId, drepId('c'), drepId('c', true)].sort(),
    );
  });

  it('refuses an oversized identifier list rather than fanning it out', async () => {
    const service = drepService({ governance: { dreps: {} } });
    await expect(
      service.getVotingPowerList(Array.from({ length: 1001 }, () => HASH)),
    ).rejects.toMatchObject({ status: 400 });
  });

  it('reads a predefined option off the stake distribution, with a null hash', async () => {
    const service = drepService({
      network: {
        getStakeDistribution: () =>
          Promise.resolve(
            env({
              totalActiveStake: '53000000000000000',
              alwaysNoConfidenceVotingPower: '3707653134137',
            }),
          ),
      },
      governance: { dreps: {} },
    });

    await expect(
      service.getVotingPowerList(['drep_always_no_confidence']),
    ).resolves.toEqual([
      {
        view: 'drep_always_no_confidence',
        // The predefined targets have no credential, exactly as the legacy
        // endpoint reported them.
        hashRaw: null,
        votingPower: 3707653134137,
        givenName: null,
      },
    ]);
  });

  it('returns a bare number, and 0 for a credential the provider does not know', async () => {
    const withPower = drepService({
      governance: {
        dreps: {
          get: () =>
            Promise.resolve(
              env(
                fullDRep({ votingPower: { amount: '777', basis: 'active' } }),
              ),
            ),
        },
      },
    });
    await expect(withPower.getVotingPower(DREP_ID)).resolves.toBe(777);

    const none = drepService({
      governance: {
        dreps: {
          get: () => Promise.reject(new ChainDataError('NOT_FOUND', DREP_ID)),
        },
      },
    });
    await expect(none.getVotingPower(DREP_ID)).resolves.toBe(0);
  });
});

/* ------------------------------------------------------------------------- */
/* Proposals                                                                  */
/* ------------------------------------------------------------------------- */

function govAction(overrides: Partial<GovAction> = {}): GovAction {
  return {
    id: ACTION_ID,
    txHash: TX,
    index: 0,
    type: 'InfoAction',
    body: { type: 'InfoAction' },
    lifecycle: {
      status: 'live',
      submitted: { epoch: 500, time: '2026-01-05T00:00:00.000Z' },
      submittedTx: { txHash: TX, index: 0 },
      expires: { epoch: 510, time: '2026-03-01T00:00:00.000Z' },
      ratifiedAt: null,
      enactedAt: null,
      droppedAt: null,
      expiredAt: null,
    },
    anchor: { url: 'https://x/ga.jsonld', dataHash: 'e'.repeat(64) },
    deposit: '100000000000',
    depositReturnAddress: 'stake1example',
    previousAction: null,
    voteAggregates: [
      {
        role: 'drep',
        representation: 'stake',
        yes: '1000000',
        no: '2000000',
        abstain: '3000000',
        notVoted: '0',
        totalEligible: '6000000',
        threshold: { numerator: 67, denominator: 100 },
      },
      {
        role: 'spo',
        representation: 'stake',
        yes: '4000000',
        no: '0',
        abstain: '0',
        notVoted: '0',
        totalEligible: '4000000',
        threshold: { numerator: 51, denominator: 100 },
      },
      {
        role: 'cc',
        representation: 'count',
        yes: '3',
        no: '1',
        abstain: '0',
        notVoted: '3',
        totalEligible: '7',
        threshold: QUORUM,
      },
    ],
    ...overrides,
  };
}

function proposalService(chainStub: StubApi): ProposalService {
  return new ProposalService(chain(chainStub), passthroughCache(), null);
}

describe('GET /proposal/list', () => {
  it('returns every legacy key with the legacy value types', async () => {
    const service = proposalService({
      governance: {
        proposals: { list: () => Promise.resolve(page([govAction()])) },
      },
    });

    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    expect(body.total).toBe(1);
    expect(body.elements[0]).toEqual({
      // db-sync's row id is gone; the CIP-129 action id is stable and unique.
      id: ACTION_ID,
      txHash: TX,
      index: 0,
      type: 'InfoAction',
      // Was db-sync's raw `description` column; the typed body replaces it.
      details: { type: 'InfoAction' },
      expiryDate: '2026-03-01T00:00:00.000Z',
      expiryEpochNo: 510,
      createdDate: '2026-01-05T00:00:00.000Z',
      createdEpochNo: 500,
      url: 'https://x/ga.jsonld',
      metadataHash: 'e'.repeat(64),
      protocolParams: null,
      // The CIP-108 strings live in the anchored document.
      title: null,
      abstract: null,
      motivation: null,
      rationale: null,
      dRepYesVotes: 1000000,
      dRepNoVotes: 2000000,
      dRepAbstainVotes: 3000000,
      poolYesVotes: 4000000,
      poolNoVotes: 0,
      poolAbstainVotes: 0,
      ccYesVotes: 3,
      ccNoVotes: 1,
      ccAbstainVotes: 0,
      prevGovActionIndex: null,
      prevGovActionTxHash: null,
      json: null,
      authors: [],
    });
  });

  function undatedAction(submittedEpoch: number, expiryEpoch: number) {
    const action = govAction();
    return govAction({
      lifecycle: {
        ...action.lifecycle,
        submitted: { epoch: submittedEpoch },
        expires: { epoch: expiryEpoch },
      },
    });
  }

  function networkInfo(network: string) {
    return () =>
      Promise.resolve(
        env({
          network,
          era: 'conway',
          tip: { epoch: 1430 },
          currentEpoch: 1430,
        }),
      );
  }

  it.each([
    // Five-day epochs from the mainnet system start (21:44:51 UTC).
    ['mainnet', 500, 510, '2024-07-28T21:44:51Z', '2024-09-16T21:44:51Z'],
    // One-day epochs from 2022-10-25.
    ['preview', 1417, 1448, '2026-09-11T00:00:00Z', '2026-10-12T00:00:00Z'],
  ])(
    'dates undated stamps on %s from the epoch schedule, as the legacy SQL did',
    async (network, submitted, expires, createdDate, expiryDate) => {
      const service = proposalService({
        network: { getNetworkInfo: networkInfo(network) },
        governance: {
          proposals: {
            list: () =>
              Promise.resolve(page([undatedAction(submitted, expires)])),
          },
        },
      });

      const [proposal] = (
        await service.list({ type: [], page: 0, pageSize: 10 })
      ).elements;
      expect(proposal).toMatchObject({
        createdDate,
        createdEpochNo: submitted,
        expiryDate,
        expiryEpochNo: expires,
      });
    },
  );

  it('keeps the legacy fallbacks on a network with no known schedule', async () => {
    const service = proposalService({
      network: { getNetworkInfo: networkInfo('private-testnet') },
      governance: {
        proposals: {
          list: () => Promise.resolve(page([undatedAction(10, 20)])),
        },
      },
    });

    const [proposal] = (await service.list({ type: [], page: 0, pageSize: 10 }))
      .elements;
    expect(proposal.createdDate).toBe('');
    expect(proposal.expiryDate).toBeNull();
  });

  it('reports UpdateCommittee under db-sync’s name, which is what clients expect', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          list: () =>
            Promise.resolve(
              page([
                govAction({
                  type: 'UpdateCommittee',
                  body: {
                    type: 'UpdateCommittee',
                    added: [],
                    removed: [],
                    quorum: QUORUM,
                  },
                }),
              ]),
            ),
        },
      },
    });

    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    // the contract renamed it; the wire format must not
    expect(body.elements[0].type).toBe('NewCommittee');
  });

  it('puts ParameterChange values on protocolParams', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          list: () =>
            Promise.resolve(
              page([
                govAction({
                  type: 'ParameterChange',
                  body: {
                    type: 'ParameterChange',
                    changes: { drepDeposit: '500000000' },
                  },
                }),
              ]),
            ),
        },
      },
    });
    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    const params = body.elements[0].protocolParams as Record<string, unknown>;

    // db-sync's param_proposal row, snake_case, every column present: the
    // frontend diffs it key by key against /epoch/params and tests `!== null`,
    // so an absent key would read as a proposed change.
    expect(Object.keys(params).sort()).toEqual(PARAM_PROPOSAL_COLUMNS);
    expect(params.drep_deposit).toBe(500000000);
    const others = Object.entries(params).filter(([k]) => k !== 'drep_deposit');
    expect(others.every(([, v]) => v === null)).toBe(true);
  });

  it('maps every proposed parameter, cost models included, to its column', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          list: () =>
            Promise.resolve(
              page([
                govAction({
                  type: 'ParameterChange',
                  body: {
                    type: 'ParameterChange',
                    changes: {
                      minFeeA: 45,
                      maxValSize: 6000,
                      coinsPerUtxoByte: '4400',
                      maxTxExecutionUnits: {
                        memory: 16500000,
                        steps: 10000000000,
                      },
                      executionUnitPrices: {
                        memory: { numerator: 577, denominator: 10000 },
                        steps: { numerator: 721, denominator: 10000000 },
                      },
                      costModels: { PlutusV3: [100788, 420] },
                      drepThresholds: protocolParams().drepThresholds,
                    },
                  },
                }),
              ]),
            ),
        },
      },
    });
    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    expect(body.elements[0].protocolParams).toMatchObject({
      min_fee_a: 45,
      max_val_size: 6000,
      coins_per_utxo_size: 4400,
      max_tx_ex_mem: 16500000,
      max_tx_ex_steps: 10000000000,
      price_mem: 0.0577,
      price_step: 0.0000721,
      cost_model: { id: null, costs: { PlutusV3: [100788, 420] }, hash: null },
      dvt_p_p_gov_group: 0.67,
      // Not a parameter; the frontend's diff view does not filter it out.
      epoch_no: null,
      min_fee_b: null,
      pvtpp_security_group: null,
    });
  });

  it('reports no anchor as null url and hash, with authors still an array', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          list: () => Promise.resolve(page([govAction({ anchor: null })])),
        },
      },
    });
    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    expect(body.elements[0]).toMatchObject({
      url: null,
      metadataHash: null,
      title: null,
      json: null,
      authors: [],
    });
  });

  it('reports a percent aggregate as 0 rather than a rounded fraction', async () => {
    // The legacy fields are whole lovelace and head counts; 0.67 has nothing
    // to put in them, and rounding it would read as "no votes".
    const service = proposalService({
      governance: {
        proposals: {
          list: () =>
            Promise.resolve(
              page([
                govAction({
                  voteAggregates: [
                    {
                      role: 'drep',
                      representation: 'percent',
                      yes: '0.67',
                      no: '0.2',
                      abstain: '0.13',
                      notVoted: '0',
                      totalEligible: '1',
                      threshold: { numerator: 67, denominator: 100 },
                    },
                  ],
                }),
              ]),
            ),
        },
      },
    });
    const body = await service.list({ type: [], page: 0, pageSize: 10 });
    expect(body.elements[0].dRepYesVotes).toBe(0);
  });

  it('filters by the legacy type name and pages the result', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          list: () =>
            Promise.resolve(
              page([
                govAction(),
                govAction({
                  id: 'gov_action1def',
                  txHash: 'b'.repeat(64),
                  type: 'UpdateCommittee',
                  body: {
                    type: 'UpdateCommittee',
                    added: [],
                    removed: [],
                    quorum: QUORUM,
                  },
                }),
              ]),
            ),
        },
      },
    });

    await expect(
      service.list({ type: ['NewCommittee'], page: 0, pageSize: 10 }),
    ).resolves.toMatchObject({ total: 1 });
    await expect(
      service.list({ type: [], page: 1, pageSize: 1 }),
    ).resolves.toMatchObject({ total: 2, page: 1, pageSize: 1 });
  });
});

describe('GET /proposal/get/:proposalId', () => {
  it('wraps the proposal with a null vote, as the legacy endpoint did', async () => {
    const service = proposalService({
      governance: {
        proposals: { get: () => Promise.resolve(env(govAction())) },
      },
    });

    const body = await service.get(`${TX}#0`);
    expect(Object.keys(body).sort()).toEqual(['proposal', 'vote']);
    expect(body.vote).toBeNull();
    expect(body.proposal.id).toBe(ACTION_ID);
  });

  it('404s with the legacy message when the action is not live', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          get: () => Promise.reject(new ChainDataError('NOT_FOUND', 'nope')),
        },
      },
    });

    await expect(service.get(`${TX}#0`)).rejects.toMatchObject({
      status: 404,
      response: {
        errorType: 'NotFoundError',
        message: `Proposal with id: ${TX}#0 not found`,
      },
    });
  });
});

describe('GET /proposal/enacted-details', () => {
  it('returns the five legacy keys, with null row ids', async () => {
    const service = proposalService({
      governance: {
        proposals: {
          getEnacted: () =>
            Promise.resolve(env({ id: ACTION_ID, txHash: TX, index: 1 })),
          get: () =>
            Promise.resolve(
              env(
                govAction({
                  body: {
                    type: 'HardForkInitiation',
                    protocolVersion: { major: 11, minor: 0 },
                  },
                }),
              ),
            ),
        },
      },
    });

    await expect(
      service.getEnactedDetails('HardForkInitiation'),
    ).resolves.toEqual({
      // db-sync row ids, which no provider carries now.
      id: null,
      txId: null,
      index: 1,
      description: {
        type: 'HardForkInitiation',
        protocolVersion: { major: 11, minor: 0 },
      },
      hash: TX,
    });
  });

  it('asks by lineage, not by type, and substitutes the hard-fork lineage', async () => {
    // `UpdateCommittee` and `NoConfidence` share a lineage, so a per-type
    // answer would be the wrong `prevGovActionId` and the ledger would reject
    // the transaction.
    const requested: string[] = [];
    const getEnacted = (lineage: string) => {
      requested.push(lineage);
      return Promise.resolve(env(null));
    };
    const service = proposalService({
      governance: { proposals: { getEnacted } },
    });

    await service.getEnactedDetails('ParameterChange');
    await service.getEnactedDetails('InfoAction');
    await service.getEnactedDetails(undefined);
    expect(requested).toEqual(['pparamUpdate', 'hardFork', 'hardFork']);
  });

  it('returns null when nothing of the lineage has ever been enacted', async () => {
    const service = proposalService({
      governance: {
        proposals: { getEnacted: () => Promise.resolve(env(null)) },
      },
    });
    await expect(
      service.getEnactedDetails('ParameterChange'),
    ).resolves.toBeNull();
  });
});

describe('GET /drep/getVotes/:drepId', () => {
  function voteRows(rows: DRepVoteRow[]): Promise<PagedEnvelope<DRepVoteRow>> {
    return Promise.resolve(env({ elements: rows, total: rows.length }));
  }

  it('returns the legacy vote/proposal pair shape', async () => {
    const service = drepService({
      governance: {
        dreps: {
          listVotes: () =>
            voteRows([
              {
                voted: true,
                action: { id: ACTION_ID, type: 'InfoAction' },
                choice: 'yes',
                anchor: {
                  url: 'https://x/r.jsonld',
                  dataHash: 'f'.repeat(64),
                },
                txRef: { txHash: '9'.repeat(64) },
                at: { epoch: 501, time: '2026-01-10T00:00:00.000Z' },
              },
            ]),
        },
        proposals: { list: () => Promise.resolve(page([govAction()])) },
      },
    });

    const votes = await service.getVotes(DREP_ID);
    expect(votes).toHaveLength(1);
    expect(votes[0].vote).toEqual({
      proposalId: ACTION_ID,
      // The legacy vote row named the DRep by its raw hex hash.
      drepId: HASH,
      vote: 'yes',
      url: 'https://x/r.jsonld',
      metadataHash: 'f'.repeat(64),
      epochNo: 501,
      date: '2026-01-10T00:00:00.000Z',
      txHash: '9'.repeat(64),
    });
    expect(votes[0].proposal.id).toBe(ACTION_ID);
    expect(votes[0].proposal.type).toBe('InfoAction');
  });

  it('drops the not-voted rows the listing also carries', async () => {
    // The contract's listing covers actions voted AND not voted, so the
    // participation denominator matches the list. The legacy endpoint returns
    // votes only.
    const service = drepService({
      governance: {
        dreps: {
          listVotes: () =>
            voteRows([
              { voted: false, action: { id: ACTION_ID, type: 'InfoAction' } },
            ]),
        },
        proposals: { list: () => Promise.resolve(page([govAction()])) },
      },
    });

    await expect(service.getVotes(DREP_ID)).resolves.toEqual([]);
  });

  it('filters the pairs by proposal type', async () => {
    const committeeAction = govAction({
      id: actionId('b'.repeat(64), 0),
      txHash: 'b'.repeat(64),
      type: 'UpdateCommittee',
      body: {
        type: 'UpdateCommittee',
        added: [],
        removed: [],
        quorum: QUORUM,
      },
    });

    const service = drepService({
      governance: {
        dreps: {
          listVotes: () =>
            voteRows([
              {
                voted: true,
                action: { id: ACTION_ID, type: 'InfoAction' },
                choice: 'no',
                anchor: null,
                txRef: { txHash: '9'.repeat(64) },
              },
              {
                voted: true,
                action: {
                  id: actionId('b'.repeat(64), 0),
                  type: 'UpdateCommittee',
                },
                choice: 'no',
                anchor: null,
                txRef: { txHash: '8'.repeat(64) },
              },
            ]),
        },
        proposals: {
          list: () => Promise.resolve(page([govAction(), committeeAction])),
        },
      },
    });

    // filtering uses the legacy type name
    await expect(
      service.getVotes(DREP_ID, ['NewCommittee']),
    ).resolves.toHaveLength(1);
    await expect(service.getVotes(DREP_ID, [])).resolves.toHaveLength(2);
  });

  it('answers 501 when the provider does not serve a DRep vote listing', async () => {
    const service = drepService({ governance: { dreps: {} } });
    await expect(service.getVotes(DREP_ID)).rejects.toMatchObject({
      status: 501,
    });
  });
});

/* ------------------------------------------------------------------------- */
/* Legacy identifiers in, contract identifiers out to the provider            */
/* ------------------------------------------------------------------------- */

describe('legacy identifiers are translated before the provider sees them', () => {
  function drepsById(dreps: DRep[], asked: string[]) {
    return (id: string): Promise<Envelope<DRep>> => {
      asked.push(id);
      const found = dreps.find((drep) => drep.id === id);
      return found === undefined
        ? Promise.reject(new ChainDataError('NOT_FOUND', id))
        : Promise.resolve(env(found));
    };
  }

  it.each([
    ['CIP-129', DREP_ID],
    ['CIP-105', DREP_VIEW],
    ['raw hex, as the wallet context sends', HASH],
  ])('finds a key DRep by its %s id', async (_form, input) => {
    const asked: string[] = [];
    const service = drepService({
      governance: { dreps: { get: drepsById([fullDRep()], asked) } },
    });
    await expect(service.getInfo(input)).resolves.toMatchObject({
      isRegisteredAsDRep: true,
    });
    expect(asked).toEqual([DREP_ID]);
  });

  it('falls back to the script id for a bare hash the key id does not match', async () => {
    const scriptId = drepId('a', true);
    const asked: string[] = [];
    const service = drepService({
      governance: {
        dreps: {
          get: drepsById(
            [
              fullDRep({
                id: scriptId,
                isScriptBased: true,
                votingPower: { amount: '42', basis: 'active' },
              }),
            ],
            asked,
          ),
        },
      },
    });
    await expect(service.getVotingPower(HASH)).resolves.toBe(42);
    expect(asked).toEqual([DREP_ID, scriptId]);
  });

  it('answers 400 for a malformed DRep id without calling the provider', async () => {
    const asked: string[] = [];
    const service = drepService({
      governance: { dreps: { get: drepsById([], asked) } },
    });
    for (const call of [
      () => service.getInfo('drep1nope'),
      () => service.getVotingPower('a'.repeat(58)),
      () => service.getVotes('../etc'),
    ]) {
      await expect(call()).rejects.toMatchObject({
        status: 400,
        response: { errorType: 'ValidationError' },
      });
    }
    expect(asked).toEqual([]);
  });

  it('lists votes by the resolved CIP-129 id, and none for an unknown DRep', async () => {
    const listed: string[] = [];
    const service = drepService({
      governance: {
        dreps: {
          get: drepsById([fullDRep()], []),
          listVotes: (id) => {
            listed.push(id);
            return Promise.resolve(env({ elements: [], total: 0 }));
          },
        },
        proposals: { list: () => Promise.resolve(page([govAction()])) },
      },
    });
    await expect(service.getVotes(HASH)).resolves.toEqual([]);
    await expect(service.getVotes(DREP_VIEW)).resolves.toEqual([]);
    expect(listed).toEqual([DREP_ID, DREP_ID]);

    // Legacy: an unknown credential has no vote rows, not a 404.
    await expect(service.getVotes('c'.repeat(56))).resolves.toEqual([]);
    expect(listed).toHaveLength(2);
  });

  it.each([
    ['txHash#index, as the frontend sends', `${TX}#0`],
    ['CIP-129', ACTION_ID],
  ])('reads a proposal by its %s id', async (_form, input) => {
    const asked: string[] = [];
    const service = proposalService({
      governance: {
        proposals: {
          get: (id) => {
            asked.push(id);
            return Promise.resolve(env(govAction()));
          },
        },
      },
    });
    await expect(service.get(input)).resolves.toMatchObject({
      proposal: { txHash: TX, index: 0 },
    });
    expect(asked).toEqual([ACTION_ID]);
  });

  it.each([`${TX}#256`, `${TX}`, 'gov_action1abc', `zz${TX.slice(2)}#0`])(
    'answers 400 for the malformed proposal id %p',
    async (input) => {
      const service = proposalService({ governance: { proposals: {} } });
      await expect(service.get(input)).rejects.toMatchObject({
        status: 400,
        response: { errorType: 'ValidationError' },
      });
    },
  );

  it('asks for a stake address in bech32, and answers null for one never seen', async () => {
    const asked: string[] = [];
    const service = new AdaHolderService(
      chain({
        accounts: {
          getDelegation: (address) => {
            asked.push(address);
            return Promise.reject(new ChainDataError('NOT_FOUND', address));
          },
        },
      }),
      passthroughCache(),
    );
    await expect(service.getCurrentDelegation(STAKE_KEY)).resolves.toBeNull();
    expect(asked).toEqual([
      'stake_test1uz42424242424242424242424242424242424242424242s7f0kdw',
    ]);
  });

  it('answers 400 for a malformed stake key, even on the zero-on-failure route', async () => {
    const service = new AdaHolderService(
      chain({
        accounts: {
          getVotingPower: () => Promise.resolve(env(null)),
          getDelegation: () => Promise.resolve(env(null)),
        },
      }),
      passthroughCache(),
    );
    await expect(service.getVotingPower('a'.repeat(55))).rejects.toMatchObject({
      status: 400,
    });
    await expect(
      service.getCurrentDelegation('stake_test1abc'),
    ).rejects.toMatchObject({ status: 400 });
  });

  it('reads a bare 56-hex stake key hash as a key reward address on the served network', async () => {
    const asked: string[] = [];
    let networkLookups = 0;
    const api = chain({
      network: {
        getNetworkInfo: () => {
          networkLookups += 1;
          return Promise.resolve(
            env({
              network: 'mainnet',
              era: 'conway',
              tip: { epoch: 500 },
              currentEpoch: 500,
            }),
          );
        },
      },
      accounts: {
        get: (stakeAddress: string) => {
          asked.push(stakeAddress);
          return Promise.resolve(
            env({ stakeAddress, stakeKeyHash: HASH, isRegistered: true }),
          );
        },
        getDelegation: (stakeAddress: string) => {
          asked.push(stakeAddress);
          return Promise.resolve(env(null));
        },
      },
    });
    const network = new LegacyNetwork(api);
    const mainnetKey =
      'stake1ux42424242424242424242424242424242424242424242ser95fn';

    await new AccountService(api, passthroughCache(), network).getAccountInfo(
      HASH,
    );
    await new AdaHolderService(
      api,
      passthroughCache(),
      network,
    ).getCurrentDelegation(HASH.toUpperCase());

    expect(asked).toEqual([mainnetKey, mainnetKey]);
    // Asked once, then remembered.
    expect(networkLookups).toBe(1);
  });

  it('takes the served network from the db-sync config without asking the provider', async () => {
    const asked: string[] = [];
    const api = chain({
      accounts: {
        getDelegation: (stakeAddress: string) => {
          asked.push(stakeAddress);
          return Promise.resolve(env(null));
        },
      },
    });
    const config = {
      get: () => ({ dbSync: { network: 'preview' } }),
    } as unknown as ConfigService;

    await new AdaHolderService(
      api,
      passthroughCache(),
      new LegacyNetwork(api, config),
    ).getCurrentDelegation(HASH);

    expect(asked).toEqual([legacyStakeAddress(STAKE_KEY)]);
  });
});
