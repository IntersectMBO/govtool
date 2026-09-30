import type { ProviderCapabilities, ProviderHealth, SystemApi } from '@govtool/data-providers/chain-data';

import { PROVIDER_ID, type Ctx } from './context';
import { DREP_FILTERS, DREP_SEARCH, DREP_SORTS, DREP_VOTE_SORTS } from './governance/dreps';
import { PROPOSAL_FILTERS, PROPOSAL_OPTIONAL_ARGUMENTS, PROPOSAL_SORTS, VOTE_AGGREGATE } from './governance/proposals';
import { NETWORK_OPTIONAL_ARGUMENTS } from './network';
import { toIso } from './numbers';

/** Koios follows the chain block by block; a tip older than this is lagging. */
export const STALE_AFTER_SECONDS = 600;

export function capabilities(): ProviderCapabilities {
  return {
    // Vote sorts are honoured by the DRep vote listing (dreps.listVotes).
    sorts: { dreps: DREP_SORTS, proposals: PROPOSAL_SORTS, ...(DREP_VOTE_SORTS.length ? { votes: DREP_VOTE_SORTS } : {}) },
    filters: { dreps: DREP_FILTERS, proposals: PROPOSAL_FILTERS },
    search: DREP_SEARCH,
    voteAggregate: VOTE_AGGREGATE,
    optionalArguments: [...NETWORK_OPTIONAL_ARGUMENTS, ...PROPOSAL_OPTIONAL_ARGUMENTS],
  };
}

export function createSystemApi(ctx: Ctx): SystemApi {
  return {
    getIdentity: async () => ctx.envelope({ id: PROVIDER_ID, name: 'Koios' }),
    getCapabilities: async () => ctx.envelope(capabilities()),
    getHealth: async () => {
      let health: ProviderHealth;
      try {
        const tip = await ctx.chain.tip();
        const at = toIso(tip.block_time);
        const lag = Math.max(0, Math.round(Date.now() / 1000 - tip.block_time));
        const block = tip.block_height ?? tip.block_no;
        health = {
          status: lag > STALE_AFTER_SECONDS ? 'degraded' : 'healthy',
          tip: { epoch: tip.epoch_no, slot: tip.abs_slot, ...(block == null ? {} : { block }), time: at },
          lastSuccessfulSyncAt: at,
          secondsSinceLastUpdate: lag,
          ...(lag > STALE_AFTER_SECONDS ? { message: `tip is ${lag} s old` } : {}),
        };
      } catch (error) {
        health = { status: 'unavailable', message: error instanceof Error ? error.message : String(error) };
      }
      return ctx.envelope(health);
    },
  };
}
