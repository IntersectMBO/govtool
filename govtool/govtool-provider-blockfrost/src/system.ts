import type { ProviderCapabilities, ProviderHealth, SystemApi } from '@govtool/data-providers/chain-data';

import { PROVIDER_ID, type Ctx } from './context';
import { DREP_FILTERS, DREP_SEARCH, DREP_SORTS, DREP_VOTE_SORTS } from './governance/dreps';
import {
  PROPOSAL_FILTERS,
  PROPOSAL_OPTIONAL_ARGUMENTS,
  PROPOSAL_SORTS,
  VOTE_AGGREGATE,
} from './governance/proposals';
import { NETWORK_OPTIONAL_ARGUMENTS } from './network';
import { isoFromUnix } from './numbers';

/** Blockfrost follows the chain block by block; a tip older than this is lagging. */
export const STALE_AFTER_SECONDS = 600;

export function capabilities(): ProviderCapabilities {
  return {
    // Vote sorts are honoured by the DRep vote listing (dreps.listVotes).
    sorts: { dreps: DREP_SORTS, proposals: PROPOSAL_SORTS, votes: DREP_VOTE_SORTS },
    filters: { dreps: DREP_FILTERS, proposals: PROPOSAL_FILTERS },
    search: DREP_SEARCH,
    voteAggregate: VOTE_AGGREGATE,
    optionalArguments: [...NETWORK_OPTIONAL_ARGUMENTS, ...PROPOSAL_OPTIONAL_ARGUMENTS],
  };
}

export function createSystemApi(ctx: Ctx): SystemApi {
  return {
    getIdentity: async () => ctx.envelope({ id: PROVIDER_ID, name: 'Blockfrost' }),
    getCapabilities: async () => ctx.envelope(capabilities()),
    getHealth: async () => {
      let health: ProviderHealth;
      try {
        const [status, tip] = await Promise.all([
          ctx.http.get<{ is_healthy: boolean }>('/health'),
          ctx.http.get<{ time: number; height: number | null; slot: number | null; epoch: number | null }>('/blocks/latest'),
        ]);
        const at = isoFromUnix(tip.time);
        const lag = Math.max(0, Math.round(Date.now() / 1000 - tip.time));
        const stale = lag > STALE_AFTER_SECONDS;
        health = {
          status: !status.is_healthy ? 'unavailable' : stale ? 'degraded' : 'healthy',
          ...(tip.epoch === null
            ? {}
            : {
                tip: {
                  epoch: tip.epoch,
                  ...(tip.slot === null ? {} : { slot: tip.slot }),
                  ...(tip.height === null ? {} : { block: tip.height }),
                  time: at,
                },
              }),
          lastSuccessfulSyncAt: at,
          secondsSinceLastUpdate: lag,
          ...(!status.is_healthy ? { message: 'Blockfrost reports itself unhealthy' } : stale ? { message: `tip is ${lag} s old` } : {}),
        };
      } catch (error) {
        // ChainDataError messages are safe by construction (see ./http).
        health = { status: 'unavailable', message: error instanceof Error ? error.message : 'Blockfrost is unreachable' };
      }
      return ctx.envelope(health);
    },
  };
}
