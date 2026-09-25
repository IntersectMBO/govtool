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
import { toInt, toIso } from './numbers';

/** db-sync follows the chain block by block; a tip older than this is lagging. */
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
    getIdentity: async () => ctx.envelope({ id: PROVIDER_ID, name: 'Cardano DB Sync' }),
    getCapabilities: async () => ctx.envelope(capabilities()),
    getHealth: async () => {
      let health: ProviderHealth;
      try {
        const [tip] = await ctx.db.query<{ epoch_no: number; slot_no: string; block_no: string; time: Date }>(
          `SELECT epoch_no, slot_no, block_no, time FROM block
            WHERE block_no IS NOT NULL ORDER BY id DESC LIMIT 1`,
        );
        if (!tip) {
          health = { status: 'unavailable', message: 'db-sync has no blocks' };
        } else {
          const at = toIso(tip.time);
          const lag = Math.max(0, Math.round((Date.now() - new Date(at).getTime()) / 1000));
          health = {
            status: lag > STALE_AFTER_SECONDS ? 'degraded' : 'healthy',
            tip: { epoch: tip.epoch_no, slot: toInt(tip.slot_no), block: toInt(tip.block_no), time: at },
            lastSuccessfulSyncAt: at,
            secondsSinceLastUpdate: lag,
            ...(lag > STALE_AFTER_SECONDS ? { message: `tip is ${lag} s old` } : {}),
          };
        }
      } catch (error) {
        health = { status: 'unavailable', message: error instanceof Error ? error.message : String(error) };
      }
      return ctx.envelope(health);
    },
  };
}
