import type { GovernanceApi } from '@govtool/data-providers/chain-data';

import type { Ctx } from '../context';
import { createCommitteeApi } from './committee';
import { createDRepsApi } from './dreps';
import { createPoolsApi } from './pools';
import { createProposalsApi } from './proposals';

export function createGovernanceApi(ctx: Ctx): GovernanceApi {
  return {
    dreps: createDRepsApi(ctx),
    proposals: createProposalsApi(ctx),
    pools: createPoolsApi(ctx),
    committee: createCommitteeApi(ctx),
  };
}
