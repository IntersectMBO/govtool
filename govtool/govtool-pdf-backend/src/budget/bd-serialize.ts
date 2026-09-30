// BD wire shapes beyond the generic serializer (SPEC §8.8): the computed
// attributes and the raw, non-enveloped POST /api/bds response.

import { ListDelegate } from '../query/list';
import { ResourceDef } from '../query/resource';
import { serializeComponents, serializeScalars } from '../query/serialize';
import {
  BdCostingResource,
  BdFurtherInformationResource,
  BdProposalDetailResource,
  BdProposalOwnershipResource,
  BdPsapbResource,
  BdResource,
} from './budget.resources';

type Row = Record<string, unknown>;

/**
 * Include merged into every BD read so the computed attributes can be built.
 * Loading `creator` without a populate is safe: the serializer only emits
 * populated relations.
 */
export function withComputedInclude(include: Record<string, unknown> | undefined): Record<string, unknown> {
  const inc: Record<string, unknown> = { ...(include ?? {}) };
  inc.master = { select: { createdAt: true } };
  if (!inc.creator) inc.creator = true;
  return inc;
}

/** A `prisma.bd` delegate for findList that always loads what `bdExtra` reads. */
export function bdListDelegate(d: {
  findMany(args: object): Promise<object[]>;
  count(args: object): Promise<number>;
}): ListDelegate {
  return {
    findMany: (args: object) => {
      const a = args as { include?: Record<string, unknown> };
      return d.findMany({ ...a, include: withComputedInclude(a.include) });
    },
    count: (args: object) => d.count(args),
  };
}

/** `master_proposal_created_at` and `user_govtool_username` (§8.8). */
export function bdExtra(row: object): Record<string, unknown> {
  const r = row as Row;
  const master = r.master as { createdAt?: Date } | null | undefined;
  const creator = r.creator as { govtoolUsername?: string | null } | null | undefined;
  const created = master?.createdAt ?? (r.createdAt as Date);
  return {
    master_proposal_created_at: created instanceof Date ? created.toISOString() : created,
    user_govtool_username: creator?.govtoolUsername ?? 'Anonymous',
  };
}

function plainSection(v: unknown, resource: ResourceDef): Record<string, unknown> | null {
  if (!v) return null;
  return {
    id: (v as Row).id,
    ...serializeScalars(v, resource),
    ...serializeComponents(v, resource),
  };
}

/**
 * Raw POST /api/bds body: top-level `master_id`, sections as plain objects of
 * their scalars (no lookup relations), creator as the public projection (Δ39).
 * Contact information is never returned (Δ11).
 */
export function bdCreateResponse(row: object): Record<string, unknown> {
  const r = row as Row;
  const creator = r.creator as { id: number; govtoolUsername: string | null };
  return {
    id: r.id,
    ...serializeScalars(row, BdResource),
    bd_proposal_ownership: plainSection(r.proposalOwnership, BdProposalOwnershipResource),
    bd_psapb: plainSection(r.psapb, BdPsapbResource),
    bd_proposal_detail: plainSection(r.proposalDetail, BdProposalDetailResource),
    bd_costing: plainSection(r.costing, BdCostingResource),
    bd_further_information: plainSection(r.furtherInformation, BdFurtherInformationResource),
    creator: { id: creator.id, govtool_username: creator.govtoolUsername },
  };
}
