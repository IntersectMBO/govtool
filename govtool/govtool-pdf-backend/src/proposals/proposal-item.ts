// The fixed /proposals item shape (SPEC §8.2): a proposal wrapping one of its
// contents. `content` and `content.attributes.gov_action_type` carry no
// `data` wrapper; the two content relations are `{data}`-wrapped (Δ21).

import { Prisma } from '@prisma/client';
import { GovernanceActionTypeResource } from '../lookups/lookups.resources';
import { Entity, serializeEntity, serializeScalars } from '../query/serialize';
import { PopulateTree } from '../query/types';
import { ProposalContentResource, ProposalResource } from './proposal.resources';

/** What every read of a content row loads to build an item. */
export const CONTENT_ITEM_INCLUDE = {
  links: { orderBy: [{ position: 'asc' }, { id: 'asc' }] },
  withdrawals: { orderBy: [{ position: 'asc' }, { id: 'asc' }] },
  constitutionContent: true,
  hardForkContent: true,
  govActionType: true,
} satisfies Prisma.ProposalContentInclude;

export type ContentForItem = Prisma.ProposalContentGetPayload<{ include: typeof CONTENT_ITEM_INCLUDE }>;

export const PROPOSAL_ITEM_INCLUDE = {
  user: { select: { govtoolUsername: true } },
} satisfies Prisma.ProposalInclude;

export type ProposalForItem = Prisma.ProposalGetPayload<{ include: typeof PROPOSAL_ITEM_INCLUDE }>;

const CONTENT_RELATIONS: PopulateTree = new Map([
  ['proposal_constitution_content', { fields: null, children: new Map() }],
  ['proposal_hard_fork_content', { fields: null, children: new Map() }],
]);

export function serializeContent(content: ContentForItem): Entity {
  const e = serializeEntity(content, ProposalContentResource, { populate: CONTENT_RELATIONS });
  const t = serializeEntity(content.govActionType, GovernanceActionTypeResource);
  e.attributes.gov_action_type = t;
  return e;
}

export function serializeProposalItem(proposal: ProposalForItem, content: ContentForItem | null): Entity {
  return {
    id: proposal.id,
    attributes: {
      ...serializeScalars(proposal, ProposalResource),
      user_govtool_username: proposal.user.govtoolUsername ?? 'Anonymous',
      content: content ? serializeContent(content) : null,
    },
  };
}
