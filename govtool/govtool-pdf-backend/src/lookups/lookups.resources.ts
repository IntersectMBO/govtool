import { col, defineResource, ResourceDef } from '../query/resource';

/** §5.2: seeded, publishedAt. */
export const GovernanceActionTypeResource = defineResource({
  name: 'governance-action-type',
  scalars: { gov_action_type_name: col.str('name', false) },
  publishedAt: true,
});

/** Route segment, descriptor and Prisma delegate name of each lookup list (§8.1). */
export const LOOKUP_ROUTES: ReadonlyArray<{
  path: string;
  resource: ResourceDef;
  model: 'governanceActionType';
}> = [
  { path: 'governance-action-types', resource: GovernanceActionTypeResource, model: 'governanceActionType' },
];
