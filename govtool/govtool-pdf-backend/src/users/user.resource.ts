import { col, defineResource } from '../query/resource';

/**
 * The public user projection (§3.4): the only way a user row reaches another
 * user. `fields[0]=username` is accepted and yields `attributes: {}` (Δ2).
 * Filters reach only `id` and `govtool_username`.
 */
export const PublicUserResource = defineResource({
  name: 'user',
  scalars: { govtool_username: col.str('govtoolUsername') },
  timestamps: false,
  noopFields: ['username'],
});
