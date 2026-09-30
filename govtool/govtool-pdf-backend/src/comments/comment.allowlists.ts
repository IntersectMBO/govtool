import { scalarPaths } from '../query/resource';
import { QueryAllowlist } from '../query/types';
import { CommentResource } from './comment.resources';

/**
 * GET /api/comments (§8.7). `comments_reports.hash` is `$eq` only (knowing
 * the hash is the capability). `comments_reports.maintainer` is what pdf-ui
 * sends for a relation that does not exist: accepted, ignored.
 */
export const COMMENTS_ALLOWLIST: QueryAllowlist = {
  resource: CommentResource,
  filterable: [
    ...scalarPaths(CommentResource),
    'comments_reports.hash',
    'comments_reports.moderation_status',
  ],
  filterOps: { 'comments_reports.hash': ['$eq'] },
  sortable: scalarPaths(CommentResource),
  populatable: ['comments_reports.reporter'],
  ignoredPopulate: ['comments_reports.maintainer'],
};
