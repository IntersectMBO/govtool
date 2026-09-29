import { col, defineResource } from '../query/resource';
import { PublicUserResource } from '../users/user.resource';

/**
 * §5.3 CommentsReport. `hash` is hidden: filterable where an allowlist names
 * it (`comments_reports.hash`, `$eq` only), never serialized (Δ10). The
 * `moderator` relation is not declared, so it can never be populated.
 */
export const CommentsReportResource = defineResource({
  name: 'comments-report',
  scalars: { moderation_status: col.bool('moderationStatus', true) },
  hidden: { hash: col.str('hash', false) },
  publishedAt: true,
  relations: {
    reporter: {
      field: 'reporter',
      target: () => PublicUserResource,
      many: false,
      fk: 'reporterId',
      nullable: false,
    },
    comment: {
      field: 'comment',
      target: () => CommentResource,
      many: false,
      fk: 'commentId',
      nullable: false,
    },
  },
});

/**
 * §5.3 Comment. The computed `user_govtool_username`, `user_is_validated` and
 * `subcommens_number` (§8.7) are added by the endpoint as `extra`.
 */
export const CommentResource = defineResource({
  name: 'comment',
  scalars: {
    proposal_id: col.legacyId('proposalId', true),
    bd_proposal_id: col.legacyId('bdMasterId', true),
    comment_parent_id: col.legacyId('parentId', true),
    user_id: col.legacyId('userId'),
    comment_text: col.str('text', false),
    drep_id: col.str('drepId'),
  },
  relations: {
    comments_reports: { field: 'reports', target: () => CommentsReportResource, many: true },
  },
});
