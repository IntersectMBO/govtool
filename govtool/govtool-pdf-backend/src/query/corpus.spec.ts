// Every Appendix A query parses against its endpoint allowlist, and each
// resource rejects private user paths (§11.1 Allowlist).

import { CORPUS } from '../../test/helpers/pdf-ui-corpus';
import {
  BD_DRAFTS_ALLOWLIST,
  BD_POLL_VOTES_ALLOWLIST,
  BD_POLLS_ALLOWLIST,
  BDS_ALLOWLIST,
} from '../budget/budget.allowlists';
import { COMMENTS_ALLOWLIST } from '../comments/comment.allowlists';
import { ApiError } from '../common/errors';
import { lookupAllowlist } from '../lookups/lookups.controller';
import { LOOKUP_ROUTES } from '../lookups/lookups.resources';
import { POLL_VOTES_ALLOWLIST, POLLS_ALLOWLIST } from '../polls/poll.allowlists';
import { PROPOSAL_VOTES_ALLOWLIST, PROPOSALS_ALLOWLIST } from '../proposals/proposal.allowlists';
import { parseQuery } from './parse';
import { toPrismaInclude, toPrismaOrderBy, toPrismaWhere } from './prisma';
import { parseQueryString } from './raw-query';
import { QueryAllowlist } from './types';

const ALLOWLISTS: Record<string, QueryAllowlist> = {
  proposals: PROPOSALS_ALLOWLIST,
  'proposal-votes': PROPOSAL_VOTES_ALLOWLIST,
  polls: POLLS_ALLOWLIST,
  'poll-votes': POLL_VOTES_ALLOWLIST,
  comments: COMMENTS_ALLOWLIST,
  bds: BDS_ALLOWLIST,
  'bds/1': BDS_ALLOWLIST,
  'bd-polls': BD_POLLS_ALLOWLIST,
  'bd-poll-votes': BD_POLL_VOTES_ALLOWLIST,
  'bd-drafts': BD_DRAFTS_ALLOWLIST,
  ...Object.fromEntries(LOOKUP_ROUTES.map((r) => [r.path, lookupAllowlist(r.resource)])),
};

function v(fn: () => unknown): string {
  try {
    fn();
  } catch (e) {
    if (e instanceof ApiError) return `${e.errorName}: ${e.message}`;
    throw e;
  }
  return 'no error';
}

describe('pdf-ui corpus (Appendix A)', () => {
  it.each(CORPUS.map((c) => [`${c.route}?${c.query}`, c] as const))('%s', (_label, c) => {
    const allowlist = ALLOWLISTS[c.route];
    expect(allowlist).toBeDefined();
    const q = parseQuery(parseQueryString(c.query), allowlist);
    // It must also translate (virtual prop_id is the one rewrite, done by
    // the endpoint: skip translation of the filter in that case only).
    if (!c.query.includes('[prop_id]')) toPrismaWhere(allowlist.resource, q.filters);
    toPrismaOrderBy(allowlist.resource, q.sort);
    toPrismaInclude(allowlist.resource, q.populate);
  });

  it('proposals list sorts resolve to the intended Prisma order', () => {
    const q = parseQuery(parseQueryString('sort[proposal][prop_likes]=DESC'), PROPOSALS_ALLOWLIST);
    expect(toPrismaOrderBy(PROPOSALS_ALLOWLIST.resource, q.sort)).toEqual([
      { proposal: { likes: 'desc' } },
      { id: 'asc' },
    ]);
  });

  it('comments corpus drops maintainer and projects reporter to nothing (Δ2)', () => {
    const q = parseQuery(
      parseQueryString(
        'populate[comments_reports][populate][reporter][fields][0]=username&populate[comments_reports][populate][maintainer][fields][0]=username',
      ),
      COMMENTS_ALLOWLIST,
    );
    const reports = q.populate.get('comments_reports');
    expect([...(reports?.children.keys() ?? [])]).toEqual(['reporter']);
    expect(reports?.children.get('reporter')?.fields).toEqual([]);
  });

  it('bd single corpus populates further information without a component populate', () => {
    const q = parseQuery(
      parseQueryString('populate[0]=bd_further_information.proposal_links'),
      BDS_ALLOWLIST,
    );
    expect(toPrismaInclude(BDS_ALLOWLIST.resource, q.populate)).toEqual({
      furtherInformation: { include: { links: { orderBy: [{ position: 'asc' }, { id: 'asc' }] } } },
    });
  });
});

describe('private paths are rejected on every resource', () => {
  it.each([
    ['bds', 'filters[creator][username]=e0', 'ValidationError: Invalid key creator.username'],
    ['bds', 'filters[creator][email]=x', 'ValidationError: Invalid key creator.email'],
    ['bds', 'sort[creator][email]=asc', 'ValidationError: Invalid key creator.email'],
    ['bds', 'sort[creator][username]=asc', 'ValidationError: Invalid key creator.username'],
    ['bds', 'populate=bd_contact_information', 'ValidationError: Invalid populate bd_contact_information'],
    [
      'bds',
      'filters[bd_contact_information][be_email]=x',
      'ValidationError: Invalid key bd_contact_information',
    ],
    ['bd-drafts', 'filters[creator][username]=e0', 'ValidationError: Invalid key creator.username'],
    ['bd-drafts', 'filters[draft_data]=x', 'ValidationError: Invalid key draft_data'],
    [
      'comments',
      'filters[comments_reports][hash][$containsi]=a',
      'ValidationError: Invalid operator $containsi',
    ],
    [
      'comments',
      'filters[comments_reports][reporter][username]=e0',
      'ValidationError: Invalid key comments_reports.reporter.username',
    ],
    [
      'comments',
      'populate[comments_reports][populate][moderator]=*',
      'ValidationError: Invalid populate comments_reports.moderator',
    ],
    ['comments', 'sort[comments_reports][hash]=asc', 'ValidationError: Invalid key comments_reports.hash'],
    ['proposals', 'fields[0]=prop_name', 'ValidationError: Invalid query parameter: fields'],
    ['proposals', 'filters[proposal][user][username]=x', 'ValidationError: Invalid key proposal.user'],
    ['governance-action-types', 'populate=*', 'no error'],
    ['governance-action-types', 'populate=contents', 'ValidationError: Invalid populate contents'],
  ])('%s ?%s', (route, query, expected) => {
    expect(v(() => parseQuery(parseQueryString(query), ALLOWLISTS[route]))).toBe(expected);
  });
});
