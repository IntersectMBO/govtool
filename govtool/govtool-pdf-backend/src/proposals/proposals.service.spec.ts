import { ApiError } from '../common/errors';
import { parseQuery } from '../query/parse';
import { toPrismaWhere } from '../query/prisma';
import { parseQueryString } from '../query/raw-query';
import { PROPOSALS_ALLOWLIST } from './proposal.allowlists';
import { ProposalContentResource } from './proposal.resources';
import { parseSubmissionDate, rewriteProposalFilters } from './proposals.service';

const where = (qs: string, caller: number | null) => {
  const q = parseQuery(parseQueryString(qs), PROPOSALS_ALLOWLIST);
  return toPrismaWhere(ProposalContentResource, rewriteProposalFilters(q.filters, caller));
};

describe('rewriteProposalFilters (§8.2)', () => {
  it('no filters: active, non-draft revisions', () => {
    expect(where('', null)).toEqual({
      AND: [{ revActive: { equals: true } }, { isDraft: { equals: false } }],
    });
  });

  it('prop_id becomes proposal_id and drops the rev_active rule', () => {
    expect(where('filters[$and][0][prop_id]=5', null)).toEqual({
      AND: [{ proposalId: { equals: 5 } }, { isDraft: { equals: false } }],
    });
  });

  it('is_draft forces the caller and drops every client user_id (Δ22)', () => {
    expect(
      where('filters[$and][2][is_draft]=true&filters[$and][3][user_id]=9&filters[user_id]=8', 4),
    ).toEqual({
      AND: [{ isDraft: { equals: true } }, { revActive: { equals: true } }, { userId: { equals: 4 } }],
    });
  });

  it('is_draft without a caller is BD `User is required`', () => {
    try {
      where('filters[is_draft]=false', null);
      throw new Error('expected');
    } catch (e) {
      expect(e).toBeInstanceOf(ApiError);
      expect((e as ApiError).details).toBe('User is required');
    }
  });

  it('is_draft nested under $or is not the draft switch: non-drafts are still forced', () => {
    const w = where('filters[$or][0][is_draft]=true&filters[$or][1][prop_name]=x', null);
    expect(w).toEqual({
      AND: [
        { OR: [{ isDraft: { equals: true } }, { name: { equals: 'x' } }] },
        { revActive: { equals: true } },
        { isDraft: { equals: false } },
      ],
    });
  });

  it('prop_id anywhere but the top level is not a field', () => {
    expect(() => where('filters[$or][0][prop_id]=1', null)).toThrow('Invalid key prop_id');
  });

  it('deep counter sort and filter paths are accepted', () => {
    const q = parseQuery(
      parseQueryString('sort[proposal][prop_likes]=DESC&filters[proposal][prop_likes][$gt]=1'),
      PROPOSALS_ALLOWLIST,
    );
    expect(q.sort).toEqual([{ path: ['proposal', 'prop_likes'], direction: 'desc' }]);
    expect(toPrismaWhere(ProposalContentResource, q.filters)).toEqual({
      proposal: { is: { likes: { gt: 1 } } },
    });
  });
});

describe('parseSubmissionDate', () => {
  it('stores the UTC date of a date or datetime', () => {
    expect(parseSubmissionDate('2026-09-26').toISOString()).toBe('2026-09-26T00:00:00.000Z');
    expect(parseSubmissionDate('2026-09-26T23:59:59.999Z').toISOString()).toBe('2026-09-26T00:00:00.000Z');
    expect(parseSubmissionDate('2026-09-26T01:00:00+02:00').toISOString()).toBe('2026-09-25T00:00:00.000Z');
  });
  it.each(['soon', '', '26/09/2026', '2026-13-40', 5])('rejects %p', (v) => {
    expect(() => parseSubmissionDate(v)).toThrow('prop_submission_date is invalid');
  });
});
