import { PROPOSALS_ALLOWLIST } from '../proposals/proposal.allowlists';
import { ProposalContentResource } from '../proposals/proposal.resources';
import {
  andWith,
  hasTopLevel,
  mentions,
  removeTopLevel,
  renameTopLevel,
  topLevelConditions,
} from './filter-helpers';
import { makeCond, parseQuery } from './parse';
import { toPrismaWhere } from './prisma';
import { parseQueryString } from './raw-query';

const f = (s: string) => parseQuery(parseQueryString(s), PROPOSALS_ALLOWLIST).filters;

describe('filter helpers (the /proposals rewrites)', () => {
  it('finds top-level conditions through $and but not $or', () => {
    const root = f('filters[$and][0][prop_id]=3&filters[is_draft]=true&filters[$or][0][user_id]=1');
    expect(topLevelConditions(root).map((c) => c.path.join('.'))).toEqual(['prop_id', 'is_draft']);
    expect(hasTopLevel(root, 'prop_id')).toBe(true);
    expect(hasTopLevel(root, 'user_id')).toBe(false);
    expect(mentions(root, 'user_id')).toBe(true);
  });

  it('prop_id → proposal_id, then translates', () => {
    const root = renameTopLevel(f('filters[$and][0][prop_id]=3'), 'prop_id', ['proposal_id']);
    expect(toPrismaWhere(ProposalContentResource, root)).toEqual({ proposalId: { equals: 3 } });
  });

  it('drop a client user_id, force the caller and rev_active', () => {
    const root = removeTopLevel(f('filters[$and][0][is_draft]=true&filters[$and][1][user_id]=9'), 'user_id');
    const forced = andWith(
      root,
      makeCond(ProposalContentResource, ['user_id'], '$eq', 4),
      makeCond(ProposalContentResource, ['prop_rev_active'], '$eq', true),
    );
    expect(toPrismaWhere(ProposalContentResource, forced)).toEqual({
      AND: [{ isDraft: { equals: true } }, { userId: { equals: 4 } }, { revActive: { equals: true } }],
    });
  });

  it('andWith on no filters', () => {
    expect(
      toPrismaWhere(
        ProposalContentResource,
        andWith(null, makeCond(ProposalContentResource, ['is_draft'], '$eq', false)),
      ),
    ).toEqual({ isDraft: { equals: false } });
  });

  it('makeCond rejects unknown paths', () => {
    expect(() => makeCond(ProposalContentResource, ['nope'], '$eq', 1)).toThrow(/not a path/);
  });
});
