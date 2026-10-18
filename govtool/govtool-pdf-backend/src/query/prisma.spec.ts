import { POSTS, TPost } from './__fixtures__/resources';
import { parseQuery } from './parse';
import { escapeLike, toPrismaInclude, toPrismaOrderBy, toPrismaPaging, toPrismaWhere } from './prisma';
import { parseQueryString } from './raw-query';

const parsed = (s: string) => parseQuery(parseQueryString(s), POSTS);
const where = (s: string) => toPrismaWhere(TPost, parsed(s).filters);

describe('toPrismaWhere', () => {
  it('empty', () => {
    expect(where('')).toEqual({});
  });

  it.each([
    ['filters[likes]=1', { likes: { equals: 1 } }],
    ['filters[likes][$ne]=1', { likes: { not: 1 } }],
    ['filters[likes][$lt]=1', { likes: { lt: 1 } }],
    ['filters[likes][$lte]=1', { likes: { lte: 1 } }],
    ['filters[likes][$gt]=1', { likes: { gt: 1 } }],
    ['filters[likes][$gte]=1', { likes: { gte: 1 } }],
    ['filters[likes][$in]=1,2', { likes: { in: [1, 2] } }],
    ['filters[likes][$notIn]=1,2', { likes: { notIn: [1, 2] } }],
    ['filters[parent_id][$null]=true', { parentId: { equals: null } }],
    ['filters[parent_id][$null]=false', { parentId: { not: null } }],
    ['filters[parent_id][$notNull]=true', { parentId: { not: null } }],
    ['filters[parent_id][$notNull]=false', { parentId: { equals: null } }],
    ['filters[title][$contains]=x', { title: { contains: 'x' } }],
    ['filters[title][$notContains]=x', { NOT: { title: { contains: 'x' } } }],
    ['filters[title][$containsi]=x', { title: { contains: 'x', mode: 'insensitive' } }],
    ['filters[title][$notContainsi]=x', { NOT: { title: { contains: 'x', mode: 'insensitive' } } }],
    ['filters[title][$startsWith]=x', { title: { startsWith: 'x' } }],
    ['filters[title][$endsWith]=x', { title: { endsWith: 'x' } }],
    ['filters[parent_id]=7', { parentId: { equals: 7 } }],
  ])('%s', (s, expected) => {
    expect(where(s)).toEqual(expected);
  });

  it('$null on a non-nullable column is constant', () => {
    // Not {OR: []} / {AND: []}: Prisma drops those inside another AND/OR.
    expect(where('filters[title][$null]=true')).toEqual({ id: { in: [] } });
    expect(where('filters[title][$null]=false')).toEqual({ id: { notIn: [] } });
  });

  it('LIKE wildcards and backslash match literally', () => {
    expect(escapeLike('50%_off\\')).toBe('50\\%\\_off\\\\');
    expect(where('filters[title][$containsi]=100%25_x')).toEqual({
      title: { contains: '100\\%\\_x', mode: 'insensitive' },
    });
  });

  it('empty $containsi matches every non-null value', () => {
    expect(where('filters[body][$containsi]=')).toEqual({ body: { contains: '', mode: 'insensitive' } });
  });

  it('relation id through the FK; deeper paths through `is`; to-many through `some`', () => {
    expect(where('filters[creator]=5')).toEqual({ creatorId: { equals: 5 } });
    expect(where('filters[section][kind][id]=3')).toEqual({ section: { is: { kindId: { equals: 3 } } } });
    expect(where('filters[creator][govtool_username][$eq]=al')).toEqual({
      creator: { is: { govtoolUsername: { equals: 'al' } } },
    });
    expect(where('filters[tags][label]=x')).toEqual({ tags: { some: { label: { equals: 'x' } } } });
    expect(where('filters[tags][secret][$eq]=h')).toEqual({ tags: { some: { secret: { equals: 'h' } } } });
  });

  it('logical nodes', () => {
    expect(where('filters[$and][0][likes]=1&filters[$and][1][title]=a')).toEqual({
      AND: [{ likes: { equals: 1 } }, { title: { equals: 'a' } }],
    });
    expect(where('filters[$or][0][likes]=1&filters[$or][1][title]=a')).toEqual({
      OR: [{ likes: { equals: 1 } }, { title: { equals: 'a' } }],
    });
    expect(where('filters[$not][likes]=1')).toEqual({ NOT: { likes: { equals: 1 } } });
    expect(where('filters[likes]=1&filters[title]=a')).toEqual({
      AND: [{ likes: { equals: 1 } }, { title: { equals: 'a' } }],
    });
  });

  it('refuses an unrewritten virtual filter', () => {
    expect(() => where('filters[post_id]=1')).toThrow(/virtual filters must be rewritten/);
  });
});

describe('toPrismaOrderBy', () => {
  const order = (s: string) => toPrismaOrderBy(TPost, parsed(s).sort);

  it('defaults to id asc', () => {
    expect(order('')).toEqual([{ id: 'asc' }]);
  });
  it('appends id asc as the tie-breaker', () => {
    expect(order('sort[createdAt]=DESC')).toEqual([{ createdAt: 'desc' }, { id: 'asc' }]);
  });
  it('does not repeat a trailing id', () => {
    expect(order('sort=likes:desc,id:desc')).toEqual([{ likes: 'desc' }, { id: 'desc' }]);
  });
  it('deep paths through to-one relations', () => {
    expect(order('sort[section][title]=ASC')).toEqual([{ section: { title: 'asc' } }, { id: 'asc' }]);
    expect(order('sort[creator][govtool_username]=desc')).toEqual([
      { creator: { govtoolUsername: 'desc' } },
      { id: 'asc' },
    ]);
  });
  it('endpoint fallback when the client sends none', () => {
    expect(toPrismaOrderBy(TPost, [], [{ path: ['createdAt'], direction: 'desc' }])).toEqual([
      { createdAt: 'desc' },
      { id: 'asc' },
    ]);
  });
});

describe('toPrismaInclude', () => {
  const inc = (s: string) => toPrismaInclude(TPost, parsed(s).populate);
  const links = { orderBy: [{ position: 'asc' }, { id: 'asc' }] };

  it('components are always included', () => {
    expect(inc('')).toEqual({ links });
  });
  it('relations, nested relations and nested components', () => {
    expect(inc('populate[0]=creator&populate[1]=section.kind&populate[2]=tags')).toEqual({
      links,
      creator: true,
      section: { include: { links, kind: true } },
      tags: { orderBy: { id: 'asc' } },
    });
  });
});

describe('toPrismaPaging', () => {
  it('page and offset forms', () => {
    expect(toPrismaPaging({ kind: 'page', page: 3, pageSize: 25, withCount: true })).toEqual({
      skip: 50,
      take: 25,
    });
    expect(toPrismaPaging({ kind: 'offset', start: 7, limit: 5, withCount: true })).toEqual({
      skip: 7,
      take: 5,
    });
  });
});
