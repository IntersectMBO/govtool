import { ApiError } from '../common/errors';
import { POSTS } from './__fixtures__/resources';
import { parseQuery } from './parse';
import { parseQueryString } from './raw-query';
import { ParsedQuery, PopulateTree } from './types';

const q = (s: string): ParsedQuery => parseQuery(parseQueryString(s), POSTS);

function err(s: string): string {
  try {
    q(s);
  } catch (e) {
    if (e instanceof ApiError) return `${e.status} ${e.errorName}: ${e.message}`;
    throw e;
  }
  return 'no error';
}
const V = (m: string) => `400 ValidationError: ${m}`;

/** Populate tree as nested plain objects for readable assertions. */
function tree(t: PopulateTree): Record<string, unknown> {
  const o: Record<string, unknown> = {};
  for (const [k, n] of t) o[k] = { fields: n.fields, children: tree(n.children) };
  return o;
}

describe('parseQuery: top level', () => {
  it('accepts an empty query with defaults', () => {
    expect(q('')).toEqual({
      filters: null,
      sort: [],
      populate: new Map(),
      fields: null,
      pagination: { kind: 'page', page: 1, pageSize: 25, withCount: true },
    });
  });

  it.each(['publicationState=live', 'locale=en', '_q=x', 'foo[bar]=1'])(
    'rejects unknown key %s (Δ7)',
    (s) => {
      expect(err(s)).toBe(V(`Invalid query parameter: ${s.split(/[=[]/)[0]}`));
    },
  );
});

describe('parseQuery: filters', () => {
  it('implicit $eq', () => {
    expect(q('filters[title]=hello').filters).toEqual({
      kind: 'and',
      children: [{ kind: 'cond', path: ['title'], op: '$eq', value: 'hello' }],
    });
  });

  it('explicit operators and siblings AND together', () => {
    expect(q('filters[likes][$gt]=1&filters[likes][$lte]=5&filters[is_live]=true').filters?.children).toEqual(
      [
        { kind: 'cond', path: ['likes'], op: '$gt', value: 1 },
        { kind: 'cond', path: ['likes'], op: '$lte', value: 5 },
        { kind: 'cond', path: ['is_live'], op: '$eq', value: true },
      ],
    );
  });

  it('$and as array and as index-keyed object', () => {
    const arr = q('filters[$and][0][title]=a&filters[$and][1][likes]=2').filters;
    expect(arr).toEqual({
      kind: 'and',
      children: [
        {
          kind: 'and',
          children: [
            { kind: 'and', children: [{ kind: 'cond', path: ['title'], op: '$eq', value: 'a' }] },
            { kind: 'and', children: [{ kind: 'cond', path: ['likes'], op: '$eq', value: 2 }] },
          ],
        },
      ],
    });
    // Indices beyond qs's arrayLimit (100) arrive as an object.
    const obj = q('filters[$and][150][title]=a&filters[$and][200][likes]=2').filters;
    expect(obj).toEqual(arr);
  });

  it('$or and $not', () => {
    expect(q('filters[$or][0][title]=a&filters[$or][1][title]=b').filters?.children[0]).toMatchObject({
      kind: 'or',
      children: [{ kind: 'and' }, { kind: 'and' }],
    });
    expect(q('filters[$not][title]=a').filters?.children[0]).toEqual({
      kind: 'not',
      child: { kind: 'and', children: [{ kind: 'cond', path: ['title'], op: '$eq', value: 'a' }] },
    });
    expect(q('filters[title][$not][$eq]=a').filters?.children[0]).toMatchObject({ kind: 'not' });
  });

  it('nested relation paths', () => {
    expect(q('filters[section][kind][id]=3').filters?.children).toEqual([
      { kind: 'cond', path: ['section', 'kind', 'id'], op: '$eq', value: 3 },
    ]);
    expect(q('filters[creator][govtool_username][$containsi]=Al').filters?.children).toEqual([
      { kind: 'cond', path: ['creator', 'govtool_username'], op: '$containsi', value: 'Al' },
    ]);
  });

  it('a scalar or operator on a relation compares its id', () => {
    expect(q('filters[creator]=5').filters?.children).toEqual([
      { kind: 'cond', path: ['creator', 'id'], op: '$eq', value: 5 },
    ]);
    expect(q('filters[creator][$ne]=5').filters?.children).toEqual([
      { kind: 'cond', path: ['creator', 'id'], op: '$ne', value: 5 },
    ]);
    expect(q('filters[section][kind]=2').filters?.children).toEqual([
      { kind: 'cond', path: ['section', 'kind', 'id'], op: '$eq', value: 2 },
    ]);
  });

  it('legacy string ids are coerced to integers', () => {
    expect(q('filters[parent_id][$eq]=12').filters?.children[0]).toMatchObject({ value: 12 });
    expect(err('filters[parent_id]=abc')).toBe(V('Invalid value for parent_id'));
    expect(err('filters[parent_id]=-1')).toBe(V('Invalid value for parent_id'));
    expect(err('filters[parent_id]=1.5')).toBe(V('Invalid value for parent_id'));
    expect(err('filters[parent_id]=99999999999')).toBe(V('Invalid value for parent_id'));
  });

  it('coerces booleans, dates, floats', () => {
    expect(q('filters[is_live]=TRUE').filters?.children[0]).toMatchObject({ value: true });
    expect(q('filters[is_live]=0').filters?.children[0]).toMatchObject({ value: false });
    expect(err('filters[is_live]=yes')).toBe(V('Invalid value for is_live'));
    expect(q('filters[closed_at][$gt]=2026-01-02T03:04:05.000Z').filters?.children[0]).toMatchObject({
      value: new Date('2026-01-02T03:04:05.000Z'),
    });
    expect(q('filters[published_on]=2026-01-02').filters?.children[0]).toMatchObject({
      value: new Date('2026-01-02T00:00:00.000Z'),
    });
    expect(err('filters[closed_at]=yesterday')).toBe(V('Invalid value for closed_at'));
    expect(q('filters[ratio][$lt]=0.5').filters?.children[0]).toMatchObject({ value: 0.5 });
    expect(err('filters[ratio]=x')).toBe(V('Invalid value for ratio'));
  });

  it('$in / $notIn as array or comma list, coerced', () => {
    expect(q('filters[id][$in][0]=1&filters[id][$in][1]=2').filters?.children[0]).toMatchObject({
      op: '$in',
      value: [1, 2],
    });
    expect(q('filters[id][$notIn]=3,4').filters?.children[0]).toMatchObject({ op: '$notIn', value: [3, 4] });
    expect(q('filters[id]=1&filters[id]=2').filters?.children[0]).toMatchObject({ op: '$in', value: [1, 2] });
    expect(err('filters[id][$in]=1,x')).toBe(V('Invalid value for id'));
  });

  it('$null / $notNull take booleans', () => {
    expect(q('filters[parent_id][$null]=true').filters?.children[0]).toMatchObject({
      op: '$null',
      value: true,
    });
    expect(q('filters[parent_id][$notNull]=false').filters?.children[0]).toMatchObject({
      op: '$notNull',
      value: false,
    });
    expect(err('filters[parent_id][$null]=maybe')).toBe(V('Invalid value for parent_id'));
  });

  it('string operators on string fields only', () => {
    for (const op of [
      '$contains',
      '$notContains',
      '$containsi',
      '$notContainsi',
      '$startsWith',
      '$endsWith',
    ]) {
      expect(q(`filters[title][${op}]=x`).filters?.children[0]).toMatchObject({ op, value: 'x' });
      expect(err(`filters[likes][${op}]=1`)).toBe(V(`Invalid operator ${op}`));
    }
  });

  it('ordering operators not on booleans (Prisma cannot order them)', () => {
    for (const op of ['$lt', '$lte', '$gt', '$gte']) {
      expect(err(`filters[is_live][${op}]=true`)).toBe(V(`Invalid operator ${op}`));
    }
    expect(q('filters[is_live][$in]=true,false').filters?.children[0]).toMatchObject({
      value: [true, false],
    });
  });

  it('every operator of §4.3 is accepted', () => {
    for (const op of ['$eq', '$ne', '$lt', '$lte', '$gt', '$gte']) {
      expect(q(`filters[likes][${op}]=1`).filters?.children[0]).toMatchObject({ op, value: 1 });
    }
  });

  it('unknown operator', () => {
    expect(err('filters[title][$like]=x')).toBe(V('Invalid operator $like'));
    expect(err('filters[$xor][0][title]=x')).toBe(V('Invalid operator $xor'));
  });

  it('unknown or non-allowlisted keys', () => {
    expect(err('filters[nope]=1')).toBe(V('Invalid key nope'));
    expect(err('filters[creator][username]=e0')).toBe(V('Invalid key creator.username'));
    expect(err('filters[creator][email]=x')).toBe(V('Invalid key creator.email'));
    expect(err('filters[section][kind][kind_name]=x')).toBe(V('Invalid key section.kind.kind_name'));
    expect(err('filters[title][sub]=x')).toBe(V('Invalid key title.sub'));
    // Hidden scalars resolve only where allowlisted, with their op limits.
    expect(q('filters[tags][secret][$eq]=h').filters?.children[0]).toMatchObject({
      path: ['tags', 'secret'],
    });
    expect(err('filters[tags][secret][$containsi]=h')).toBe(V('Invalid operator $containsi'));
    // JSON columns are not filterable.
    expect(err('filters[blob]=x')).toBe(V('Invalid key blob'));
  });

  it('virtual filters parse and keep their wire path', () => {
    expect(q('filters[$and][0][post_id]=7').filters?.children[0]).toMatchObject({
      kind: 'and',
      children: [{ kind: 'and', children: [{ kind: 'cond', path: ['post_id'], value: 7 }] }],
    });
  });

  it('limits: $and length, nesting, $in size', () => {
    const many = Array.from({ length: 21 }, (_, i) => `filters[$or][${i}][likes]=${i}`).join('&');
    expect(err(many)).toBe(V('Query too complex'));
    const twenty = Array.from({ length: 20 }, (_, i) => `filters[$or][${i}][likes]=${i}`).join('&');
    expect(err(twenty)).toBe('no error');
    expect(err('filters[$and][0][$or][0][$and][0][$or][0][title]=x')).toBe('no error');
    expect(err('filters[$and][0][$or][0][$and][0][$or][0][$not][title]=x')).toBe(V('Query too complex'));
    const ins = Array.from({ length: 101 }, (_, i) => i + 1).join(',');
    expect(err(`filters[id][$in]=${ins}`)).toBe(V('Query too complex'));
  });
});

describe('parseQuery: sort', () => {
  const s = (x: string) => q(x).sort;

  it.each([
    ['sort=createdAt:desc', [{ path: ['createdAt'], direction: 'desc' }]],
    [
      'sort=title:asc,likes:DESC',
      [
        { path: ['title'], direction: 'asc' },
        { path: ['likes'], direction: 'desc' },
      ],
    ],
    ['sort=title', [{ path: ['title'], direction: 'asc' }]],
    ['sort[0]=createdAt:desc', [{ path: ['createdAt'], direction: 'desc' }]],
    ['sort[createdAt]=desc', [{ path: ['createdAt'], direction: 'desc' }]],
    ['sort[createdAt]=DESC', [{ path: ['createdAt'], direction: 'desc' }]],
    ['sort[section][title]=DESC', [{ path: ['section', 'title'], direction: 'desc' }]],
    ['sort[0][createdAt]=asc', [{ path: ['createdAt'], direction: 'asc' }]],
    ['sort=section.title:Asc', [{ path: ['section', 'title'], direction: 'asc' }]],
    [
      'sort[0]=likes:desc&sort[1]=id:asc',
      [
        { path: ['likes'], direction: 'desc' },
        { path: ['id'], direction: 'asc' },
      ],
    ],
  ])('%s', (x, expected) => {
    expect(s(x)).toEqual(expected);
  });

  it('bad direction', () => {
    expect(err('sort=title:up')).toBe(V('Invalid order direction'));
    expect(err('sort[title]=sideways')).toBe(V('Invalid order direction'));
  });

  it('non-allowlisted, private or to-many paths', () => {
    expect(err('sort=nope:asc')).toBe(V('Invalid key nope'));
    expect(err('sort[creator][email]=asc')).toBe(V('Invalid key creator.email'));
    expect(err('sort[creator][username]=asc')).toBe(V('Invalid key creator.username'));
    // Allowlisted but through a to-many relation: deep sort joins to-one only.
    expect(err('sort[tags][label]=asc')).toBe(V('Invalid key tags.label'));
  });
});

describe('parseQuery: populate', () => {
  const p = (x: string) => tree(q(x).populate);
  const leaf = (children = {}) => ({ fields: null, children });

  it.each([
    ['populate=*', { creator: leaf(), section: leaf(), tags: leaf() }],
    ['populate=creator', { creator: leaf() }],
    ['populate=creator,tags', { creator: leaf(), tags: leaf() }],
    ['populate=section.kind', { section: leaf({ kind: leaf() }) }],
    ['populate[0]=section.kind&populate[1]=creator', { section: leaf({ kind: leaf() }), creator: leaf() }],
    ['populate[creator]=*', { creator: leaf() }],
    ['populate[creator]=true', { creator: leaf() }],
    ['populate[section][populate][kind]=*', { section: leaf({ kind: leaf() }) }],
    ['populate[section][populate][0]=kind', { section: leaf({ kind: leaf() }) }],
    ['populate[section][populate]=*', { section: leaf({ kind: leaf() }) }],
  ])('%s', (x, expected) => {
    expect(p(x)).toEqual(expected);
  });

  it('fields on a relation, and user fields intersected with the public projection (Δ2)', () => {
    expect(p('populate[section][fields][0]=title')).toEqual({ section: { fields: ['title'], children: {} } });
    expect(p('populate[tags][populate][owner][fields][0]=username')).toEqual({
      tags: leaf({ owner: { fields: [], children: {} } }),
    });
    expect(p('populate[creator][fields][0]=govtool_username')).toEqual({
      creator: { fields: ['govtool_username'], children: {} },
    });
    expect(err('populate[creator][fields][0]=email')).toBe(V('Invalid key creator.email'));
  });

  it('ignored paths are accepted; their populatable owner still populates', () => {
    expect(p('populate[tags][populate][maintainer][fields][0]=username')).toEqual({ tags: leaf() });
    expect(p('populate[0]=section.links')).toEqual({ section: leaf() });
  });

  it('rejects unknown, private and too-deep paths', () => {
    expect(err('populate=secret_stuff')).toBe(V('Invalid populate secret_stuff'));
    expect(err('populate=tags.owner.bogus')).toBe(V('Invalid populate tags.owner.bogus'));
    expect(err('populate[section][sort]=x')).toBe(V('Invalid populate section.sort'));
    expect(err('populate=a.b.c.d')).toBe(V('Query too complex'));
  });
});

describe('parseQuery: fields', () => {
  it('both forms', () => {
    expect(q('fields[0]=title&fields[1]=createdAt').fields).toEqual(['title', 'createdAt']);
    expect(q('fields=title,likes').fields).toEqual(['title', 'likes']);
  });
  it('unknown field', () => {
    expect(err('fields[0]=nope')).toBe(V('Invalid key nope'));
  });
  it('disabled per endpoint', () => {
    expect(() => parseQuery(parseQueryString('fields[0]=title'), { ...POSTS, fields: false })).toThrow(
      'Invalid query parameter: fields',
    );
  });
});

describe('parseQuery: pagination', () => {
  const pg = (x: string) => q(x).pagination;

  it('page form with defaults and clamping', () => {
    expect(pg('pagination[page]=3')).toEqual({ kind: 'page', page: 3, pageSize: 25, withCount: true });
    expect(pg('pagination[pageSize]=1000')).toEqual({
      kind: 'page',
      page: 1,
      pageSize: 1000,
      withCount: true,
    });
    expect(pg('pagination[pageSize]=5000')).toMatchObject({ pageSize: 1000 });
  });

  it('start form, -1 and clamping', () => {
    expect(pg('pagination[start]=10')).toEqual({ kind: 'offset', start: 10, limit: 25, withCount: true });
    expect(pg('pagination[limit]=-1')).toMatchObject({ start: 0, limit: 1000 });
    expect(pg('pagination[limit]=2000')).toMatchObject({ limit: 1000 });
  });

  it('withCount=false', () => {
    expect(pg('pagination[withCount]=false')).toMatchObject({ withCount: false });
  });

  it.each([
    'pagination[page]=0',
    'pagination[pageSize]=0',
    'pagination[page]=x',
    'pagination[page]=1.5',
    'pagination[page]=1&pagination[start]=0',
    'pagination[limit]=0',
    'pagination[start]=-1',
    'pagination[cursor]=1',
    'pagination=5',
    'pagination[withCount]=maybe',
  ])('%s → Invalid pagination', (x) => {
    expect(err(x)).toBe(V('Invalid pagination'));
  });
});

describe('parseQueryString decoding (§4.1)', () => {
  it('+ is a space, %XX decodes, malformed % stays literal', () => {
    expect(q('filters[title][$containsi]=a+b').filters?.children[0]).toMatchObject({ value: 'a b' });
    expect(q('filters[title][$containsi]=a%20b%21').filters?.children[0]).toMatchObject({ value: 'a b!' });
    expect(q('filters[title][$containsi]=100%').filters?.children[0]).toMatchObject({ value: '100%' });
    expect(q('filters[title][$containsi]=%zz_').filters?.children[0]).toMatchObject({ value: '%zz_' });
  });

  it('an unencoded & splits the pair and a # ends the query', () => {
    expect(err('filters[title][$containsi]=a&b')).toBe(V('Invalid query parameter: b'));
    expect(q('filters[title][$containsi]=a#b&sort=nope').filters?.children[0]).toMatchObject({ value: 'a' });
    expect(q('filters[title][$containsi]=a#b&sort=nope').sort).toEqual([]);
  });

  it('literal brackets and $ need no encoding; encoded ones work too', () => {
    expect(q('filters%5Btitle%5D%5B%24eq%5D=x').filters?.children[0]).toMatchObject({
      op: '$eq',
      value: 'x',
    });
  });

  it('prototype keys are dropped', () => {
    expect(parseQueryString('__proto__[x]=1&constructor[prototype][y]=2')).toEqual({});
  });
});
