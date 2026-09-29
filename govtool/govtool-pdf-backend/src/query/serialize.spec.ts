import { POSTS, TKind, TPost } from './__fixtures__/resources';
import { parseQuery } from './parse';
import { parseQueryString } from './raw-query';
import { list, paginationMeta, serializeEntity, single } from './serialize';

const T = new Date('2026-09-26T10:00:00.000Z');
const U = new Date('2026-09-26T11:00:00.000Z');

const row = {
  id: 7,
  title: 'Hello',
  body: null,
  likes: 3,
  ratio: 0.5,
  isLive: false,
  parentId: 12,
  publishedOn: new Date('2026-01-02T00:00:00.000Z'),
  closedAt: null,
  blob: { a: [1, 2] },
  creatorId: 5,
  createdAt: T,
  updatedAt: U,
  links: [
    { id: 1, link: 'https://a', text: null, position: 0 },
    { id: 2, link: 'https://b', text: 'B', position: 1 },
  ],
  creator: { id: 5, username: 'e0secret', govtoolUsername: 'alice', blocked: false },
  section: null,
  tags: [{ id: 9, label: 'x', secret: 'shh', createdAt: T, updatedAt: U }],
};

const populate = (s: string) => parseQuery(parseQueryString(s), POSTS).populate;

describe('serializeEntity', () => {
  it('keeps nulls, stringifies legacy ids, formats dates, inlines components', () => {
    expect(serializeEntity(row, TPost)).toEqual({
      id: 7,
      attributes: {
        title: 'Hello',
        body: null,
        likes: 3,
        ratio: 0.5,
        is_live: false,
        parent_id: '12',
        published_on: '2026-01-02',
        closed_at: null,
        blob: { a: [1, 2] },
        createdAt: '2026-09-26T10:00:00.000Z',
        updatedAt: '2026-09-26T11:00:00.000Z',
        links: [
          { id: 1, url: 'https://a', text: null },
          { id: 2, url: 'https://b', text: 'B' },
        ],
      },
    });
  });

  it('a null legacy reference stays null', () => {
    expect(serializeEntity({ ...row, parentId: null }, TPost).attributes.parent_id).toBeNull();
  });

  it('relations appear only when populated, wrapped in data; users use the public projection', () => {
    const e = serializeEntity(row, TPost, {
      populate: populate('populate[0]=creator&populate[1]=section&populate[2]=tags'),
    });
    expect(e.attributes.creator).toEqual({ data: { id: 5, attributes: { govtool_username: 'alice' } } });
    expect(e.attributes.section).toEqual({ data: null });
    expect(e.attributes.tags).toEqual({
      data: [{ id: 9, attributes: { label: 'x', createdAt: T.toISOString(), updatedAt: U.toISOString() } }],
    });
    expect(JSON.stringify(e)).not.toContain('e0secret');
    expect(JSON.stringify(e)).not.toContain('shh');
    expect(serializeEntity(row, TPost).attributes).not.toHaveProperty('creator');
  });

  it('relation fields: username yields empty attributes (Δ2)', () => {
    const e = serializeEntity(row, TPost, { populate: populate('populate[creator][fields][0]=username') });
    expect(e.attributes.creator).toEqual({ data: { id: 5, attributes: {} } });
  });

  it('root fields limit scalars only; components, relations and extras are unaffected', () => {
    const e = serializeEntity(row, TPost, {
      fields: ['title'],
      populate: populate('populate=creator'),
      extra: { user_govtool_username: 'alice' },
    });
    expect(Object.keys(e.attributes).sort()).toEqual(['creator', 'links', 'title', 'user_govtool_username']);
  });

  it('nested relations follow the same rule', () => {
    const withSection = {
      ...row,
      section: {
        id: 4,
        title: 'S',
        createdAt: T,
        updatedAt: U,
        links: [],
        kind: { id: 2, name: 'K', createdAt: T, updatedAt: U, publishedAt: T },
      },
    };
    const e = serializeEntity(withSection, TPost, { populate: populate('populate=section.kind') });
    expect(e.attributes.section).toEqual({
      data: {
        id: 4,
        attributes: {
          title: 'S',
          createdAt: T.toISOString(),
          updatedAt: U.toISOString(),
          links: [],
          kind: {
            data: {
              id: 2,
              attributes: {
                kind_name: 'K',
                createdAt: T.toISOString(),
                updatedAt: U.toISOString(),
                publishedAt: T.toISOString(),
              },
            },
          },
        },
      },
    });
  });

  it('publishedAt falls back to createdAt', () => {
    expect(
      serializeEntity({ id: 1, name: 'K', createdAt: T, updatedAt: U }, TKind).attributes.publishedAt,
    ).toBe(T.toISOString());
  });
});

describe('envelopes', () => {
  it('single', () => {
    expect(single(null)).toEqual({ data: null, meta: {} });
  });

  it('page pagination with pageCount', () => {
    const p = { kind: 'page' as const, page: 2, pageSize: 25, withCount: true };
    expect(paginationMeta(p, 51)).toEqual({ page: 2, pageSize: 25, pageCount: 3, total: 51 });
    expect(paginationMeta(p, 0)).toEqual({ page: 2, pageSize: 25, pageCount: 0, total: 0 });
    expect(paginationMeta({ ...p, withCount: false }, null)).toEqual({ page: 2, pageSize: 25 });
  });

  it('start/limit pagination', () => {
    const p = { kind: 'offset' as const, start: 5, limit: 10, withCount: true };
    expect(paginationMeta(p, 7)).toEqual({ start: 5, limit: 10, total: 7 });
    expect(list([], paginationMeta(p, 0))).toEqual({
      data: [],
      meta: { pagination: { start: 5, limit: 10, total: 0 } },
    });
  });
});
