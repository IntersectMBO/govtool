// Small descriptors for the query unit tests (not used by the app).

import { col, defineResource, scalarPaths } from '../resource';
import { QueryAllowlist } from '../types';

export const TUser = defineResource({
  name: 'user',
  scalars: { govtool_username: col.str('govtoolUsername') },
  timestamps: false,
  noopFields: ['username'],
});

export const TTag = defineResource({
  name: 'tag',
  scalars: { label: col.str('label', false) },
  hidden: { secret: col.str('secret', false) },
  relations: { owner: { field: 'owner', target: () => TUser, many: false, fk: 'ownerId' } },
});

export const TKind = defineResource({
  name: 'kind',
  scalars: { kind_name: col.str('name', false) },
  publishedAt: true,
});

export const TSection = defineResource({
  name: 'section',
  scalars: { title: col.str('title') },
  relations: { kind: { field: 'kind', target: () => TKind, many: false, fk: 'kindId' } },
  components: { links: { field: 'links', scalars: { url: col.str('link', false) } } },
});

export const TPost = defineResource({
  name: 'post',
  scalars: {
    title: col.str('title', false),
    body: col.str('body'),
    likes: col.int('likes'),
    ratio: col.float('ratio'),
    is_live: col.bool('isLive'),
    parent_id: col.legacyId('parentId', true),
    published_on: col.date('publishedOn'),
    closed_at: col.datetime('closedAt'),
    blob: col.json('blob'),
  },
  relations: {
    creator: { field: 'creator', target: () => TUser, many: false, fk: 'creatorId', nullable: false },
    section: { field: 'section', target: () => TSection, many: false, fk: 'sectionId' },
    tags: { field: 'tags', target: () => TTag, many: true },
  },
  components: { links: { field: 'links', scalars: { url: col.str('link', false), text: col.str('text') } } },
});

export const POSTS: QueryAllowlist = {
  resource: TPost,
  filterable: [
    ...scalarPaths(TPost),
    'creator',
    'creator.govtool_username',
    'section.kind.id',
    'section.title',
    'tags.label',
    'tags.secret',
  ],
  filterOps: { 'tags.secret': ['$eq'] },
  virtualFilters: { post_id: 'int' },
  sortable: [...scalarPaths(TPost), 'section.title', 'creator.govtool_username', 'tags.label'],
  populatable: ['creator', 'section.kind', 'tags.owner'],
  ignoredPopulate: ['tags.maintainer', 'section.links'],
};
