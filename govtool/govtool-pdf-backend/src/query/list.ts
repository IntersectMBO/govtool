// One call for a typical list route: ParsedQuery + extra where → rows + meta.

import { toPrismaInclude, toPrismaOrderBy, toPrismaPaging, toPrismaWhere, PrismaWhere } from './prisma';
import { ResourceDef } from './resource';
import { Entity, ListEnvelope, PaginationMeta, list, paginationMeta, serializeEntity } from './serialize';
import { ParsedQuery, SortItem } from './types';

/** The part of a Prisma model delegate a list needs. */
export interface ListDelegate {
  findMany(args: object): Promise<object[]>;
  count(args: object): Promise<number>;
}

/** `prisma.bdType` etc. typed loosely enough for findList. */
export function delegate(d: unknown): ListDelegate {
  return d as ListDelegate;
}

export interface FindListOptions {
  /** ANDed with the client filters (forced conditions such as the caller). */
  where?: PrismaWhere;
  /** Order when the client sends no sort (default `id asc`). */
  defaultSort?: SortItem[];
}

export interface FindListResult<R = object> {
  rows: R[];
  meta: PaginationMeta;
}

export async function findList<R = object>(
  d: ListDelegate,
  resource: ResourceDef,
  q: ParsedQuery,
  opts: FindListOptions = {},
): Promise<FindListResult<R>> {
  const clientWhere = toPrismaWhere(resource, q.filters);
  const parts = [clientWhere, opts.where ?? {}].filter((w) => Object.keys(w).length > 0);
  const where = parts.length === 0 ? {} : parts.length === 1 ? parts[0] : { AND: parts };
  const include = toPrismaInclude(resource, q.populate);
  const { skip, take } = toPrismaPaging(q.pagination);
  const [rows, total] = await Promise.all([
    d.findMany({
      where,
      orderBy: toPrismaOrderBy(resource, q.sort, opts.defaultSort),
      skip,
      take,
      ...(include ? { include } : {}),
    }),
    q.pagination.withCount ? d.count({ where }) : Promise.resolve(null),
  ]);
  return { rows: rows as R[], meta: paginationMeta(q.pagination, total) };
}

/** findList then the plain v4 list envelope. `extra` adds computed attributes. */
export async function listEnvelope(
  d: ListDelegate,
  resource: ResourceDef,
  q: ParsedQuery,
  opts: FindListOptions & { extra?: (row: object) => Record<string, unknown> } = {},
): Promise<ListEnvelope<Entity>> {
  const { rows, meta } = await findList(d, resource, q, opts);
  return list(
    rows.map((row) =>
      serializeEntity(row, resource, {
        populate: q.populate,
        fields: q.fields,
        extra: opts.extra ? opts.extra(row) : undefined,
      }),
    ),
    meta,
  );
}
