// Strapi v4 envelope serializer (SPEC §3.1–§3.4).

import { ResourceDef, ScalarType, attributeScalars, resolveRelation } from './resource';
import { Pagination, PopulateTree } from './types';

export interface Entity {
  id: number;
  attributes: Record<string, unknown>;
}
export interface SingleEnvelope<T = Entity | null> {
  data: T;
  meta: Record<string, never>;
}
export type PaginationMeta =
  | { page: number; pageSize: number; pageCount?: number; total?: number }
  | { start: number; limit: number; total?: number };
export interface ListEnvelope<T = Entity> {
  data: T[];
  meta: { pagination: PaginationMeta };
}

type Row = Record<string, unknown>;

/** A stored value to its wire form by type (§3.2). */
export function wireValue(v: unknown, type: ScalarType): unknown {
  if (v === null || v === undefined) return null;
  switch (type) {
    case 'legacyId':
      return typeof v === 'number' || typeof v === 'bigint' ? v.toString() : v;
    case 'datetime':
      return v instanceof Date ? v.toISOString() : v;
    case 'date':
      return v instanceof Date ? v.toISOString().slice(0, 10) : v;
    default:
      return v;
  }
}

export interface SerializeOptions {
  /** Relations to emit; unpopulated relations never appear (§3.3). */
  populate?: PopulateTree | null;
  /** Root attribute selection (`fields`); null for all. */
  fields?: string[] | null;
  /** Computed attributes (not stored), merged last. */
  extra?: Record<string, unknown>;
}

/** Scalars of a row as a plain object, `{wire: value}`; no id, no relations. */
export function serializeScalars(
  row: object,
  resource: ResourceDef,
  fields: string[] | null = null,
): Record<string, unknown> {
  const r = row as Row;
  const out: Record<string, unknown> = {};
  for (const [wire, def] of attributeScalars(resource)) {
    if (fields && !fields.includes(wire)) continue;
    let v = r[def.field];
    // publishedAt equals createdAt (§3.2) when the model has no column.
    if (wire === 'publishedAt' && (v === undefined || v === null)) v = r.createdAt;
    out[wire] = wireValue(v, def.type);
  }
  return out;
}

/** Components of a row, always inline (§3.3), in stored order. */
export function serializeComponents(row: object, resource: ResourceDef): Record<string, unknown> {
  const r = row as Row;
  const out: Record<string, unknown> = {};
  for (const [wire, comp] of Object.entries(resource.components ?? {})) {
    const items = (r[comp.field] as Row[] | undefined) ?? [];
    out[wire] = items.map((it) => {
      const o: Record<string, unknown> = { id: it.id };
      for (const [cw, def] of Object.entries(comp.scalars)) o[cw] = wireValue(it[def.field], def.type);
      return o;
    });
  }
  return out;
}

/** One row as `{id, attributes}` with components and populated relations. */
export function serializeEntity(row: object, resource: ResourceDef, opts: SerializeOptions = {}): Entity {
  const r = row as Row;
  const attributes: Record<string, unknown> = {
    ...serializeScalars(row, resource, opts.fields ?? null),
    ...serializeComponents(row, resource),
  };
  for (const [name, node] of opts.populate ?? []) {
    const rel = resolveRelation(resource, name);
    if (!rel) continue;
    const target = rel.target();
    const childOpts: SerializeOptions = { populate: node.children, fields: node.fields };
    const v = r[rel.field];
    if (rel.many) {
      const list = (v as object[] | undefined) ?? [];
      attributes[name] = { data: list.map((x) => serializeEntity(x, target, childOpts)) };
    } else {
      attributes[name] = { data: v ? serializeEntity(v, target, childOpts) : null };
    }
  }
  if (opts.extra) Object.assign(attributes, opts.extra);
  return { id: r.id as number, attributes };
}

export function single<T = Entity | null>(data: T): SingleEnvelope<T> {
  return { data, meta: {} };
}

export function paginationMeta(p: Pagination, total: number | null): PaginationMeta {
  if (p.kind === 'page') {
    const m: PaginationMeta = { page: p.page, pageSize: p.pageSize };
    if (p.withCount && total !== null) {
      m.pageCount = total === 0 ? 0 : Math.ceil(total / p.pageSize);
      m.total = total;
    }
    return m;
  }
  const m: PaginationMeta = { start: p.start, limit: p.limit };
  if (p.withCount && total !== null) m.total = total;
  return m;
}

export function list<T = Entity>(data: T[], pagination: PaginationMeta): ListEnvelope<T> {
  return { data, meta: { pagination } };
}
