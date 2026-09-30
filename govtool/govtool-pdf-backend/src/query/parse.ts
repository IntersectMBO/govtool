// The `qs` object of a request to a validated ParsedQuery (SPEC §4). Every
// rejection is a ValidationError with the exact §4 message.

import { validationError } from '../common/errors';
import { ResourceDef, ScalarType, resolveRelation, resolveScalar, attributeScalars } from './resource';
import {
  AndNode,
  CondNode,
  FilterNode,
  LIMITS,
  OPERATORS,
  Operator,
  Pagination,
  ParsedQuery,
  PopulateNode,
  PopulateTree,
  QueryAllowlist,
  STRING_OPERATORS,
  SortDirection,
  SortItem,
} from './types';

const TOP_LEVEL = new Set(['filters', 'sort', 'populate', 'fields', 'pagination']);
const tooComplex = () => validationError('Query too complex');
const invalidKey = (path: string) => validationError(`Invalid key ${path}`);
const invalidValue = (path: string) => validationError(`Invalid value for ${path}`);
const invalidOperator = (op: string) => validationError(`Invalid operator ${op}`);
const invalidPagination = () => validationError('Invalid pagination');

type Obj = Record<string, unknown>;
const isObj = (v: unknown): v is Obj => typeof v === 'object' && v !== null && !Array.isArray(v);
const has = (o: object, k: string): boolean => Object.prototype.hasOwnProperty.call(o, k) as boolean;

/** An array, or an index-keyed object as `qs` produces past arrayLimit. */
function asList(v: unknown): unknown[] | null {
  if (Array.isArray(v)) return v as unknown[];
  if (isObj(v)) {
    const keys = Object.keys(v);
    if (keys.length > 0 && keys.every((k) => /^\d+$/.test(k))) {
      return keys.sort((a, b) => Number(a) - Number(b)).map((k): unknown => v[k]);
    }
  }
  return null;
}

// ---------------------------------------------------------------- allowlist

interface CompiledAllowlist {
  a: QueryAllowlist;
  filterable: Set<string>;
  sortable: Set<string>;
  populatable: Set<string>;
  ignoredPopulate: Set<string>;
}

const compiled = new WeakMap<QueryAllowlist, CompiledAllowlist>();

function compile(a: QueryAllowlist): CompiledAllowlist {
  const hit = compiled.get(a);
  if (hit) return hit;
  const filterable = new Set<string>();
  for (const p of a.filterable) {
    filterable.add(p);
    // A relation path alone means its id.
    const segs = p.split('.');
    let r: ResourceDef | undefined = a.resource;
    for (const s of segs) {
      const rel: ReturnType<typeof resolveRelation> = r ? resolveRelation(r, s) : undefined;
      r = rel ? rel.target() : undefined;
    }
    if (r) filterable.add(`${p}.id`);
  }
  // Virtual filters are filterable by declaration.
  for (const v of Object.keys(a.virtualFilters ?? {})) filterable.add(v);
  const populatable = new Set<string>();
  for (const p of a.populatable ?? []) {
    const segs = p.split('.');
    for (let i = 1; i <= segs.length; i++) populatable.add(segs.slice(0, i).join('.'));
  }
  const c: CompiledAllowlist = {
    a,
    filterable,
    sortable: new Set(a.sortable),
    populatable,
    ignoredPopulate: new Set(a.ignoredPopulate ?? []),
  };
  compiled.set(a, c);
  return c;
}

// ---------------------------------------------------------------- coercion

const INT_MAX = 2147483647;
const ISO_RE = /^\d{4}-\d{2}-\d{2}(?:[T ]\d{2}:\d{2}(?::\d{2}(?:\.\d{1,9})?)?(?:Z|[+-]\d{2}:?\d{2})?)?$/;

/** Coerce a filter value by the target type (§4.3); throws V on failure. */
export function coerceValue(raw: unknown, type: ScalarType, path: string): unknown {
  switch (type) {
    case 'int':
    case 'legacyId': {
      if (typeof raw === 'number' && Number.isInteger(raw) && raw >= 0 && raw <= INT_MAX) return raw;
      if (typeof raw !== 'string' || !/^\d+$/.test(raw)) throw invalidValue(path);
      const n = Number(raw);
      if (n > INT_MAX) throw invalidValue(path);
      return n;
    }
    case 'float': {
      if (typeof raw === 'number' && Number.isFinite(raw)) return raw;
      if (typeof raw !== 'string' || !/^-?\d+(?:\.\d+)?$/.test(raw)) throw invalidValue(path);
      return Number(raw);
    }
    case 'boolean': {
      if (typeof raw === 'boolean') return raw;
      if (typeof raw !== 'string') throw invalidValue(path);
      const l = raw.toLowerCase();
      if (l === 'true' || l === '1') return true;
      if (l === 'false' || l === '0') return false;
      throw invalidValue(path);
    }
    case 'datetime':
    case 'date': {
      if (raw instanceof Date && !Number.isNaN(raw.getTime())) return raw;
      if (typeof raw !== 'string' || !ISO_RE.test(raw)) throw invalidValue(path);
      const s = raw.length === 10 ? `${raw}T00:00:00.000Z` : raw.replace(' ', 'T');
      const d = new Date(s);
      if (Number.isNaN(d.getTime())) throw invalidValue(path);
      return d;
    }
    case 'string':
      // U+0000 cannot reach Postgres (it fails the query with a 500).
      if (typeof raw === 'string') {
        if (raw.includes('\u0000')) throw invalidValue(path);
        return raw;
      }
      if (typeof raw === 'number') return String(raw);
      throw invalidValue(path);
    case 'json':
      throw invalidKey(path);
  }
}

function coerceBool(raw: unknown, path: string): boolean {
  return coerceValue(raw, 'boolean', path) as boolean;
}

// ---------------------------------------------------------------- filters

interface FilterCtx {
  c: CompiledAllowlist;
}

function fieldType(ctx: FilterCtx, r: ResourceDef, prefix: string[], name: string): ScalarType | undefined {
  const s = resolveScalar(r, name);
  if (s) {
    // Hidden scalars resolve only when the allowlist names them explicitly.
    if (r.hidden && has(r.hidden, name) && !ctx.c.filterable.has([...prefix, name].join('.'))) {
      return undefined;
    }
    return s.type;
  }
  if (prefix.length === 0 && ctx.c.a.virtualFilters && has(ctx.c.a.virtualFilters, name)) {
    return ctx.c.a.virtualFilters[name];
  }
  return undefined;
}

function cond(ctx: FilterCtx, path: string[], op: Operator, raw: unknown, type: ScalarType): CondNode {
  const p = path.join('.');
  if (!ctx.c.filterable.has(p)) throw invalidKey(p);
  const allowedOps = ctx.c.a.filterOps?.[p];
  if (allowedOps && !allowedOps.includes(op)) throw invalidOperator(op);
  if (STRING_OPERATORS.includes(op) && type !== 'string') throw invalidOperator(op);
  // Prisma has no ordering on booleans; left to it, the query fails with a 500.
  if (type === 'boolean' && ['$lt', '$lte', '$gt', '$gte'].includes(op)) throw invalidOperator(op);
  if (type === 'json') throw invalidKey(p);
  let value: unknown;
  if (op === '$null' || op === '$notNull') {
    value = coerceBool(raw, p);
  } else if (op === '$in' || op === '$notIn') {
    const list = asList(raw) ?? (typeof raw === 'string' ? (raw === '' ? [] : raw.split(',')) : null);
    if (!list) throw invalidValue(p);
    if (list.length > LIMITS.inValues) throw tooComplex();
    value = list.map((v) => coerceValue(v, type, p));
  } else {
    if (Array.isArray(raw) || isObj(raw)) throw invalidValue(p);
    value = coerceValue(raw, type, p);
  }
  return { kind: 'cond', path, op, value };
}

const and = (children: FilterNode[]): AndNode => ({ kind: 'and', children });

/** Value(s) on a scalar path: implicit `$eq`, array → `$in`, operator object. */
function parseFieldValue(
  ctx: FilterCtx,
  v: unknown,
  path: string[],
  type: ScalarType,
  logicalDepth: number,
): FilterNode[] {
  if (Array.isArray(v)) return [cond(ctx, path, '$in', v, type)];
  if (!isObj(v)) return [cond(ctx, path, '$eq', v, type)];
  const out: FilterNode[] = [];
  for (const [k, val] of Object.entries(v)) {
    if (k === '$not') {
      if (logicalDepth + 1 > LIMITS.logicalDepth) throw tooComplex();
      out.push({ kind: 'not', child: and(parseFieldValue(ctx, val, path, type, logicalDepth + 1)) });
    } else if ((OPERATORS as readonly string[]).includes(k)) {
      out.push(cond(ctx, path, k as Operator, val, type));
    } else if (k.startsWith('$')) {
      throw invalidOperator(k);
    } else {
      throw invalidKey([...path, k].join('.'));
    }
  }
  return out;
}

function parseLogical(
  ctx: FilterCtx,
  r: ResourceDef,
  prefix: string[],
  key: '$and' | '$or',
  v: unknown,
  logicalDepth: number,
): FilterNode {
  if (logicalDepth + 1 > LIMITS.logicalDepth) throw tooComplex();
  const list = asList(v);
  if (!list) throw invalidValue(key);
  if (list.length > LIMITS.logicalArray) throw tooComplex();
  const children = list.map((el) => {
    if (!isObj(el)) throw invalidValue(key);
    return and(parseFilterObject(ctx, r, prefix, el, logicalDepth + 1));
  });
  return { kind: key === '$and' ? 'and' : 'or', children };
}

function parseFilterObject(
  ctx: FilterCtx,
  r: ResourceDef,
  prefix: string[],
  obj: Obj,
  logicalDepth: number,
): FilterNode[] {
  const out: FilterNode[] = [];
  for (const [k, v] of Object.entries(obj)) {
    if (k === '$and' || k === '$or') {
      out.push(parseLogical(ctx, r, prefix, k, v, logicalDepth));
      continue;
    }
    if (k === '$not') {
      if (logicalDepth + 1 > LIMITS.logicalDepth) throw tooComplex();
      if (!isObj(v)) throw invalidValue('$not');
      out.push({ kind: 'not', child: and(parseFilterObject(ctx, r, prefix, v, logicalDepth + 1)) });
      continue;
    }
    if (k.startsWith('$')) {
      // An operator directly on a relation compares the related id.
      if (prefix.length > 0 && (OPERATORS as readonly string[]).includes(k)) {
        out.push(cond(ctx, [...prefix, 'id'], k as Operator, v, 'int'));
        continue;
      }
      throw invalidOperator(k);
    }
    const path = [...prefix, k];
    const type = fieldType(ctx, r, prefix, k);
    if (type) {
      out.push(...parseFieldValue(ctx, v, path, type, logicalDepth));
      continue;
    }
    const rel = resolveRelation(r, k);
    if (rel) {
      if (!isObj(v)) {
        out.push(...parseFieldValue(ctx, v, [...path, 'id'], 'int', logicalDepth));
      } else {
        out.push(...parseFilterObject(ctx, rel.target(), path, v, logicalDepth));
      }
      continue;
    }
    throw invalidKey(path.join('.'));
  }
  return out;
}

export function parseFilters(raw: unknown, allowlist: QueryAllowlist): AndNode | null {
  if (raw === undefined) return null;
  if (!isObj(raw)) throw invalidValue('filters');
  const ctx: FilterCtx = { c: compile(allowlist) };
  const children = parseFilterObject(ctx, allowlist.resource, [], raw, 0);
  return children.length ? and(children) : null;
}

/**
 * Build one condition as the parser would, for endpoint rewrites (e.g.
 * /proposals adding `prop_rev_active = true`). Bypasses the allowlist.
 */
export function makeCond(resource: ResourceDef, path: string[], op: Operator, raw: unknown): CondNode {
  let r = resource;
  for (const seg of path.slice(0, -1)) {
    const rel = resolveRelation(r, seg);
    if (!rel) throw new Error(`makeCond: ${path.join('.')} is not a path of ${resource.name}`);
    r = rel.target();
  }
  const s = resolveScalar(r, path[path.length - 1]);
  if (!s) throw new Error(`makeCond: ${path.join('.')} is not a path of ${resource.name}`);
  const p = path.join('.');
  const value =
    op === '$null' || op === '$notNull'
      ? coerceBool(raw, p)
      : op === '$in' || op === '$notIn'
        ? (raw as unknown[]).map((x) => coerceValue(x, s.type, p))
        : coerceValue(raw, s.type, p);
  return { kind: 'cond', path, op, value };
}

// ---------------------------------------------------------------- sort

function direction(raw: unknown): SortDirection {
  if (typeof raw !== 'string') throw validationError('Invalid order direction');
  const l = raw.trim().toLowerCase();
  if (l === 'asc' || l === 'desc') return l;
  throw validationError('Invalid order direction');
}

function sortFromString(s: string): SortItem[] {
  return s
    .split(',')
    .map((x) => x.trim())
    .filter((x) => x !== '')
    .map((part) => {
      const i = part.indexOf(':');
      const field = i < 0 ? part : part.slice(0, i);
      const dir = i < 0 ? 'asc' : direction(part.slice(i + 1));
      return { path: field.split('.'), direction: dir };
    });
}

function sortFromObject(o: Obj, prefix: string[]): SortItem[] {
  const out: SortItem[] = [];
  for (const [k, v] of Object.entries(o)) {
    if (isObj(v)) out.push(...sortFromObject(v, [...prefix, k]));
    else out.push({ path: [...prefix, ...k.split('.')], direction: direction(v) });
  }
  return out;
}

export function parseSort(raw: unknown, allowlist: QueryAllowlist): SortItem[] {
  if (raw === undefined) return [];
  let items: SortItem[];
  if (typeof raw === 'string') items = sortFromString(raw);
  else if (Array.isArray(raw)) {
    items = raw.flatMap((el) => {
      if (typeof el === 'string') return sortFromString(el);
      if (isObj(el)) return sortFromObject(el, []);
      throw validationError('Invalid order direction');
    });
  } else if (isObj(raw)) {
    const list = asList(raw);
    items = list
      ? list.flatMap((el) => {
          if (typeof el === 'string') return sortFromString(el);
          if (isObj(el)) return sortFromObject(el, []);
          throw validationError('Invalid order direction');
        })
      : sortFromObject(raw, []);
  } else {
    throw validationError('Invalid order direction');
  }
  const c = compile(allowlist);
  for (const it of items) {
    const p = it.path.join('.');
    if (!c.sortable.has(p)) throw invalidKey(p);
    assertToOnePath(allowlist.resource, it.path, p);
  }
  return items;
}

function assertToOnePath(resource: ResourceDef, path: string[], p: string): void {
  let r = resource;
  for (const seg of path.slice(0, -1)) {
    const rel = resolveRelation(r, seg);
    if (!rel || rel.many) throw invalidKey(p);
    r = rel.target();
  }
  if (!resolveScalar(r, path[path.length - 1])) throw invalidKey(p);
}

// ---------------------------------------------------------------- fields

function fieldList(raw: unknown, where: string): string[] {
  let list: unknown[];
  if (typeof raw === 'string')
    list = raw
      .split(',')
      .map((s) => s.trim())
      .filter((s) => s !== '');
  else {
    const l = asList(raw);
    if (!l) throw invalidKey(where);
    list = l;
  }
  return list.map((f) => {
    if (typeof f !== 'string') throw invalidKey(where);
    return f;
  });
}

/** Validate a fields selection against a resource's public scalars. */
function checkFields(r: ResourceDef, fields: string[], prefix: string): string[] {
  const allowed = new Set(['id', ...attributeScalars(r).map(([n]) => n)]);
  const noop = new Set(r.noopFields ?? []);
  const out: string[] = [];
  for (const f of fields) {
    if (allowed.has(f)) out.push(f);
    else if (!noop.has(f)) throw invalidKey(prefix ? `${prefix}.${f}` : f);
  }
  return out;
}

export function parseFields(raw: unknown, allowlist: QueryAllowlist): string[] | null {
  if (raw === undefined) return null;
  if (allowlist.fields === false) throw validationError('Invalid query parameter: fields');
  return checkFields(allowlist.resource, fieldList(raw, 'fields'), '');
}

// ---------------------------------------------------------------- populate

interface PopCtx {
  c: CompiledAllowlist;
}

function childrenOf(c: CompiledAllowlist, prefix: string): string[] {
  const depth = prefix === '' ? 1 : prefix.split('.').length + 1;
  return [...c.populatable]
    .filter((p) => p.split('.').length === depth && (prefix === '' || p.startsWith(`${prefix}.`)))
    .map((p) => p.split('.')[depth - 1]);
}

function isIgnored(c: CompiledAllowlist, path: string): boolean {
  for (const ig of c.ignoredPopulate) {
    if (path === ig || path.startsWith(`${ig}.`)) return true;
  }
  return false;
}

function targetOf(resource: ResourceDef, path: string[]): ResourceDef {
  let r = resource;
  for (const seg of path) {
    const rel = resolveRelation(r, seg);
    if (!rel) throw validationError(`Invalid populate ${path.join('.')}`);
    r = rel.target();
  }
  return r;
}

/** Ensure the node for `path` exists (creating its ancestors); null when ignored. */
function ensure(ctx: PopCtx, tree: PopulateTree, path: string[]): PopulateNode | null {
  if (path.length > LIMITS.populateDepth) throw tooComplex();
  const p = path.join('.');
  if (isIgnored(ctx.c, p)) {
    // `bd_further_information.proposal_links`: the component is a no-op but
    // its owner is still populated.
    for (let i = path.length - 1; i > 0; i--) {
      const prefix = path.slice(0, i);
      const pp = prefix.join('.');
      if (!isIgnored(ctx.c, pp) && ctx.c.populatable.has(pp)) {
        ensure(ctx, tree, prefix);
        break;
      }
    }
    return null;
  }
  if (!ctx.c.populatable.has(p)) throw validationError(`Invalid populate ${p}`);
  let level = tree;
  let node: PopulateNode | undefined;
  for (const seg of path) {
    node = level.get(seg);
    if (!node) {
      node = { fields: null, children: new Map() };
      level.set(seg, node);
    }
    level = node.children;
  }
  return node ?? null;
}

function addPathString(ctx: PopCtx, tree: PopulateTree, prefix: string[], s: string): void {
  for (const part of s
    .split(',')
    .map((x) => x.trim())
    .filter((x) => x !== '')) {
    if (part === '*') {
      for (const child of childrenOf(ctx.c, prefix.join('.'))) ensure(ctx, tree, [...prefix, child]);
      continue;
    }
    ensure(ctx, tree, [...prefix, ...part.split('.')]);
  }
}

function addPopulateValue(ctx: PopCtx, tree: PopulateTree, prefix: string[], v: unknown): void {
  if (typeof v === 'string') return addPathString(ctx, tree, prefix, v);
  const list = asList(v);
  if (list) {
    for (const el of list) addPopulateValue(ctx, tree, prefix, el);
    return;
  }
  if (!isObj(v)) throw validationError(`Invalid populate ${prefix.join('.') || 'populate'}`);
  for (const [k, spec] of Object.entries(v)) {
    const path = [...prefix, k];
    const p = path.join('.');
    if (spec === false || spec === 'false') continue;
    const node = ensure(ctx, tree, path);
    if (!node) continue;
    if (spec === '*' || spec === true || spec === 'true') continue;
    if (!isObj(spec)) throw validationError(`Invalid populate ${p}`);
    for (const [sk, sv] of Object.entries(spec)) {
      if (sk === 'populate') {
        if (sv === '*' || sv === 'true' || sv === true) {
          for (const child of childrenOf(ctx.c, p)) ensure(ctx, tree, [...path, child]);
        } else {
          addPopulateValue(ctx, tree, path, sv);
        }
      } else if (sk === 'fields') {
        const target = targetOf(ctx.c.a.resource, path);
        node.fields = checkFields(target, fieldList(sv, `${p}.fields`), p);
      } else {
        throw validationError(`Invalid populate ${p}.${sk}`);
      }
    }
  }
}

export function parsePopulate(raw: unknown, allowlist: QueryAllowlist): PopulateTree {
  const tree: PopulateTree = new Map();
  if (raw === undefined) return tree;
  addPopulateValue({ c: compile(allowlist) }, tree, [], raw);
  return tree;
}

// ---------------------------------------------------------------- pagination

function pageInt(v: unknown, min: number): number {
  if (typeof v !== 'string' || !/^-?\d+$/.test(v)) throw invalidPagination();
  const n = Number(v);
  if (!Number.isSafeInteger(n) || n < min) throw invalidPagination();
  return n;
}

export function parsePagination(raw: unknown): Pagination {
  if (raw === undefined) {
    return { kind: 'page', page: 1, pageSize: LIMITS.defaultPageSize, withCount: true };
  }
  if (!isObj(raw)) throw invalidPagination();
  for (const k of Object.keys(raw)) {
    if (!['page', 'pageSize', 'start', 'limit', 'withCount'].includes(k)) throw invalidPagination();
  }
  let withCount = true;
  if (raw.withCount !== undefined) {
    const w = typeof raw.withCount === 'string' ? raw.withCount.toLowerCase() : raw.withCount;
    if (w === 'true' || w === '1') withCount = true;
    else if (w === 'false' || w === '0') withCount = false;
    else throw invalidPagination();
  }
  const pageForm = raw.page !== undefined || raw.pageSize !== undefined;
  const offsetForm = raw.start !== undefined || raw.limit !== undefined;
  if (pageForm && offsetForm) throw invalidPagination();
  if (offsetForm) {
    const start = raw.start === undefined ? 0 : pageInt(raw.start, 0);
    let limit = raw.limit === undefined ? LIMITS.defaultPageSize : pageInt(raw.limit, -1);
    if (limit === 0) throw invalidPagination();
    if (limit === -1 || limit > LIMITS.maxPageSize) limit = LIMITS.maxPageSize;
    return { kind: 'offset', start, limit, withCount };
  }
  const page = raw.page === undefined ? 1 : pageInt(raw.page, 1);
  let pageSize = raw.pageSize === undefined ? LIMITS.defaultPageSize : pageInt(raw.pageSize, 1);
  if (pageSize > LIMITS.maxPageSize) pageSize = LIMITS.maxPageSize;
  return { kind: 'page', page, pageSize, withCount };
}

// ---------------------------------------------------------------- entry point

/** Validate and parse the whole query object against an endpoint allowlist. */
export function parseQuery(raw: Obj, allowlist: QueryAllowlist): ParsedQuery {
  for (const k of Object.keys(raw)) {
    if (!TOP_LEVEL.has(k)) throw validationError(`Invalid query parameter: ${k}`);
  }
  return {
    filters: parseFilters(raw.filters, allowlist),
    sort: parseSort(raw.sort, allowlist),
    populate: parsePopulate(raw.populate, allowlist),
    fields: parseFields(raw.fields, allowlist),
    pagination: parsePagination(raw.pagination),
  };
}
