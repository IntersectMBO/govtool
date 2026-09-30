import type { ResourceDef, ScalarType } from './resource';

export const OPERATORS = [
  '$eq',
  '$ne',
  '$lt',
  '$lte',
  '$gt',
  '$gte',
  '$in',
  '$notIn',
  '$null',
  '$notNull',
  '$contains',
  '$notContains',
  '$containsi',
  '$notContainsi',
  '$startsWith',
  '$endsWith',
] as const;
export type Operator = (typeof OPERATORS)[number];

export const STRING_OPERATORS: readonly Operator[] = [
  '$contains',
  '$notContains',
  '$containsi',
  '$notContainsi',
  '$startsWith',
  '$endsWith',
];

/** A single comparison on a wire path. `value` is already coerced. */
export interface CondNode {
  kind: 'cond';
  /** Wire path segments, e.g. `['bd_psapb', 'type_name', 'id']`. */
  path: string[];
  op: Operator;
  value: unknown;
}
export interface AndNode {
  kind: 'and';
  children: FilterNode[];
}
export interface OrNode {
  kind: 'or';
  children: FilterNode[];
}
export interface NotNode {
  kind: 'not';
  child: FilterNode;
}
export type FilterNode = CondNode | AndNode | OrNode | NotNode;

export type SortDirection = 'asc' | 'desc';
export interface SortItem {
  path: string[];
  direction: SortDirection;
}

export interface PopulateNode {
  /** `fields` selection on the relation, or null for all public scalars. */
  fields: string[] | null;
  children: PopulateTree;
}
export type PopulateTree = Map<string, PopulateNode>;

export type Pagination =
  | { kind: 'page'; page: number; pageSize: number; withCount: boolean }
  | { kind: 'offset'; start: number; limit: number; withCount: boolean };

export interface ParsedQuery {
  /** Root is always an AND of the top-level keys; null when no filters. */
  filters: AndNode | null;
  /** Empty when the client sent no sort. */
  sort: SortItem[];
  populate: PopulateTree;
  /** Root `fields` selection, or null for every public scalar. */
  fields: string[] | null;
  pagination: Pagination;
}

/**
 * Per-endpoint allowlist of SPEC §4.2. Paths are wire names joined by dots.
 * A filterable entry naming a relation (`creator`) allows `creator.id`.
 */
export interface QueryAllowlist {
  resource: ResourceDef;
  filterable: readonly string[];
  /** Restrict the operators on a filterable path (e.g. `comments_reports.hash`: `$eq`). */
  filterOps?: Readonly<Record<string, readonly Operator[]>>;
  /**
   * Filter keys that are not fields of the resource but that the endpoint
   * rewrites before translation (e.g. `prop_id` on /proposals). Filterable
   * by declaration; translating one that was not rewritten throws.
   */
  virtualFilters?: Readonly<Record<string, ScalarType>>;
  sortable: readonly string[];
  /** Populatable relation paths; every prefix is implied. Depth ≤ 3. */
  populatable?: readonly string[];
  /**
   * Populate paths pdf-ui sends that mean nothing here: accepted, dropped,
   * with anything below them.
   */
  ignoredPopulate?: readonly string[];
  /** `fields` allowed at the root. Default true. */
  fields?: boolean;
}

export const LIMITS = {
  logicalArray: 20,
  logicalDepth: 4,
  populateDepth: 3,
  inValues: 100,
  defaultPageSize: 25,
  maxPageSize: 1000,
} as const;
