// AST helpers for endpoint rewrites (SPEC §8.2 /proposals, §8.4 votes, ...).
// "Top-level" conditions are those reachable from the root through AND
// nodes only: the top-level keys of `filters` and of each `$and` element.

import { AndNode, CondNode, FilterNode } from './types';

function walkTop(node: FilterNode, visit: (c: CondNode) => void): void {
  if (node.kind === 'cond') visit(node);
  else if (node.kind === 'and') node.children.forEach((c) => walkTop(c, visit));
}

export function topLevelConditions(root: AndNode | null): CondNode[] {
  const out: CondNode[] = [];
  if (root) walkTop(root, (c) => out.push(c));
  return out;
}

const samePath = (a: string[], b: string) => a.join('.') === b;

/** Any top-level condition on `path` (dot-joined wire path). */
export function hasTopLevel(root: AndNode | null, path: string): boolean {
  return topLevelConditions(root).some((c) => samePath(c.path, path));
}

function removeFrom(node: FilterNode, pred: (c: CondNode) => boolean): FilterNode | null {
  if (node.kind === 'cond') return pred(node) ? null : node;
  if (node.kind !== 'and') return node;
  const children = node.children
    .map((c) => removeFrom(c, pred))
    .filter((c): c is FilterNode => c !== null && !(c.kind === 'and' && c.children.length === 0));
  return { kind: 'and', children };
}

/**
 * Drop every top-level condition on `path` (e.g. a client `user_id` that the
 * server replaces with the caller). Conditions under `$or`/`$not` are
 * untouched, so an endpoint that forces a value must also AND its own.
 */
export function removeTopLevel(root: AndNode | null, path: string): AndNode | null {
  if (!root) return null;
  return removeFrom(root, (c) => samePath(c.path, path)) as AndNode;
}

/** Rewrite every top-level condition on `from` to the same condition on `to`. */
export function renameTopLevel(root: AndNode | null, from: string, to: string[]): AndNode | null {
  if (!root) return null;
  walkTop(root, (c) => {
    if (samePath(c.path, from)) c.path = to;
  });
  return root;
}

/** AND extra conditions onto the root. */
export function andWith(root: AndNode | null, ...extra: FilterNode[]): AndNode {
  return { kind: 'and', children: [...(root ? [root] : []), ...extra] };
}

/** Whether any condition anywhere (including under `$or`/`$not`) is on `path`. */
export function mentions(node: FilterNode | null, path: string): boolean {
  if (!node) return false;
  if (node.kind === 'cond') return samePath(node.path, path);
  if (node.kind === 'not') return mentions(node.child, path);
  return node.children.some((c) => mentions(c, path));
}
