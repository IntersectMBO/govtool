import {
  tryLegacyDRepCandidates,
  tryLegacyGovActionId,
} from '../common/legacy-ids';

/**
 * Which page a frontend URL is, and its canonical path. Both URL schemes are
 * recognised: the one on develop (/governance_actions, /drep_directory, ...)
 * and PR 4310's (/actions, /dreps, ...). `scheme` only picks which one the
 * canonical path is written in, which must be the one the deployed frontend
 * serves.
 */

export type PageKind =
  | 'home'
  | 'actionList'
  | 'action'
  | 'actionHistoryList'
  | 'drepList'
  | 'drep'
  | 'forumList'
  | 'forumProposal'
  | 'budget'
  | 'guides'
  | 'faqs'
  | 'wallet'
  | 'unknown';

export type UrlScheme = 'develop' | 'clean';

export type ResolvedPage = {
  kind: PageKind;
  id?: string;
  /** An action reached through the history pages. */
  history?: boolean;
  canonicalPath: string | null;
};

const MAX_PATH_LENGTH = 512;
const TX_HASH = /^[0-9a-fA-F]{64}$/;
const BUDGET_ID = /^[\w-]{1,64}$/;

export const isActionId = (id: string): boolean =>
  TX_HASH.test(id) || tryLegacyGovActionId(id) !== undefined;

export const isDRepId = (id: string): boolean =>
  tryLegacyDRepCandidates(id) !== undefined;

export const isForumId = (id: string): boolean => /^[1-9][0-9]{0,9}$/.test(id);

const PREFIXES: Record<UrlScheme, Record<string, string>> = {
  develop: {
    actions: '/governance_actions',
    dreps: '/drep_directory',
    proposals: '/proposal_discussion',
    budget: '/budget_discussion',
  },
  clean: {
    actions: '/actions',
    dreps: '/dreps',
    proposals: '/proposals',
    budget: '/budget',
  },
};

/** The canonical path of a page, for an id that may only be known later. */
export const canonicalPathFor = (
  kind: PageKind,
  id: string | undefined,
  scheme: UrlScheme,
  history: boolean,
): string | null => {
  const p = PREFIXES[scheme];
  const withId = (base: string) => (id === undefined ? base : `${base}/${id}`);
  switch (kind) {
    case 'home':
      return '/';
    case 'actionList':
      return p.actions;
    case 'actionHistoryList':
      return `${p.actions}/history`;
    case 'action':
      return withId(history ? `${p.actions}/history` : p.actions);
    case 'drepList':
      return p.dreps;
    case 'drep':
      return withId(p.dreps);
    case 'forumList':
      return p.proposals;
    case 'forumProposal':
      return withId(p.proposals);
    case 'budget':
      return withId(p.budget);
    case 'guides':
      return '/guides';
    case 'faqs':
      return '/faqs';
    default:
      return null;
  }
};

/** First path segments, either scheme, by the section they open. */
const SECTIONS: Record<string, string> = {
  governance_actions: 'actions',
  actions: 'actions',
  drep_directory: 'dreps',
  dreps: 'dreps',
  proposal_discussion: 'proposals',
  proposals: 'proposals',
  budget_discussion: 'budget',
  budget: 'budget',
};

const WALLET_SEGMENTS = new Set([
  'dashboard',
  'my',
  'register_drep',
  'register_direct_voter',
  'retire_drep',
  'retire_direct_voter',
  'edit_drep',
  'stake_keys',
  'create_governance_action',
]);

const UNKNOWN: ResolvedPage = { kind: 'unknown', canonicalPath: null };
const WALLET: ResolvedPage = { kind: 'wallet', canonicalPath: null };

type Match = { kind: PageKind; id?: string; history?: boolean } | null;

const matchActions = (rest: string[]): Match => {
  const [a, b, c] = rest;
  if (rest.length === 0) return { kind: 'actionList' };
  if (a === 'new') return rest.length === 1 ? { kind: 'wallet' } : null;
  if (a === 'history') {
    if (rest.length === 1) return { kind: 'actionHistoryList' };
    if (rest.length === 2 && isActionId(b)) {
      return { kind: 'action', id: b, history: true };
    }
    return null;
  }
  if (a === 'category') {
    if (rest.length === 2) return { kind: 'actionList' };
    if (rest.length === 3 && isActionId(c)) return { kind: 'action', id: c };
    return null;
  }
  return rest.length === 1 && isActionId(a) ? { kind: 'action', id: a } : null;
};

const matchSection = (section: string, rest: string[]): Match => {
  const [a] = rest;
  switch (section) {
    case 'actions':
      return matchActions(rest);
    case 'dreps':
      if (rest.length === 0) return { kind: 'drepList' };
      return rest.length === 1 && isDRepId(a) ? { kind: 'drep', id: a } : null;
    case 'proposals':
      if (rest.length === 0) return { kind: 'forumList' };
      if (a === 'propose' || a === 'new') return { kind: 'wallet' };
      return rest.length === 1 && isForumId(a)
        ? { kind: 'forumProposal', id: a }
        : null;
    case 'budget':
      // The archive's list, a proposal, its categories and the old
      // /propose, which redirects to the list.
      if (rest.length === 1 && a !== 'propose' && BUDGET_ID.test(a)) {
        return { kind: 'budget', id: a };
      }
      return { kind: 'budget' };
    default:
      return null;
  }
};

/** Path, query and fragment stripped down to its segments. */
const segmentsOf = (rawPath: string): string[] | null => {
  if (rawPath.length > MAX_PATH_LENGTH) return null;
  const path = rawPath.split(/[?#]/, 1)[0];
  let decoded: string;
  try {
    decoded = decodeURIComponent(path);
  } catch {
    return null;
  }
  if (!decoded.startsWith('/')) return null;
  return decoded.split('/').filter((segment) => segment !== '');
};

export const resolvePath = (
  rawPath: string,
  scheme: UrlScheme,
): ResolvedPage => {
  let segments = segmentsOf(rawPath);
  if (segments === null) return UNKNOWN;
  if (segments[0] === 'connected') segments = segments.slice(1);

  const [first, ...rest] = segments;
  let match: Match;
  if (first === undefined) match = { kind: 'home' };
  else if (first === 'guides' && rest.length === 0) match = { kind: 'guides' };
  else if (first === 'faqs' && rest.length === 0) match = { kind: 'faqs' };
  else if (WALLET_SEGMENTS.has(first)) match = { kind: 'wallet' };
  else if (first in SECTIONS) match = matchSection(SECTIONS[first], rest);
  else match = null;

  if (match === null) return UNKNOWN;
  if (match.kind === 'wallet') return WALLET;

  const history = match.history ?? false;
  const page: ResolvedPage = {
    kind: match.kind,
    canonicalPath: canonicalPathFor(match.kind, match.id, scheme, history),
  };
  if (match.id !== undefined) page.id = match.id;
  if (history) page.history = true;
  return page;
};
