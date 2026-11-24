import {
  ACTION_TYPE_NAMES,
  DEFAULT_TITLE,
  DREP_SUFFIX,
  FIXED_DESCRIPTIONS,
  FORUM_CARD_LABEL,
  HOME_CRUMB,
  HOME_TITLE,
  LIST_TITLES,
  SITE_DESCRIPTION,
  TITLE_SUFFIX,
  actionListDescription,
  drepListDescription,
} from './copy';
import {
  canonicalPathFor,
  type PageKind,
  type ResolvedPage,
  type UrlScheme,
} from './routes';
import type { SeoAction, SeoDRep, SeoForumProposal } from './seo.types';
import { plainText, stripEmoji, truncate } from './text';

const TITLE_MAX = 120;
const DESCRIPTION_MAX = 155;
const CARD_TITLE_MAX = 140;

export type CardInput = {
  kind: 'action' | 'drep' | 'forum';
  id: string;
  title: string;
  label: string;
};

export type Breadcrumb = { name: string; path: string };

export type PageInfo = {
  kind: PageKind;
  title: string;
  description: string;
  canonicalPath: string | null;
  noindex: boolean;
  card: CardInput | null;
  breadcrumbs: Breadcrumb[];
  summary: { heading: string; lines: string[] };
  drepName?: string;
};

export type PageData = {
  action?: SeoAction | null;
  drep?: SeoDRep | null;
  forum?: SeoForumProposal | null;
  openActionCount?: number;
  listedDRepCount?: number;
};

const MONTHS = [
  'Jan',
  'Feb',
  'Mar',
  'Apr',
  'May',
  'Jun',
  'Jul',
  'Aug',
  'Sep',
  'Oct',
  'Nov',
  'Dec',
];

/** `30 Nov 2026`, in UTC; null for a missing or unreadable date. */
const formatDate = (iso: string | null): string | null => {
  if (iso === null) return null;
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return null;
  return `${d.getUTCDate()} ${MONTHS[d.getUTCMonth()]} ${d.getUTCFullYear()}`;
};

/** `gov_action1abc…xyz`: the prefix, three characters, the last three. */
export const shortId = (id: string): string => {
  const separator = id.lastIndexOf('1');
  const head = separator > 0 ? separator + 4 : 8;
  return id.length <= head + 4 ? id : `${id.slice(0, head)}…${id.slice(-3)}`;
};

/** Lovelace as rounded ADA: 482774913502419 is `482.8M`. */
export const formatAda = (lovelace: bigint): string => {
  const ada = Number(lovelace / 1000n) / 1000;
  const scaled = (value: number, unit: string) =>
    `${value.toFixed(1).replace(/\.0$/, '')}${unit}`;
  if (ada >= 1e9) return scaled(ada / 1e9, 'B');
  if (ada >= 1e6) return scaled(ada / 1e6, 'M');
  if (ada >= 1e3) return scaled(ada / 1e3, 'K');
  return String(Math.round(ada));
};

/** User text as a title, or null when nothing readable is left. */
const readableTitle = (raw: string | null): string | null => {
  const text = plainText(raw);
  return stripEmoji(text).trim() === '' ? null : text;
};

const pageTitle = (text: string) => truncate(text, TITLE_MAX) + TITLE_SUFFIX;
const description = (text: string) => truncate(text, DESCRIPTION_MAX);
const sentence = (lead: string, body: string | null) => {
  const rest = plainText(body);
  return rest === '' ? `${lead}.` : `${lead}. ${rest}`;
};

export const isDRepIndexed = (d: SeoDRep): boolean =>
  !d.doNotList && d.metadataError === null && d.hasMetadata;

/** The outcome of a finished action, or null while it is live. */
const outcomeOf = ({ status }: SeoAction): string | null => {
  if (status.enacted_epoch !== null) {
    return `Enacted in epoch ${status.enacted_epoch}`;
  }
  if (status.ratified_epoch !== null) {
    return `Ratified in epoch ${status.ratified_epoch}`;
  }
  if (status.dropped_epoch !== null) {
    return `Dropped in epoch ${status.dropped_epoch}`;
  }
  if (status.expired_epoch !== null) {
    return `Expired in epoch ${status.expired_epoch}`;
  }
  return null;
};

/** Whether an action is still open for voting. */
export const isLive = (action: SeoAction): boolean =>
  outcomeOf(action) === null;

/** The canonical path of an action: its history page once it has finished. */
export const actionPath = (action: SeoAction, scheme: UrlScheme): string =>
  canonicalPathFor(
    'action',
    action.cip129Id,
    scheme,
    outcomeOf(action) !== null,
  ) ?? '/';

const defaults = (kind: PageKind): PageInfo => ({
  kind,
  title: DEFAULT_TITLE,
  description: SITE_DESCRIPTION,
  canonicalPath: null,
  noindex: true,
  card: null,
  breadcrumbs: [],
  summary: { heading: DEFAULT_TITLE, lines: [SITE_DESCRIPTION] },
});

type Parts = {
  heading: string;
  description: string;
  canonicalPath: string | null;
  card?: CardInput;
  section?: Breadcrumb;
  noindex?: boolean;
  drepName?: string;
};

const assemble = (kind: PageKind, parts: Parts): PageInfo => {
  const title = kind === 'home' ? HOME_TITLE : pageTitle(parts.heading);
  const breadcrumbs: Breadcrumb[] = [];
  if (kind !== 'home' && parts.canonicalPath !== null) {
    breadcrumbs.push({ name: HOME_CRUMB, path: '/' });
    if (parts.section !== undefined) breadcrumbs.push(parts.section);
    breadcrumbs.push({
      name: truncate(parts.heading, TITLE_MAX),
      path: parts.canonicalPath,
    });
  }
  const info: PageInfo = {
    kind,
    title,
    description: parts.description,
    canonicalPath: parts.canonicalPath,
    noindex: parts.noindex ?? false,
    card: parts.card ?? null,
    breadcrumbs,
    summary: {
      heading:
        kind === 'home' ? HOME_TITLE : truncate(parts.heading, TITLE_MAX),
      lines: [parts.description],
    },
  };
  if (parts.drepName !== undefined) info.drepName = parts.drepName;
  return info;
};

const listCrumb = (
  kind: keyof typeof LIST_TITLES,
  scheme: UrlScheme,
): Breadcrumb => ({
  name: LIST_TITLES[kind],
  path: canonicalPathFor(kind, undefined, scheme, false) ?? '/',
});

const actionPage = (
  page: ResolvedPage,
  action: SeoAction,
  scheme: UrlScheme,
): PageInfo => {
  const typeName = ACTION_TYPE_NAMES[action.type] ?? action.type;
  const heading =
    readableTitle(action.title) ?? `${typeName} ${shortId(action.cip129Id)}`;
  const outcome = outcomeOf(action);
  const deadline = formatDate(action.expiryDate);
  const lead =
    outcome !== null
      ? `${typeName} · ${outcome}`
      : deadline !== null
        ? `${typeName} · voting open until ${deadline}`
        : `${typeName} · voting open`;
  // The app sends a finished action to its history page.
  const history = (page.history ?? false) || outcome !== null;
  return assemble('action', {
    heading,
    description: description(sentence(lead, action.abstract)),
    canonicalPath: canonicalPathFor('action', action.cip129Id, scheme, history),
    section: listCrumb(history ? 'actionHistoryList' : 'actionList', scheme),
    card: {
      kind: 'action',
      id: action.cip129Id,
      title: truncate(heading, CARD_TITLE_MAX),
      label: `${typeName} · ${outcome ?? 'Voting open'}`,
    },
  });
};

const drepPage = (drep: SeoDRep, scheme: UrlScheme): PageInfo => {
  const name = readableTitle(drep.givenName);
  const display = name ?? shortId(drep.view);
  const power =
    drep.votingPower === null
      ? ''
      : ` · ${formatAda(drep.votingPower)} ADA voting power`;
  const label = `${drep.status} DRep`;
  return assemble('drep', {
    heading: `${display}${DREP_SUFFIX}`,
    description: description(sentence(`${label}${power}`, drep.objectives)),
    canonicalPath: canonicalPathFor('drep', drep.view, scheme, false),
    section: listCrumb('drepList', scheme),
    noindex: !isDRepIndexed(drep),
    ...(name === null ? {} : { drepName: name }),
    card: {
      kind: 'drep',
      id: drep.view,
      title: truncate(display, CARD_TITLE_MAX),
      label,
    },
  });
};

const forumPage = (
  page: ResolvedPage,
  forum: SeoForumProposal,
  scheme: UrlScheme,
): PageInfo => {
  const heading = readableTitle(forum.title) ?? `Proposal ${forum.id}`;
  const abstract = plainText(forum.abstract);
  return assemble('forumProposal', {
    heading,
    description: description(
      abstract === '' ? FIXED_DESCRIPTIONS.forumList : abstract,
    ),
    canonicalPath: page.canonicalPath,
    section: listCrumb('forumList', scheme),
    card: {
      kind: 'forum',
      id: forum.id,
      title: truncate(heading, CARD_TITLE_MAX),
      label: FORUM_CARD_LABEL,
    },
  });
};

/**
 * The tags' content for one page. Pure: the controller looks up `data`. A
 * detail page whose id is valid but holds nothing is noindex, with the
 * site-wide defaults.
 */
export const buildPage = (
  page: ResolvedPage,
  data: PageData,
  scheme: UrlScheme,
): PageInfo => {
  switch (page.kind) {
    case 'home':
      return assemble('home', {
        heading: HOME_TITLE,
        description: SITE_DESCRIPTION,
        canonicalPath: '/',
      });
    case 'actionList':
      return assemble('actionList', {
        heading: LIST_TITLES.actionList,
        description: actionListDescription(data.openActionCount),
        canonicalPath: page.canonicalPath,
      });
    case 'drepList':
      return assemble('drepList', {
        heading: LIST_TITLES.drepList,
        description: drepListDescription(data.listedDRepCount),
        canonicalPath: page.canonicalPath,
      });
    case 'actionHistoryList':
    case 'forumList':
    case 'guides':
    case 'faqs':
      return assemble(page.kind, {
        heading: LIST_TITLES[page.kind],
        description: FIXED_DESCRIPTIONS[page.kind],
        canonicalPath: page.canonicalPath,
      });
    case 'budget':
      return assemble('budget', {
        heading: LIST_TITLES.budget,
        description: FIXED_DESCRIPTIONS.budget,
        canonicalPath: page.canonicalPath,
        section:
          page.id === undefined ? undefined : listCrumb('budget', scheme),
      });
    case 'action':
      return data.action
        ? actionPage(page, data.action, scheme)
        : defaults('action');
    case 'drep':
      return data.drep ? drepPage(data.drep, scheme) : defaults('drep');
    case 'forumProposal':
      if (data.forum === null) return defaults('forumProposal');
      // Not fetched from the forum yet: generic wording, still indexable.
      if (data.forum === undefined) {
        return assemble('forumProposal', {
          heading: LIST_TITLES.forumList,
          description: FIXED_DESCRIPTIONS.forumList,
          canonicalPath: page.canonicalPath,
          section: listCrumb('forumList', scheme),
        });
      }
      return forumPage(page, data.forum, scheme);
    default:
      return defaults(page.kind);
  }
};
