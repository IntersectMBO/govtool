import {
  encodeCip129DRepId,
  encodeCip129GovActionId,
} from '../src/common/legacy-ids';
import {
  canonicalPathFor,
  isActionId,
  isDRepId,
  isForumId,
  resolvePath,
} from '../src/seo/routes';

const TX = 'ab'.repeat(32);
const ACTION = encodeCip129GovActionId({ txHash: TX, index: 0 });
const DREP = encodeCip129DRepId({ hash: 'cd'.repeat(28), isScript: false });

describe('id checks', () => {
  it('accepts CIP-129 action ids and bare tx hashes', () => {
    expect(isActionId(ACTION)).toBe(true);
    expect(isActionId(TX)).toBe(true);
    expect(isActionId('gov_action1qqqq')).toBe(false);
    expect(isActionId('zz')).toBe(false);
  });

  it('accepts DRep ids in their bech32 and hex forms', () => {
    expect(isDRepId(DREP)).toBe(true);
    expect(isDRepId('cd'.repeat(28))).toBe(true);
    expect(isDRepId('not-a-drep')).toBe(false);
  });

  it('accepts positive integer forum ids', () => {
    expect(isForumId('42')).toBe(true);
    expect(isForumId('0')).toBe(false);
    expect(isForumId('12345678901')).toBe(false);
    expect(isForumId('4a')).toBe(false);
  });
});

describe('resolvePath', () => {
  it.each([
    ['/', 'home', undefined, '/', '/'],
    [
      '/governance_actions',
      'actionList',
      undefined,
      '/governance_actions',
      '/actions',
    ],
    ['/actions', 'actionList', undefined, '/governance_actions', '/actions'],
    [
      '/governance_actions/category/InfoAction',
      'actionList',
      undefined,
      '/governance_actions',
      '/actions',
    ],
    [
      `/governance_actions/${ACTION}`,
      'action',
      ACTION,
      `/governance_actions/${ACTION}`,
      `/actions/${ACTION}`,
    ],
    [
      `/actions/${ACTION}`,
      'action',
      ACTION,
      `/governance_actions/${ACTION}`,
      `/actions/${ACTION}`,
    ],
    [
      `/governance_actions/category/InfoAction/${ACTION}`,
      'action',
      ACTION,
      `/governance_actions/${ACTION}`,
      `/actions/${ACTION}`,
    ],
    [
      '/governance_actions/history',
      'actionHistoryList',
      undefined,
      '/governance_actions/history',
      '/actions/history',
    ],
    [
      '/actions/history',
      'actionHistoryList',
      undefined,
      '/governance_actions/history',
      '/actions/history',
    ],
    [
      `/governance_actions/history/${ACTION}`,
      'action',
      ACTION,
      `/governance_actions/history/${ACTION}`,
      `/actions/history/${ACTION}`,
    ],
    ['/drep_directory', 'drepList', undefined, '/drep_directory', '/dreps'],
    ['/dreps', 'drepList', undefined, '/drep_directory', '/dreps'],
    [
      `/drep_directory/${DREP}`,
      'drep',
      DREP,
      `/drep_directory/${DREP}`,
      `/dreps/${DREP}`,
    ],
    [
      `/dreps/${DREP}`,
      'drep',
      DREP,
      `/drep_directory/${DREP}`,
      `/dreps/${DREP}`,
    ],
    [
      '/proposal_discussion',
      'forumList',
      undefined,
      '/proposal_discussion',
      '/proposals',
    ],
    [
      '/proposal_discussion/42',
      'forumProposal',
      '42',
      '/proposal_discussion/42',
      '/proposals/42',
    ],
    [
      '/proposals/42',
      'forumProposal',
      '42',
      '/proposal_discussion/42',
      '/proposals/42',
    ],
    [
      '/budget_discussion',
      'budget',
      undefined,
      '/budget_discussion',
      '/budget',
    ],
    ['/budget/17', 'budget', '17', '/budget_discussion/17', '/budget/17'],
    ['/guides', 'guides', undefined, '/guides', '/guides'],
    ['/faqs', 'faqs', undefined, '/faqs', '/faqs'],
  ])('%s is %s', (path, kind, id, developCanonical, cleanCanonical) => {
    const develop = resolvePath(path, 'develop');
    const clean = resolvePath(path, 'clean');
    expect(develop).toMatchObject({ kind, canonicalPath: developCanonical });
    expect(clean).toMatchObject({ kind, canonicalPath: cleanCanonical });
    expect(develop.id).toBe(id);
    expect(clean.id).toBe(id);
  });

  it.each([
    '/dashboard',
    '/my/votes_and_favorites',
    '/my/drep/register',
    '/register_drep',
    '/retire_direct_voter',
    '/edit_drep',
    '/stake_keys',
    '/create_governance_action',
    '/actions/new',
    '/proposal_discussion/propose',
    '/proposals/new',
  ])('%s is a wallet page with no canonical', (path) => {
    expect(resolvePath(path, 'develop')).toEqual({
      kind: 'wallet',
      canonicalPath: null,
    });
  });

  it('maps a /connected path to its public twin', () => {
    expect(
      resolvePath(`/connected/governance_actions/${ACTION}`, 'develop'),
    ).toMatchObject({
      kind: 'action',
      id: ACTION,
      canonicalPath: `/governance_actions/${ACTION}`,
    });
    expect(resolvePath('/connected/drep_directory', 'clean')).toMatchObject({
      kind: 'drepList',
      canonicalPath: '/dreps',
    });
  });

  it('ignores a query, a fragment, a trailing slash and percent-encoding', () => {
    const plain = resolvePath(`/governance_actions/${ACTION}`, 'develop');
    for (const variant of [
      `/governance_actions/${ACTION}/?utm_source=x`,
      `/governance_actions/${ACTION}/`,
      `/governance_actions/${ACTION}#0`,
      `/governance_actions/%67${ACTION.slice(1)}`,
    ]) {
      expect(resolvePath(variant, 'develop')).toEqual(plain);
    }
  });

  it('keeps a bare tx hash as the id', () => {
    expect(resolvePath(`/governance_actions/${TX}`, 'develop')).toMatchObject({
      kind: 'action',
      id: TX,
    });
  });

  it.each([
    '/drep_directory/not-a-drep',
    '/governance_actions/zz',
    '/proposal_discussion/0',
    '/error',
    '/nothing/here',
    '/governance_actions/%E0%A4%A',
    `/${'a'.repeat(2000)}`,
  ])('%s is unknown with no canonical', (path) => {
    expect(resolvePath(path, 'develop')).toEqual({
      kind: 'unknown',
      canonicalPath: null,
    });
  });
});

describe('canonicalPathFor', () => {
  it('builds the canonical path for an id found later', () => {
    expect(canonicalPathFor('action', ACTION, 'develop', false)).toBe(
      `/governance_actions/${ACTION}`,
    );
    expect(canonicalPathFor('action', ACTION, 'clean', true)).toBe(
      `/actions/history/${ACTION}`,
    );
    expect(canonicalPathFor('wallet', undefined, 'clean', false)).toBeNull();
  });
});
