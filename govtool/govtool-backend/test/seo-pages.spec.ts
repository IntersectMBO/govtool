import { buildPage, isDRepIndexed } from '../src/seo/pages';
import { resolvePath } from '../src/seo/routes';
import type { SeoAction, SeoDRep } from '../src/seo/seo.types';
import { actionId, drepId } from './ids';

const SUFFIX = ' | Cardano GovTool';
const TX = 'ab'.repeat(32);
const ACTION = actionId(TX, 0);
const DREP = drepId('a');

const NO_STATUS = {
  ratified_epoch: null,
  enacted_epoch: null,
  dropped_epoch: null,
  expired_epoch: null,
};
const NO_TIMES = {
  ratified_time: null,
  enacted_time: null,
  dropped_time: null,
  expired_time: null,
};

function action(overrides: Partial<SeoAction> = {}): SeoAction {
  return {
    cip129Id: ACTION,
    type: 'InfoAction',
    title: 'State of the Cardano Developer Ecosystem',
    abstract: 'The **survey** is back for 2026.',
    expiryDate: '2026-11-30T21:44:51Z',
    expiryEpoch: 671,
    createdDate: '2026-10-28T10:27:09.000Z',
    status: NO_STATUS,
    statusTimes: NO_TIMES,
    ...overrides,
  };
}

function drep(overrides: Partial<SeoDRep> = {}): SeoDRep {
  return {
    view: DREP,
    givenName: 'YUTA',
    objectives: 'Bridge the Japanese community and governance.',
    status: 'Active',
    votingPower: 482774913502419n,
    doNotList: false,
    metadataError: null,
    hasMetadata: true,
    latestRegistrationDate: '2025-02-10T15:55:32.000Z',
    ...overrides,
  };
}

const page = (path: string) => resolvePath(path, 'develop');

describe('home and lists', () => {
  it('puts the brand first on the home page, with no suffix', () => {
    const home = buildPage(page('/'), {}, 'develop');
    expect(home.title).toBe(
      'Cardano GovTool: vote, delegate and follow Cardano governance',
    );
    expect(home.noindex).toBe(false);
    expect(home.canonicalPath).toBe('/');
    expect(home.breadcrumbs).toEqual([]);
  });

  it.each([
    [0, 'No governance actions open right now. See past actions and outcomes.'],
    [1, '1 governance action open for voting on Cardano.'],
    [7, '7 governance actions open for voting on Cardano.'],
  ])('describes %i open actions', (count, description) => {
    const list = buildPage(
      page('/governance_actions'),
      { openActionCount: count },
      'develop',
    );
    expect(list.title).toBe(`Governance actions${SUFFIX}`);
    expect(list.description).toBe(description);
  });

  it.each([
    [0, 'Find and delegate to a DRep on Cardano.'],
    [1, 'Find and delegate to the 1 DRep on Cardano.'],
    [689, 'Find and delegate to one of 689 DReps on Cardano.'],
  ])('describes %i listed DReps', (count, description) => {
    const list = buildPage(
      page('/drep_directory'),
      { listedDRepCount: count },
      'develop',
    );
    expect(list.title).toBe(`DRep directory${SUFFIX}`);
    expect(list.description).toBe(description);
  });

  it('gives breadcrumbs from home to the list', () => {
    expect(
      buildPage(resolvePath('/dreps', 'clean'), {}, 'clean').breadcrumbs,
    ).toEqual([
      { name: 'Home', path: '/' },
      { name: 'DRep directory', path: '/dreps' },
    ]);
  });
});

describe('governance actions', () => {
  it('describes a live action by type, deadline and abstract', () => {
    const p = buildPage(
      page(`/governance_actions/${ACTION}`),
      { action: action() },
      'develop',
    );
    expect(p.title).toBe(`State of the Cardano Developer Ecosystem${SUFFIX}`);
    expect(p.description).toBe(
      'Info action · voting open until 30 Nov 2026. The survey is back for 2026.',
    );
    expect(p.card).toEqual({
      kind: 'action',
      id: ACTION,
      title: 'State of the Cardano Developer Ecosystem',
      label: 'Info action · Voting open',
    });
    expect(p.noindex).toBe(false);
    expect(p.canonicalPath).toBe(`/governance_actions/${ACTION}`);
  });

  it.each([
    [{ ...NO_STATUS, ratified_epoch: 580 }, 'Ratified in epoch 580'],
    [
      { ...NO_STATUS, ratified_epoch: 580, enacted_epoch: 581 },
      'Enacted in epoch 581',
    ],
    [{ ...NO_STATUS, expired_epoch: 590 }, 'Expired in epoch 590'],
    [{ ...NO_STATUS, dropped_epoch: 585 }, 'Dropped in epoch 585'],
  ])('describes a finished action by its outcome', (status, outcome) => {
    const p = buildPage(
      page(`/governance_actions/${ACTION}`),
      { action: action({ type: 'TreasuryWithdrawals', status }) },
      'develop',
    );
    expect(p.description.startsWith(`Treasury withdrawal · ${outcome}.`)).toBe(
      true,
    );
    expect(p.card?.label).toBe(`Treasury withdrawal · ${outcome}`);
  });

  it.each(['  🗳️  ', '**', '', null])(
    'falls back to the type and short id for the title %p',
    (title) => {
      const p = buildPage(
        page(`/governance_actions/${ACTION}`),
        { action: action({ title }) },
        'develop',
      );
      expect(p.title).toMatch(
        /^Info action gov_action1[a-z0-9]{3}…[a-z0-9]{3} \| Cardano GovTool$/,
      );
      expect(p.card?.title).toMatch(/^Info action gov_action1/);
    },
  );

  it('points a finished action at its history page, where the app sends it', () => {
    const p = buildPage(
      page(`/governance_actions/${ACTION}`),
      { action: action({ status: { ...NO_STATUS, expired_epoch: 590 } }) },
      'develop',
    );
    expect(p.canonicalPath).toBe(`/governance_actions/history/${ACTION}`);
    expect(p.breadcrumbs[1]).toEqual({
      name: 'Governance action history',
      path: '/governance_actions/history',
    });
  });

  it('uses the CIP-129 id in the canonical path of a bare tx hash', () => {
    const p = buildPage(
      page(`/governance_actions/${TX}`),
      { action: action() },
      'clean',
    );
    expect(p.canonicalPath).toBe(`/actions/${ACTION}`);
  });

  it('marks a valid id with no action noindex, with the default title', () => {
    const p = buildPage(
      page(`/governance_actions/${ACTION}`),
      { action: null },
      'develop',
    );
    expect(p.noindex).toBe(true);
    expect(p.title).toBe('Cardano GovTool');
    expect(p.card).toBeNull();
    expect(p.canonicalPath).toBeNull();
  });
});

describe('DReps', () => {
  it('describes a DRep by name, status, voting power and objectives', () => {
    const p = buildPage(
      page(`/drep_directory/${DREP}`),
      { drep: drep() },
      'develop',
    );
    expect(p.title).toBe(`YUTA · Cardano DRep${SUFFIX}`);
    expect(p.description).toBe(
      'Active DRep · 482.8M ADA voting power. Bridge the Japanese community and governance.',
    );
    expect(p.card).toEqual({
      kind: 'drep',
      id: DREP,
      title: 'YUTA',
      label: 'Active DRep',
    });
    expect(p.drepName).toBe('YUTA');
    expect(p.noindex).toBe(false);
  });

  it('falls back to a shortened id without a name', () => {
    const p = buildPage(
      page(`/drep_directory/${DREP}`),
      { drep: drep({ givenName: null }) },
      'develop',
    );
    expect(p.title).toMatch(/^drep1[a-z0-9]{3}…[a-z0-9]{3} · Cardano DRep \| /);
    // A shortened id is not a person's name, so no ProfilePage is claimed.
    expect(p.drepName).toBeUndefined();
  });

  it.each([
    [{ doNotList: true }, true],
    [{ metadataError: 'HASH_MISMATCH: x' }, true],
    [{ hasMetadata: false }, true],
    [{ status: 'Retired' as const }, false],
  ])('sets noindex for %p to %p', (overrides, noindex) => {
    const d = drep(overrides);
    expect(isDRepIndexed(d)).toBe(!noindex);
    expect(
      buildPage(page(`/drep_directory/${DREP}`), { drep: d }, 'develop')
        .noindex,
    ).toBe(noindex);
  });

  it('leaves out voting power it does not know', () => {
    const p = buildPage(
      page(`/drep_directory/${DREP}`),
      { drep: drep({ votingPower: null, objectives: null }) },
      'develop',
    );
    expect(p.description).toBe('Active DRep.');
  });
});

describe('forum, wallet and unknown pages', () => {
  it('describes a forum proposal by its title and abstract', () => {
    const p = buildPage(
      page('/proposal_discussion/42'),
      {
        forum: {
          id: '42',
          title: 'Fund a dev rel team',
          abstract: 'Twelve months.',
          updatedAt: null,
        },
      },
      'develop',
    );
    expect(p.title).toBe(`Fund a dev rel team${SUFFIX}`);
    expect(p.description).toBe('Twelve months.');
    expect(p.card).toMatchObject({ kind: 'forum', id: '42' });
  });

  it('keeps a forum proposal not fetched yet indexable, with generic wording', () => {
    const p = buildPage(page('/proposal_discussion/42'), {}, 'develop');
    expect(p.title).toBe(`Proposal discussion${SUFFIX}`);
    expect(p.noindex).toBe(false);
    expect(p.canonicalPath).toBe('/proposal_discussion/42');
    expect(p.card).toBeNull();
  });

  it('marks a forum proposal the forum does not have noindex', () => {
    const p = buildPage(
      page('/proposal_discussion/42'),
      { forum: null },
      'develop',
    );
    expect(p.noindex).toBe(true);
  });

  it.each(['/dashboard', '/nothing'])('%s is noindex with defaults', (path) => {
    const p = buildPage(page(path), {}, 'develop');
    expect(p.noindex).toBe(true);
    expect(p.canonicalPath).toBeNull();
    expect(p.title).toBe('Cardano GovTool');
  });
});

describe('limits', () => {
  it('caps titles at 120 characters plus the suffix and descriptions at 155', () => {
    const long = 'word '.repeat(100);
    const p = buildPage(
      page(`/governance_actions/${ACTION}`),
      { action: action({ title: long, abstract: long }) },
      'develop',
    );
    expect(p.title.length).toBeLessThanOrEqual(120 + SUFFIX.length);
    expect(p.description.length).toBeLessThanOrEqual(155);
    expect(p.card?.title.length).toBeLessThanOrEqual(140);
  });
});
