import { ApiError } from '../common/errors';
import { amountClone, parseBdInput } from './bd-input';

function body(over: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    privacy_policy: true,
    intersect_named_administrator: false,
    intersect_admin_further_text: 'text',
    bd_proposal_ownership: {
      submited_on_behalf: 'Company',
      company_name: 'Acme',
      be_country: 1,
      agreed: true,
    },
    bd_psapb: { problem_statement: 'p', type_name: 2, roadmap_name: '10', committee_name: 1 },
    bd_proposal_detail: { proposal_name: 'Name', contract_type_name: 4 },
    bd_costing: {
      ada_amount: '1,5',
      usd_to_ada_conversion_rate: 2,
      amount_in_preferred_currency: '',
      preferred_currency: 1,
      cost_breakdown: 'c',
    },
    bd_further_information: {
      proposal_links: [{ prop_link: '' }, { prop_link: 'https://a.example', prop_link_text: 'A' }],
    },
    ...over,
  };
}

function err(fn: () => unknown): string {
  try {
    fn();
  } catch (e) {
    if (e instanceof ApiError) return `${e.errorName}: ${e.message}`;
    throw e;
  }
  return 'no error';
}

describe('parseBdInput', () => {
  it('reads the writable fields and collects lookup references', () => {
    const r = parseBdInput(body());
    expect(r.masterId).toBeNull();
    expect(r.ownership).toMatchObject({ companyName: 'Acme', beCountryId: 1, agreed: true, groupName: null });
    expect(r.psapb).toMatchObject({ typeId: 2, roadmapId: 10, committeeId: 1 });
    expect(r.lookups.map((l) => [l.field, l.model, l.id])).toEqual([
      ['be_country', 'countryList', 1],
      ['type_name', 'bdType', 2],
      ['roadmap_name', 'bdRoadMap', 10],
      ['committee_name', 'bdIntersectCommittee', 1],
      ['contract_type_name', 'bdContractType', 4],
      ['preferred_currency', 'bdCurrency', 1],
    ]);
    expect(r.contact).toBeNull();
  });

  it('stores costing amounts as strings and computes the clones', () => {
    const c = parseBdInput(body()).costing;
    expect(c).toMatchObject({
      adaAmount: '1,5',
      usdToAdaConversionRate: '2',
      amountInPreferredCurrency: '',
      adaAmountClone: 1.5,
      usdToAdaConversionRateClone: 2,
      amountInPreferredCurrencyClone: 0,
    });
    expect(amountClone(null)).toBe(0);
    expect(amountClone('abc')).toBe(0);
    expect(amountClone('1e400')).toBe(0);
  });

  it('drops blank links and renumbers positions', () => {
    expect(parseBdInput(body()).links).toEqual([{ position: 0, link: 'https://a.example', text: 'A' }]);
  });

  it('ignores echoed attributes from the edit flow', () => {
    const r = parseBdInput(
      body({
        master_id: '7',
        is_active: false,
        prop_comments_number: 99,
        submitted_for_vote: '2026-01-01T00:00:00.000Z',
        creator: { govtool_username: 'mallory' },
        user_govtool_username: 'x',
        master_proposal_created_at: 'x',
      }),
    );
    expect(r.masterId).toBe(7);
    expect(Object.keys(r).sort()).toEqual(
      [
        'contact',
        'costing',
        'intersectAdminFurtherText',
        'intersectNamedAdministrator',
        'links',
        'lookups',
        'masterId',
        'ownership',
        'proposalDetail',
        'psapb',
      ].sort(),
    );
  });

  it('requires privacy_policy === true', () => {
    expect(err(() => parseBdInput(body({ privacy_policy: false })))).toBe(
      'BadRequestError: Privacy policy must be accepted',
    );
    expect(err(() => parseBdInput(body({ privacy_policy: 'true' })))).toBe(
      'BadRequestError: Privacy policy must be accepted',
    );
  });

  it.each([
    'bd_proposal_ownership',
    'bd_psapb',
    'bd_proposal_detail',
    'bd_costing',
    'bd_further_information',
  ])('requires %s', (s) => {
    expect(err(() => parseBdInput(body({ [s]: null })))).toBe(`ValidationError: ${s} is required`);
    const b = body();
    delete b[s];
    expect(err(() => parseBdInput(b))).toBe(`ValidationError: ${s} is required`);
  });

  it('type and length rules', () => {
    expect(err(() => parseBdInput(body({ bd_psapb: { type_name: 'core' } })))).toBe(
      'ValidationError: type_name is invalid',
    );
    expect(err(() => parseBdInput(body({ bd_proposal_detail: { proposal_name: 5 } })))).toBe(
      'ValidationError: proposal_name is invalid',
    );
    expect(err(() => parseBdInput(body({ bd_proposal_detail: { proposal_name: 'x'.repeat(15001) } })))).toBe(
      'ValidationError: proposal_name is too long',
    );
    expect(err(() => parseBdInput(body({ intersect_named_administrator: 'yes' })))).toBe(
      'ValidationError: intersect_named_administrator is invalid',
    );
    expect(
      err(() =>
        parseBdInput(
          body({ bd_further_information: { proposal_links: Array(26).fill({ prop_link: 'a' }) } }),
        ),
      ),
    ).toBe('ValidationError: proposal_links is too long');
    expect(err(() => parseBdInput(body({ bd_further_information: { proposal_links: ['x'] } })))).toBe(
      'ValidationError: proposal_links is invalid',
    );
    expect(err(() => parseBdInput(body({ master_id: 'abc' })))).toBe('ValidationError: master_id is invalid');
  });

  it('BD links: http, https and ipfs only (Δ46)', () => {
    const links = (prop_link: unknown) =>
      body({ bd_further_information: { proposal_links: [{ prop_link }] } });
    for (const ok of ['https://a.example/x', 'http://a.example', 'ipfs://bafy123']) {
      expect(parseBdInput(links(ok)).links).toHaveLength(1);
    }
    for (const bad of [
      'javascript:alert(1)',
      ' JavaScript:alert(1)',
      'data:text/html,x',
      'not a url',
      'ftp://a.example',
    ]) {
      expect(err(() => parseBdInput(links(bad)))).toBe('ValidationError: prop_link is invalid');
    }
    expect(parseBdInput(links('  ')).links).toEqual([]);
  });

  it('reads legacy contact information', () => {
    const r = parseBdInput(body({ bd_contact_information: { be_email: 'a@b.c', be_nationality: 3 } }));
    expect(r.contact).toMatchObject({ beEmail: 'a@b.c', beNationalityId: 3, beCountryOfResId: null });
    expect(r.lookups.at(-1)).toEqual({ field: 'be_nationality', model: 'countryList', id: 3 });
  });
});
