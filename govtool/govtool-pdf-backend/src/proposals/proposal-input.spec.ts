import { ApiError } from '../common/errors';
import { isAllowedDocumentUrl, parseAmount, readContentInput, unwrapRelation } from './proposal-input';

const STAKE = 'stake_test1urfa857n60fa857n60fa857n60fa857n60fa857n60fa85cyqv8zy';
const opts = { networkId: null, hardForkRule: 'any-field' as const };

function err(fn: () => unknown): { name: string; message: string; details: unknown } {
  try {
    fn();
  } catch (e) {
    if (e instanceof ApiError) return { name: e.errorName, message: e.message, details: e.details };
    throw e;
  }
  throw new Error('expected an ApiError');
}

const base = (o: Record<string, unknown> = {}) => ({
  gov_action_type_id: 1,
  prop_name: 'Name',
  prop_abstract: 'A',
  prop_motivation: 'M',
  prop_rationale: 'R',
  ...o,
});

describe('readContentInput', () => {
  it('reads writable fields only; null texts become empty strings', () => {
    const r = readContentInput(
      base({
        gov_action_type_id: '1',
        prop_abstract: null,
        user_id: 7,
        prop_likes: 9,
        prop_submitted: true,
        prop_rev_active: false,
        proposal_links: [
          { id: 3, prop_link: 'https://a.io', prop_link_text: 'A' },
          { prop_link: '', prop_link_text: 'blank' },
          { prop_link: 'https://b.io' },
        ],
      }),
      opts,
    );
    expect(r).toEqual({
      govActionTypeId: 1,
      name: 'Name',
      abstract: '',
      motivation: 'M',
      rationale: 'R',
      isDraft: false,
      links: [
        { link: 'https://a.io', text: 'A' },
        { link: 'https://b.io', text: null },
      ],
      withdrawals: [],
      constitution: null,
      hardFork: null,
    });
  });

  it('type and length rules give V', () => {
    expect(err(() => readContentInput(base({ gov_action_type_id: undefined }), opts)).message).toBe(
      'gov_action_type_id is invalid',
    );
    expect(err(() => readContentInput(base({ prop_name: 'x'.repeat(81) }), opts)).message).toBe(
      'prop_name is too long',
    );
    expect(err(() => readContentInput(base({ prop_motivation: 'x'.repeat(12001) }), opts)).message).toBe(
      'prop_motivation is too long',
    );
    expect(err(() => readContentInput(base({ prop_rationale: 5 }), opts)).message).toBe(
      'prop_rationale is invalid',
    );
    expect(
      err(() => readContentInput(base({ proposal_links: [{ prop_link: 'x'.repeat(2049) }] }), opts)).message,
    ).toBe('prop_link is too long');
    expect(err(() => readContentInput(base({ proposal_links: ['x'] }), opts)).message).toBe(
      'proposal_links is invalid',
    );
    expect(err(() => readContentInput(base({ proposal_withdrawals: {} }), opts)).message).toBe(
      'proposal_withdrawals is invalid',
    );
    expect(err(() => readContentInput(base({ prop_name: '  ' }), opts)).message).toBe(
      'prop_name is required',
    );
  });

  it('a draft may have no title', () => {
    expect(readContentInput(base({ prop_name: undefined, is_draft: true }), opts).name).toBe('');
  });

  it('treasury checks apply to non-drafts only', () => {
    const t2 = (w: unknown, extra: Record<string, unknown> = {}) =>
      readContentInput(base({ gov_action_type_id: 2, proposal_withdrawals: w, ...extra }), opts);
    expect(err(() => t2([])).details).toBe('Withdrawal parametars not exist');
    const bad = 'Withdrawal addrress or amount parametars not valid';
    expect(err(() => t2([{ prop_receiving_address: STAKE, prop_amount: '-1' }])).details).toBe(bad);
    expect(err(() => t2([{ prop_receiving_address: STAKE, prop_amount: '' }])).details).toBe(bad);
    expect(err(() => t2([{ prop_receiving_address: STAKE }])).details).toBe(bad);
    expect(err(() => t2([{ prop_receiving_address: 'stake1xyz', prop_amount: 1 }])).details).toBe(bad);
    expect(t2([{ prop_receiving_address: STAKE, prop_amount: ' 3 ' }]).withdrawals).toEqual([
      { receivingAddress: STAKE, amount: 3 },
    ]);
    expect(
      err(() =>
        readContentInput(
          base({
            gov_action_type_id: 2,
            proposal_withdrawals: [{ prop_receiving_address: STAKE, prop_amount: 1 }],
          }),
          {
            ...opts,
            networkId: 1,
          },
        ),
      ).details,
    ).toBe(bad);
    expect(t2([{ prop_receiving_address: 'x', prop_amount: 'abc' }], { is_draft: true }).withdrawals).toEqual(
      [{ receivingAddress: 'x', amount: null }],
    );
  });

  it('constitution rules; any other guardrails value is stored as false', () => {
    const t3 = (c: unknown, extra: Record<string, unknown> = {}) =>
      readContentInput(base({ gov_action_type_id: 3, proposal_constitution_content: c, ...extra }), opts);
    expect(err(() => t3(undefined)).details).toBe(
      'proposal_constitution_content is required for Constitution action',
    );
    expect(err(() => t3({ data: null })).details).toBe(
      'proposal_constitution_content is required for Constitution action',
    );
    expect(
      t3({ prop_constitution_url: 'https://c.io', prop_have_guardrails_script: 'yes' }).constitution,
    ).toEqual({
      constitutionUrl: 'https://c.io',
      haveGuardrailsScript: false,
      guardrailsScriptUrl: null,
      guardrailsScriptHash: null,
    });
    expect(
      t3({
        prop_constitution_url: 'ipfs://c',
        prop_have_guardrails_script: true,
        prop_guardrails_script_url: 'https://g.io',
        prop_guardrails_script_hash: 'h',
      }).constitution,
    ).toMatchObject({ haveGuardrailsScript: true, guardrailsScriptHash: 'h' });
    // Drafts store what they are given.
    expect(
      t3({ prop_constitution_url: 'javascript:x' }, { is_draft: true }).constitution?.constitutionUrl,
    ).toBe('javascript:x');
    // Other types never carry the object.
    expect(
      readContentInput(base({ proposal_constitution_content: { prop_constitution_url: 'x' } }), opts)
        .constitution,
    ).toBeNull();
  });

  it('hard-fork creation rule differs between create and revision', () => {
    const h = { proposal_hard_fork_content: { major: 10, minor: '' } };
    expect(readContentInput(base({ gov_action_type_id: 6, ...h }), opts).hardFork).toEqual({
      previousGaHash: null,
      previousGaId: null,
      major: '10',
      minor: null,
    });
    expect(
      readContentInput(base({ gov_action_type_id: 6, ...h }), { ...opts, hardForkRule: 'previous-ga-id' })
        .hardFork,
    ).toBeNull();
    expect(
      readContentInput(base({ gov_action_type_id: 6, proposal_hard_fork_content: { previous_ga_id: 0 } }), {
        ...opts,
        hardForkRule: 'previous-ga-id',
      }).hardFork?.previousGaId,
    ).toBe('0');
    expect(readContentInput(base({ gov_action_type_id: 6 }), opts).hardFork).toBeNull();
  });
});

describe('helpers', () => {
  it('unwrapRelation', () => {
    expect(unwrapRelation({ data: { id: 1, attributes: { a: 1 } } }, 'f')).toEqual({ a: 1 });
    expect(unwrapRelation({ data: null }, 'f')).toBeUndefined();
    expect(unwrapRelation({ a: 1 }, 'f')).toEqual({ a: 1 });
    expect(unwrapRelation(null, 'f')).toBeUndefined();
    expect(err(() => unwrapRelation('x', 'f')).message).toBe('f is invalid');
  });
  it('parseAmount', () => {
    expect([
      parseAmount(1),
      parseAmount('2.5'),
      parseAmount('x'),
      parseAmount(-1),
      parseAmount(Infinity),
      parseAmount(null),
    ]).toEqual([1, 2.5, null, null, null, null]);
  });
  it('isAllowedDocumentUrl (Δ24)', () => {
    expect(['https://a.io', 'http://a.io', 'ipfs://bafy'].map(isAllowedDocumentUrl)).toEqual([
      true,
      true,
      true,
    ]);
    expect(
      ['javascript:alert(1)', 'data:text/html,x', 'ftp://a', '', 'nope', null].map(isAllowedDocumentUrl),
    ).toEqual(Array(6).fill(false));
  });
});

describe('links and NUL (Δ47)', () => {
  it('refuses non-http(s)/ipfs links, drafts included; drops blanks', () => {
    for (const link of ['javascript:alert(1)', 'data:,x', 'mailto:a@b.c', 'nope']) {
      expect(err(() => readContentInput(base({ proposal_links: [{ prop_link: link }] }), opts)).message).toBe(
        'prop_link is invalid',
      );
      expect(
        err(() => readContentInput(base({ is_draft: true, proposal_links: [{ prop_link: link }] }), opts))
          .message,
      ).toBe('prop_link is invalid');
    }
    expect(
      readContentInput(base({ proposal_links: [{ prop_link: ' ' }, { prop_link: 'ipfs://x' }] }), opts).links,
    ).toEqual([{ link: 'ipfs://x', text: null }]);
  });
  it('a NUL byte in any string is V', () => {
    expect(err(() => readContentInput(base({ prop_name: 'a\u0000' }), opts)).message).toBe(
      'prop_name is invalid',
    );
    expect(err(() => readContentInput(base({ prop_rationale: '\u0000' }), opts)).message).toBe(
      'prop_rationale is invalid',
    );
  });
});
