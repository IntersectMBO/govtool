import { isValidUsername, selfProjection } from './self-projection';

describe('govtool_username rule', () => {
  it.each(['alice', 'a', 'a.b_c', '123', 'x'.repeat(30), 'a..', 'a_'])('accepts %p', (v) => {
    expect(isValidUsername(v)).toBe(true);
  });

  it.each(['.alice', '_alice', 'x'.repeat(31), 'Alice', 'a-b', 'a#b', 'a b', '', 5, null])(
    'rejects %p',
    (v) => {
      expect(isValidUsername(v)).toBe(false);
    },
  );
});

describe('selfProjection', () => {
  it('has exactly the self fields and never email or secrets (Δ1)', () => {
    const at = new Date('2026-09-26T10:00:00.000Z');
    const p = selfProjection({
      id: 3,
      username: 'e0ab',
      govtoolUsername: null,
      isValidated: false,
      blocked: false,
      createdAt: at,
      updatedAt: at,
    });
    expect(p).toEqual({
      id: 3,
      username: 'e0ab',
      provider: 'local',
      confirmed: true,
      blocked: false,
      govtool_username: null,
      is_validated: false,
      createdAt: '2026-09-26T10:00:00.000Z',
      updatedAt: '2026-09-26T10:00:00.000Z',
    });
  });
});
