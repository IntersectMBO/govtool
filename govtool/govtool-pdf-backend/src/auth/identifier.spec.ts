import { ApiError } from '../common/errors';
import { parseIdentifier } from './identifier';

const H56 = 'ab'.repeat(28);

function message(fn: () => unknown): string {
  try {
    fn();
  } catch (e) {
    if (e instanceof ApiError) return `${e.errorName}: ${e.message}`;
    throw e;
  }
  return 'no error';
}

describe('parseIdentifier', () => {
  it('accepts e0 and e1 reward addresses as stake logins', () => {
    expect(parseIdentifier(`e0${H56}`, null)).toEqual({
      kind: 'stake',
      identifier: `e0${H56}`,
      keyHash: H56,
      networkId: 0,
    });
    expect(parseIdentifier(`e1${H56}`, null)).toMatchObject({ kind: 'stake', networkId: 1 });
  });

  it('lowercases', () => {
    expect(parseIdentifier(`E0${H56.toUpperCase()}`, null)).toMatchObject({ identifier: `e0${H56}` });
  });

  it('accepts a 56-hex key hash as a DRep login', () => {
    expect(parseIdentifier(H56, null)).toEqual({ kind: 'drep', identifier: H56, keyHash: H56 });
  });

  it.each([`f0${H56}`, `f1${H56}`, `e2${H56}`, `00${H56}`, `e0${H56}00`, H56.slice(2), 'xyz', '', 5, null])(
    'rejects %p',
    (v) => {
      expect(message(() => parseIdentifier(v, null))).toBe('ValidationError: Invalid identifier');
    },
  );

  it('checks the network when configured', () => {
    expect(message(() => parseIdentifier(`e1${H56}`, 0))).toBe(
      'ValidationError: Identifier network does not match',
    );
    expect(message(() => parseIdentifier(`e0${H56}`, 1))).toBe(
      'ValidationError: Identifier network does not match',
    );
    expect(parseIdentifier(`e1${H56}`, 1)).toMatchObject({ networkId: 1 });
    // DRep key hashes carry no network.
    expect(parseIdentifier(H56, 1)).toMatchObject({ kind: 'drep' });
  });
});
