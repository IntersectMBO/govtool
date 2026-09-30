import { hasNulDeep, readString, readText } from './fields';

describe('NUL in string readers', () => {
  it('readString and readText refuse U+0000 as V `<field> is invalid`', () => {
    expect(() => readString({ f: 'a\u0000b' }, 'f')).toThrow('f is invalid');
    expect(() => readText({ t: '\u0000' }, 't')).toThrow('t is invalid');
    expect(readString({ f: 'fine' }, 'f')).toBe('fine');
  });
  it('hasNulDeep walks values and keys', () => {
    expect(hasNulDeep({ a: [{ b: 'x\u0000' }] })).toBe(true);
    expect(hasNulDeep({ ['k\u0000']: 1 })).toBe(true);
    expect(hasNulDeep({ a: [1, 'ok', null, { b: true }] })).toBe(false);
  });
});
