import { isValidURLFormat, isValidHashFormat } from '..';

describe('isValidURLFormat', () => {
  it('returns true for valid HTTP URLs', () => {
    const validHttpUrl = 'http://example.com';
    expect(isValidURLFormat(validHttpUrl)).toBe(true);
  });

  it('returns true for valid HTTPS URLs', () => {
    const validHttpsUrl = 'https://example.com';
    expect(isValidURLFormat(validHttpsUrl)).toBe(true);
  });

  it('returns true for valid HTTPS URLs with IP', () => {
    const validHttpsUrl = 'http://192.168.0.1/resoruce';
    expect(isValidURLFormat(validHttpsUrl)).toBe(true);
  });

  it('returns true for valid IPFS URLs', () => {
    const validIpfsUrl =
      'ipfs://c94ae10c7bbc2632f051cadcec61e24a954aa1d61173597b21c03534';
    expect(isValidURLFormat(validIpfsUrl)).toBe(true);
  });

  it('returns false for invalid URLs', () => {
    const invalidUrl = 'htp:/example.com';
    expect(isValidURLFormat(invalidUrl)).toBe(false);
  });

  it('returns false for invalid URLs without domain', () => {
    const invalidUrl = 'https://invalid';
    expect(isValidURLFormat(invalidUrl)).toBe(false);
  });

  it('returns false for strings that are not URLs', () => {
    const notUrl = 'Just a string';
    expect(isValidURLFormat(notUrl)).toBe(false);
  });

  it('returns true for empty string', () => {
    const empty = '';
    expect(isValidURLFormat(empty)).toBe(true);
  });
});

describe('isValidHashFormat', () => {
  it('returns true for valid hexadecimal strings', () => {
    const validHash = '1a3B';
    expect(isValidHashFormat(validHash)).toBe(true);
  });

  it('returns false for non-hexadecimal strings', () => {
    const invalidHash = 'GHIJKLMNOP';
    expect(isValidHashFormat(invalidHash)).toBe(false);
  });

  it('returns false for hexadecimal strings with spaces', () => {
    const invalidHash = '1a 3B';
    expect(isValidHashFormat(invalidHash)).toBe(false);
  });

  it('returns true for empty string', () => {
    const empty = '';
    expect(isValidHashFormat(empty)).toBe(true);
  });
});
