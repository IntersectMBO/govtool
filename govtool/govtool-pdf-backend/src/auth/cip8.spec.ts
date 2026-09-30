import { baseAddress, drepIdentifier, newKey, signCip8 } from '../../test/helpers/cip8-signer';
import { verifyCip8 } from './cip8';

const MESSAGE = 'To proceed, please sign this data.\nNonce: 00112233445566778899aabbccddeeff\nTimestamp: 1';

describe('verifyCip8', () => {
  const key = newKey();
  const keyHash = drepIdentifier(key);

  it('accepts a valid signature over the challenge', async () => {
    const s = await signCip8(key, MESSAGE);
    await expect(verifyCip8({ ...s, message: MESSAGE, expectedKeyHash: keyHash })).resolves.toBe(true);
  });

  it('accepts a base address in the protected header (Playwright wallet)', async () => {
    const s = await signCip8(key, MESSAGE, { address: baseAddress(newKey(), key) });
    await expect(verifyCip8({ ...s, message: MESSAGE, expectedKeyHash: keyHash })).resolves.toBe(true);
  });

  it('accepts a hashed: true payload', async () => {
    const s = await signCip8(key, MESSAGE, { hashed: true });
    await expect(verifyCip8({ ...s, message: MESSAGE, expectedKeyHash: keyHash })).resolves.toBe(true);
  });

  it('rejects a raw payload flagged hashed and vice versa', async () => {
    const s = await signCip8(key, MESSAGE, { hashed: true });
    await expect(verifyCip8({ ...s, message: 'other', expectedKeyHash: keyHash })).resolves.toBe(false);
  });

  it('rejects a key whose hash is not the expected one', async () => {
    const other = newKey();
    const s = await signCip8(other, MESSAGE);
    await expect(verifyCip8({ ...s, message: MESSAGE, expectedKeyHash: keyHash })).resolves.toBe(false);
  });

  it('rejects a payload that is not the challenge (Δ15)', async () => {
    const s = await signCip8(key, 'an old message');
    await expect(verifyCip8({ ...s, message: MESSAGE, expectedKeyHash: keyHash })).resolves.toBe(false);
  });

  it('rejects a tampered signature', async () => {
    const s = await signCip8(key, MESSAGE, { tamper: true });
    await expect(verifyCip8({ ...s, message: MESSAGE, expectedKeyHash: keyHash })).resolves.toBe(false);
  });

  it('rejects alg other than -8', async () => {
    const s = await signCip8(key, MESSAGE, { alg: -7 });
    await expect(verifyCip8({ ...s, message: MESSAGE, expectedKeyHash: keyHash })).resolves.toBe(false);
  });

  it('rejects a key type other than OKP', async () => {
    const s = await signCip8(key, MESSAGE, { keyType: 2 });
    await expect(verifyCip8({ ...s, message: MESSAGE, expectedKeyHash: keyHash })).resolves.toBe(false);
  });

  it('rejects a signature bound to a different key hash pair (swapped key)', async () => {
    const s = await signCip8(key, MESSAGE);
    const other = await signCip8(newKey(), MESSAGE);
    await expect(
      verifyCip8({ signature: s.signature, key: other.key, message: MESSAGE, expectedKeyHash: keyHash }),
    ).resolves.toBe(false);
  });

  it.each([
    ['non-hex', 'zz', 'zz'],
    ['odd length', 'abc', 'abc'],
    ['empty', '', ''],
    ['non-CBOR', 'ff'.repeat(40), 'ff'.repeat(20)],
    ['CBOR integer', '01', '01'],
    ['CBOR text', '6161', '6161'],
    ['non-string', 42, { a: 1 }],
  ])('fails without throwing on %s input', async (_label, signature, k) => {
    await expect(verifyCip8({ signature, key: k, message: MESSAGE, expectedKeyHash: keyHash })).resolves.toBe(
      false,
    );
  });

  it('fails without throwing on truncated input', async () => {
    const s = await signCip8(key, MESSAGE);
    await expect(
      verifyCip8({
        signature: s.signature.slice(0, 40),
        key: s.key,
        message: MESSAGE,
        expectedKeyHash: keyHash,
      }),
    ).resolves.toBe(false);
    await expect(
      verifyCip8({
        signature: s.signature,
        key: s.key.slice(0, 20),
        message: MESSAGE,
        expectedKeyHash: keyHash,
      }),
    ).resolves.toBe(false);
  });
});
