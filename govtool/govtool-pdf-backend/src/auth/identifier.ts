import { validationError } from '../common/errors';

export type LoginIdentifier =
  | { kind: 'stake'; identifier: string; keyHash: string; networkId: number }
  | { kind: 'drep'; identifier: string; keyHash: string };

/**
 * SPEC §7.1. `raw` is lowercased; a 58-hex key-hash reward address (header
 * e0/e1) is a stake login, a 56-hex key hash is a DRep login. Script reward
 * addresses (f0/f1) cannot sign and are rejected. With `networkId` set, the
 * reward address header's low nibble must match it.
 */
export function parseIdentifier(raw: unknown, networkId: 0 | 1 | null): LoginIdentifier {
  if (typeof raw !== 'string') throw validationError('Invalid identifier');
  const id = raw.trim().toLowerCase();
  if (!/^[0-9a-f]+$/.test(id)) throw validationError('Invalid identifier');
  if (id.length === 58) {
    const header = id.slice(0, 2);
    if (header !== 'e0' && header !== 'e1') throw validationError('Invalid identifier');
    const net = header === 'e1' ? 1 : 0;
    if (networkId !== null && net !== networkId) {
      throw validationError('Identifier network does not match');
    }
    return { kind: 'stake', identifier: id, keyHash: id.slice(2), networkId: net };
  }
  if (id.length === 56) return { kind: 'drep', identifier: id, keyHash: id };
  throw validationError('Invalid identifier');
}
