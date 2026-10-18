import { BadRequestException } from '@nestjs/common';

export function assertHexText(value: string): void {
  const isHex = /^[0-9a-fA-F]+$/.test(value);
  const hasEvenLength = value.length % 2 === 0;

  if (!isHex || !hasEvenLength) {
    throw new BadRequestException({
      error: 'Not a valid hex value',
    });
  }
}

/**
 * A guard for an entity identifier on a path.
 *
 * The legacy routes took db-sync's raw hex credential hashes and `assertHexText`
 * was the whole check. One bech32 identifier per entity travels now — a
 * CIP-129 DRep or action id, a `stake1…` address, a `pool1…` id — so hex is no
 * longer the shape to demand. The charset is still restricted, because this
 * value is handed straight to a provider.
 */
export function assertIdentifier(value: string): void {
  if (!/^[0-9a-zA-Z_]{1,255}$/.test(value)) {
    throw new BadRequestException({
      error: 'Not a valid identifier',
    });
  }
}
