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
