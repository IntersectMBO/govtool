import { BadRequestException } from '@nestjs/common';

// Caps the rows a single request can pull into memory and onto the wire.
export const MAX_PAGE_SIZE = 100;

export function parsePageValue(
  value: unknown,
  fallback: number,
  name: string,
  max?: number,
): number {
  if (value === undefined) return fallback;
  if (
    typeof value !== 'string' ||
    !/^\d+$/.test(value) ||
    !Number.isSafeInteger(Number(value))
  ) {
    throw new BadRequestException({
      errorType: 'ValidationError',
      message: `${name} must be a non-negative safe integer`,
    });
  }

  const parsed = Number(value);

  if (max !== undefined && parsed > max) {
    throw new BadRequestException({
      errorType: 'ValidationError',
      message: `${name} must not be greater than ${max}`,
    });
  }

  return parsed;
}
