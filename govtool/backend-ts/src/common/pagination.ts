import { BadRequestException } from '@nestjs/common';

export function parsePageValue(
  value: unknown,
  fallback: number,
  name: string,
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
  return Number(value);
}
