import { InternalServerErrorException } from '@nestjs/common';

// Safe values remain numbers; larger database integers never pass through Number().
export type ApiInteger = number | bigint;

export function dbInteger(value: number | string): ApiInteger {
  if (typeof value === 'number') {
    if (Number.isSafeInteger(value)) return value;
  } else if (/^-?\d+(?:\.0+)?$/.test(value)) {
    const integer = BigInt(value.split('.')[0]);
    if (
      integer >= BigInt(Number.MIN_SAFE_INTEGER) &&
      integer <= BigInt(Number.MAX_SAFE_INTEGER)
    )
      return Number(integer);
    return integer;
  }
  throw new InternalServerErrorException({
    errorType: 'CriticalError',
    message: 'Expected an exact integer from the database',
  });
}

export function safeDbInteger(value: number | string): number {
  const integer = dbInteger(value);
  if (typeof integer === 'bigint') {
    throw new InternalServerErrorException({
      errorType: 'CriticalError',
      message: 'Database integer exceeds the supported range for this field',
    });
  }
  return integer;
}

export function compareIntegers(a: ApiInteger, b: ApiInteger): number {
  return a < b ? -1 : a > b ? 1 : 0;
}
