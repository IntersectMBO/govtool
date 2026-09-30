import { BadRequestException } from '@nestjs/common';

export function queryEnum<T extends string>(
  value: unknown,
  allowed: readonly T[],
  name: string,
): T | undefined {
  if (value === undefined) return undefined;
  if (typeof value !== 'string' || !allowed.includes(value as T)) {
    throw new BadRequestException({
      errorType: 'ValidationError',
      message: `${name} must be one of: ${allowed.join(', ')}`,
    });
  }
  return value as T;
}

export function queryEnums<T extends string>(
  value: unknown,
  bracketed: unknown,
  allowed: readonly T[],
  name: string,
): T[] {
  const values = [value, bracketed].flatMap((item) =>
    item === undefined
      ? []
      : Array.isArray(item)
        ? (item as unknown[])
        : [item],
  );
  return values.map((item) => queryEnum(item, allowed, name)!);
}
