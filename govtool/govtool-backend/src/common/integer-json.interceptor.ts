import {
  CallHandler,
  ExecutionContext,
  Injectable,
  NestInterceptor,
} from '@nestjs/common';
import { map } from 'rxjs';

// Node 22 supports rawJSON; keep the existing numeric JSON contract without rounding
// or changing large values into strings. Do not patch BigInt.prototype globally.
function jsonIntegers(value: unknown): unknown {
  if (typeof value === 'bigint') {
    return (JSON as typeof JSON & { rawJSON(text: string): object }).rawJSON(
      value.toString(),
    );
  }
  if (Array.isArray(value)) return value.map(jsonIntegers);
  if (
    value &&
    typeof value === 'object' &&
    Object.getPrototypeOf(value) === Object.prototype
  ) {
    return Object.fromEntries(
      Object.entries(value).map(([key, item]) => [key, jsonIntegers(item)]),
    );
  }
  return value;
}

@Injectable()
export class IntegerJsonInterceptor implements NestInterceptor {
  intercept(_context: ExecutionContext, next: CallHandler) {
    return next.handle().pipe(map((value: unknown) => jsonIntegers(value)));
  }
}
