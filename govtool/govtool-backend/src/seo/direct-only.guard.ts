import {
  type CanActivate,
  type ExecutionContext,
  Injectable,
  NotFoundException,
} from '@nestjs/common';
import type { Request } from 'express';

/**
 * Headers a reverse proxy adds on the way in. The public gateway (and a
 * Kubernetes ingress) always sets them and a client cannot take them off,
 * while the frontend's nginx calls the backend directly and strips them.
 */
const PROXY_HEADERS = [
  'x-forwarded-for',
  'x-real-ip',
  'forwarded',
  'x-forwarded-host',
  'x-forwarded-proto',
  'x-forwarded-port',
];

export const cameThroughProxy = (headers: Request['headers']): boolean =>
  PROXY_HEADERS.some((name) => headers[name] !== undefined);

/**
 * The SEO routes answer only the frontend's nginx. Reached from outside, as
 * /api/seo/... through the gateway, they would bypass nginx's card cache and
 * rate limit and let anyone drive the card renderer, so they answer 404.
 */
@Injectable()
export class DirectOnlyGuard implements CanActivate {
  canActivate(context: ExecutionContext): boolean {
    const request = context.switchToHttp().getRequest<Request>();
    if (cameThroughProxy(request.headers)) throw new NotFoundException();
    return true;
  }
}
