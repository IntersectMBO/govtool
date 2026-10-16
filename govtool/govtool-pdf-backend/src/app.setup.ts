import type { INestApplication } from '@nestjs/common';
import cookieParser from 'cookie-parser';
import express, { NextFunction, Request, Response } from 'express';
import { ApiExceptionFilter, sendError, toApiError } from './common/api-exception.filter';
import { AppConfig } from './config/config';
import { parseQueryString } from './query/raw-query';

const CORS_METHODS = 'GET, POST, PUT, DELETE, OPTIONS';
const CORS_HEADERS = 'Authorization, Content-Type';

/**
 * §3.8. Reflects an allowed Origin (never `*`) with credentials; answers every
 * preflight; `Vary: Origin` on every response. A foreign origin under an
 * explicit list gets no CORS headers at all.
 */
export function corsMiddleware(config: AppConfig) {
  return (req: Request, res: Response, next: NextFunction) => {
    res.vary('Origin');
    const origin = req.headers.origin;
    const allowed =
      typeof origin === 'string' &&
      origin !== '' &&
      (config.corsOrigins === '*' || config.corsOrigins.includes(origin));
    if (allowed) {
      res.setHeader('Access-Control-Allow-Origin', origin);
      res.setHeader('Access-Control-Allow-Credentials', 'true');
    }
    if (req.method === 'OPTIONS') {
      if (allowed) {
        res.setHeader('Access-Control-Allow-Methods', CORS_METHODS);
        res.setHeader('Access-Control-Allow-Headers', CORS_HEADERS);
        res.setHeader('Access-Control-Max-Age', '600');
      }
      res.status(204).end();
      return;
    }
    next();
  };
}

/**
 * Everything main.ts and the e2e app factory share, so tests run the exact
 * production pipeline. Create the app with `{ bodyParser: false }`.
 */
export function configureApp(app: INestApplication, config: AppConfig): void {
  const server = app.getHttpAdapter().getInstance() as express.Express;
  server.disable('x-powered-by');
  // Express 5's "simple" parser does not understand brackets (§2).
  server.set('query parser', (s: string) => parseQueryString(s));

  app.use(corsMiddleware(config));
  app.use(cookieParser());
  app.use(express.json({ limit: config.bodyLimitBytes, strict: true }));
  // Body-parser failures happen before Nest routing; render them here.
  app.use((err: unknown, _req: Request, res: Response, next: NextFunction) => {
    if (err === undefined || err === null) return next();
    sendError(res, toApiError(err));
  });

  app.setGlobalPrefix('api', { exclude: ['health'] });
  app.useGlobalFilters(new ApiExceptionFilter());
  app.enableShutdownHooks();
}
