import 'reflect-metadata';

import * as Sentry from '@sentry/nestjs';
import { NestFactory } from '@nestjs/core';
import type { NestExpressApplication } from '@nestjs/platform-express';
import { AppModule } from './app.module';
import { ConfigService } from './config/config.service';
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger';
import * as express from 'express';

async function bootstrap() {
  const app = await NestFactory.create<NestExpressApplication>(AppModule, {
    cors: {
      origin: '*',
      methods: 'GET,HEAD,POST,OPTIONS',
      allowedHeaders: ['Authorization', 'Content-Type'],
    },
    logger: ['error', 'log', 'warn'],
  });

  const configService = app.get(ConfigService);
  const config = configService.get();

  if (config.sentryDsn) {
    Sentry.init({
      dsn: config.sentryDsn,
      environment: config.sentryEnv,
      sendDefaultPii: false,

      // Error reporting only.
      tracesSampleRate: 0,
    });
  }

  // Resolve the real client IP behind the reverse proxy (used by the
  // /ipfs/upload rate limiter).
  app.set('trust proxy', config.trustProxy);

  // Enables SIGTERM/SIGINT handling
  app.enableShutdownHooks();

  app.use(
    express.text({
      type: 'text/plain',
      limit: '600kb',
    }),
  );

  const swaggerConfig = new DocumentBuilder()
    .setTitle('GovTool Backend TS')
    .setDescription('GovTool backend API')
    .setVersion('1.0')
    .addServer('/')
    .build();
  const swaggerDocument = SwaggerModule.createDocument(app, swaggerConfig);
  SwaggerModule.setup('swagger-ui', app, swaggerDocument, {
    jsonDocumentUrl: 'swagger.json',
  });

  await app.listen(config.port, config.host);
  console.log(`listening on ${config.host}:${config.port}`);
}

void bootstrap().catch(async (error: unknown) => {
  console.error('Backend failed to start', error);

  Sentry.captureException(error);
  await Sentry.flush(2_000);

  process.exitCode = 1;
});
