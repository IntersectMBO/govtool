import 'reflect-metadata';
import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import { ConfigService } from './config/config.service';

async function bootstrap() {
  const app = await NestFactory.create(AppModule, {
    cors: {
      origin: '*',
      methods: 'GET,HEAD,POST,OPTIONS',
      allowedHeaders: ['Authorization', 'Content-Type'],
    },
    logger: ['error', 'log', 'warn']
  });
  const configService = app.get(ConfigService);
  const config = configService.get()
  await app.listen(config.port, config.host);
  console.log(`listening on ${config.host}:${config.port}`);
}

bootstrap();
