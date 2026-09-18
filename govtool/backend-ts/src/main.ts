import 'reflect-metadata';
import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import { ConfigService } from './config/config.service';
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger';
import * as express from 'express';


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
  app.use(express.text({ type: 'text/plain', limit: '600kb'}));
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

bootstrap();
