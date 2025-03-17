import { NestFactory } from '@nestjs/core';
import { ValidationPipe } from '@nestjs/common';
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger';

import { AppModule } from './app.module';

async function bootstrap() {
  const app = await NestFactory.create(AppModule, {
    cors: true,
    logger: ['error', 'log'],
  });

  const config = new DocumentBuilder()
    .setTitle('Metadata Validation Tool')
    .setDescription('The Metadata Validation Tool API description')
    .setVersion("2.0.16")
    .build();

  const document = SwaggerModule.createDocument(app, config);
  SwaggerModule.setup('api', app, document);

  app.useGlobalPipes(new ValidationPipe());

  await app.listen(process.env.PORT);
}
bootstrap();
