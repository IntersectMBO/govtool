import { NestFactory } from '@nestjs/core';
import { ValidationPipe } from '@nestjs/common';
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger';

import { AppModule } from './app.module';

async function bootstrap() {
  const app = await NestFactory.create(AppModule, { cors: true });

  const config = new DocumentBuilder()
    .setTitle('Submission Tool')
    .setDescription('The Submission Tool API description')
    .setVersion('1.0')
    .addTag('submission')
    .build();

  const document = SwaggerModule.createDocument(app, config);
  SwaggerModule.setup('api', app, document);

  app.useGlobalPipes(
    new ValidationPipe({
      // Do not throw error on missing fields
      exceptionFactory: () => ({ status: 200, valid: false }),
    }),
  );
  await app.listen(process.env.PORT);
}
bootstrap();
