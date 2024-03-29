import { Module } from '@nestjs/common';
import { HttpModule } from '@nestjs/axios';

import { AppController } from './app.controller';
import { AppService } from './app.service';

@Module({
  imports: [
    HttpModule.register({
      timeout: 5000,
      maxRedirects: 5,
    }),
  ],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
