import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';

import { ConfigService } from './config/config.service';
import { DbService } from './db/db.service';
import { HealthController } from './health/health.controller';
import { NetworkController } from './network/network.controller';
import { NetworkService } from './network/network.service';
import { SqlService } from './sql/sq.service';


@Module({
  imports: [],
  controllers: [AppController, HealthController, NetworkController],
  providers: [AppService, ConfigService, DbService, NetworkService, SqlService],
  exports: [ConfigService, DbService]
})
export class AppModule {}
