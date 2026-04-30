import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';

import { ConfigService } from './config/config.service';
import { DbService } from './db/db.service';
import { HealthController } from './health/health.controller';
import { NetworkController } from './network/network.controller';
import { NetworkService } from './network/network.service';
import { SqlService } from './sql/sq.service';
import { EpochController } from './epoch/epoch.controller';
import { EpochService } from './epoch/epoch.service';
import { TransactionController } from './transaction/transaction.controller';
import { TransactionService } from './transaction/transaction.service';


@Module({
  imports: [],
  controllers: [AppController, HealthController, NetworkController, EpochController, TransactionController],
  providers: [AppService, ConfigService, DbService, NetworkService, SqlService, EpochService, TransactionService],
  exports: [ConfigService, DbService]
})
export class AppModule {}
