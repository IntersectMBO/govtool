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
import { AdaHolderController } from './ada-holder/ada-holder.controller';
import { AdaHolderService } from './ada-holder/ada-holder.service';
import { AccountController } from './account/account.controller';
import { AccountService } from './account/account.service';
import { DRepController } from './drep/drep.controller';
import { DRepService } from './drep/drep.service';
import { ProposalController } from './proposal/proposal.controller';
import { ProposalService } from './proposal/proposal.service';


@Module({
  imports: [],
  controllers: [AppController, HealthController, NetworkController, EpochController, TransactionController, AdaHolderController, AccountController,DRepController,ProposalController],
  providers: [AppService, ConfigService, DbService, NetworkService, SqlService, EpochService, TransactionService, AdaHolderService, AccountService, DRepService,ProposalService],
  exports: [ConfigService, DbService]
})
export class AppModule {}
