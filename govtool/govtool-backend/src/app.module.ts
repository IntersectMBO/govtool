import { Module } from '@nestjs/common';
import { APP_INTERCEPTOR } from '@nestjs/core';

import { IntegerJsonInterceptor } from './common/integer-json.interceptor';
import { AppController } from './app.controller';
import { AppService } from './app.service';

import { HealthController } from './health/health.controller';
import { NetworkController } from './network/network.controller';
import { NetworkService } from './network/network.service';
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
import { IpfsController } from './ipfs/ipfs.controller';
import { IpfsService } from './ipfs/ipfs.service';
import { SystemController } from './system/system.controller';
import { SystemService } from './system/system.service';
import { CacheService } from './cache/cache.service';
import { CacheWarmerService } from './cache/cache-warmer.service';
import { MetadataModule } from './metadata/metadata.module';
import { SurveyController } from './survey/survey.controller';
import { SurveyService } from './survey/survey.service';
import { ProvidersModule } from './providers/providers.module';

/**
 * There is no `DbService` or `SqlService` here any more: every read goes
 * through the Chain Data contract, and every pin through the pinning
 * contract, both supplied by `ProvidersModule`. This module owns the HTTP
 * surface, the legacy response shapes and the cache — nothing else.
 */
@Module({
  imports: [ProvidersModule, MetadataModule],
  controllers: [
    AppController,
    HealthController,
    NetworkController,
    EpochController,
    TransactionController,
    AdaHolderController,
    AccountController,
    DRepController,
    ProposalController,
    IpfsController,
    SystemController,
    SurveyController,
  ],
  providers: [
    // Writes bigint response fields as unquoted JSON numbers, so a lovelace
    // value above the safe range reaches the wire exactly.
    { provide: APP_INTERCEPTOR, useClass: IntegerJsonInterceptor },
    AppService,
    NetworkService,
    EpochService,
    TransactionService,
    AdaHolderService,
    AccountService,
    DRepService,
    ProposalService,
    IpfsService,
    CacheService,
    CacheWarmerService,
    SurveyService,
    SystemService,
  ],
})
export class AppModule {}
