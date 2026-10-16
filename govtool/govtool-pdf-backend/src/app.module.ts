import { Module } from '@nestjs/common';
import { AuthModule } from './auth/auth.module';
import { CommentsModule } from './comments/comments.module';
import { ConfigModule } from './config/config.module';
import { HealthController } from './health/health.controller';
import { LookupsModule } from './lookups/lookups.module';
import { PollsModule } from './polls/polls.module';
import { PrismaModule } from './prisma/prisma.module';
import { ProposalsModule } from './proposals/proposals.module';
import { ProxyModule } from './proxy/proxy.module';

@Module({
  imports: [
    ConfigModule,
    PrismaModule,
    AuthModule,
    LookupsModule,
    ProposalsModule,
    CommentsModule,
    PollsModule,
    ProxyModule,
  ],
  controllers: [HealthController],
})
export class AppModule {}
