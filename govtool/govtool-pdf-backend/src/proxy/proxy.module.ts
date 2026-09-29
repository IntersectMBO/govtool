import { Module } from '@nestjs/common';
import { ProxyController } from './proxy.controller';
import { ProxyService } from './proxy.service';

/** The GovTool proxy and the safe fetcher (§9). */
@Module({
  controllers: [ProxyController],
  providers: [ProxyService],
})
export class ProxyModule {}
