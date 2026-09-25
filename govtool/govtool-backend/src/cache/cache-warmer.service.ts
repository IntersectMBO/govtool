import {
  Inject,
  Injectable,
  Logger,
  OnModuleDestroy,
  OnModuleInit,
} from '@nestjs/common';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { DRepService } from 'src/drep/drep.service';
import { ProposalService } from 'src/proposal/proposal.service';
import { CHAIN_DATA } from 'src/providers/providers.module';

@Injectable()
export class CacheWarmerService implements OnModuleDestroy, OnModuleInit {
  private readonly logger = new Logger(CacheWarmerService.name);
  private timer?: NodeJS.Timeout;
  private refreshing = false;
  private lastBlockNo: number | null = null;

  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly drepService: DRepService,
    private readonly proposalService: ProposalService,
  ) {}

  async onModuleInit(): Promise<void> {
    await this.refreshIfNeeded(true);

    this.timer = setInterval(() => {
      void this.refreshIfNeeded(false);
    }, 20_000);
  }

  onModuleDestroy(): void {
    if (this.timer) {
      clearInterval(this.timer);
    }
  }

  private async refreshIfNeeded(force: boolean): Promise<void> {
    if (this.refreshing) {
      return;
    }

    const latestBlockNo = await this.getLatestBlockNo();

    if (
      !force &&
      latestBlockNo !== null &&
      latestBlockNo === this.lastBlockNo
    ) {
      return;
    }

    this.refreshing = true;
    const startedAt = Date.now();

    try {
      await Promise.all([
        this.drepService.warmDefaultListSnapshot(),
        this.proposalService.warmActiveProposalSnapshot(),
      ]);

      this.lastBlockNo = latestBlockNo;

      this.logger.log(
        `Snapshot caches refreshed in ${Date.now() - startedAt}ms`,
      );
    } catch (error) {
      this.logger.error(
        'Failed to refresh snapshot caches',
        error instanceof Error ? error.stack : String(error),
      );
    } finally {
      this.refreshing = false;
    }
  }

  /**
   * The tip comes from the provider's own health check now, rather than a
   * `SELECT MAX(block_no)` issued from here — the backend no longer holds a
   * database handle.
   */
  private async getLatestBlockNo(): Promise<number | null> {
    try {
      const { data } = await this.chain.system.getHealth();
      return data.tip?.block ?? null;
    } catch (error) {
      this.logger.warn(
        `Could not read the chain tip: ${error instanceof Error ? error.message : String(error)}`,
      );
      return null;
    }
  }
}
