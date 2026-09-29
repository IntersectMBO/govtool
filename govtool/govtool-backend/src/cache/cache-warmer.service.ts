import {
  Inject,
  Injectable,
  Logger,
  OnModuleDestroy,
  OnModuleInit,
  Optional,
} from '@nestjs/common';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { DRepService } from 'src/drep/drep.service';
import { OutcomesService } from 'src/outcomes/outcomes.service';
import { ProposalService } from 'src/proposal/proposal.service';
import { CHAIN_DATA } from 'src/providers/providers.module';

const SNAPSHOTS = ['DRep list', 'proposal list'] as const;

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
    @Optional() private readonly outcomes?: OutcomesService,
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
      // allSettled, not all: `all` rejects on the first failure while the
      // other warm is still running, and clearing `refreshing` then lets the
      // next tick start a second copy of it. Against a slow provider that
      // piles up one overlapping snapshot per block.
      const results = await Promise.allSettled([
        this.drepService.warmDefaultListSnapshot(),
        this.proposalService.warmActiveProposalSnapshot(),
      ]);
      const failures = results.flatMap((result, i) =>
        result.status === 'rejected'
          ? [{ what: SNAPSHOTS[i], reason: result.reason as unknown }]
          : [],
      );

      for (const { what, reason } of failures) {
        this.logger.error(
          `Failed to refresh the ${what} snapshot`,
          reason instanceof Error ? reason.stack : String(reason),
        );
      }
      if (failures.length === 0) {
        // Only a complete refresh marks the block done; otherwise the next
        // tick retries even if no new block has arrived.
        this.lastBlockNo = latestBlockNo;
        this.logger.log(
          `Snapshot caches refreshed in ${Date.now() - startedAt}ms`,
        );
        // Not awaited: document text comes from the metadata service and
        // must not hold up the next snapshot refresh.
        void this.outcomes?.warmSearchText().catch((error: unknown) => {
          this.logger.warn(
            `Could not warm outcomes search text: ${error instanceof Error ? error.message : String(error)}`,
          );
        });
      }
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
