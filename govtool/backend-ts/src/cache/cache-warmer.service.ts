import { Injectable,Logger,OnModuleDestroy,OnModuleInit } from "@nestjs/common";
import { DbService } from "src/db/db.service";
import { DRepService } from "src/drep/drep.service";
import { ProposalService } from "src/proposal/proposal.service";

type LatestBlockRow = {
    block_no: string | number | null;
}

@Injectable()
export class CacheWarmerService implements OnModuleDestroy, OnModuleInit {
    private readonly logger = new Logger(CacheWarmerService.name);
    private timer?: NodeJS.Timeout;
    private refreshing = false;
    private lastBlockNo: number | null = null

    constructor (
        private readonly dbService : DbService,
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

    if (!force && latestBlockNo !== null && latestBlockNo === this.lastBlockNo) {
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

   private async getLatestBlockNo(): Promise<number | null> {
    const result = await this.dbService.query<LatestBlockRow>(
      'SELECT MAX(block_no) AS block_no FROM block',
    );

    const blockNo = result.rows[0]?.block_no;

    if (blockNo === null || blockNo === undefined) {
      return null;
    }

    return Number(blockNo);
  }

}