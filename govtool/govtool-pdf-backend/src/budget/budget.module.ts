import { Module } from '@nestjs/common';
import { BdDraftsController } from './bd-drafts.controller';
import { BdPollsController } from './bd-polls.controller';
import { BdsController } from './bds.controller';
import { BdsService } from './bds.service';

/** Budget discussions, drafts, BD polls and votes (§8.8, §8.10, §8.11). */
@Module({
  controllers: [BdsController, BdDraftsController, BdPollsController],
  providers: [BdsService],
  exports: [BdsService],
})
export class BudgetModule {}
