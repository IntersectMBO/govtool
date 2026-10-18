import { Module } from '@nestjs/common';
import { PollsController } from './polls.controller';
import { PollsService } from './polls.service';

/** Polls and poll votes (§8.5, §8.6). */
@Module({
  controllers: [PollsController],
  providers: [PollsService],
})
export class PollsModule {}
