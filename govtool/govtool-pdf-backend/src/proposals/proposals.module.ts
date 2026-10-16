import { Module } from '@nestjs/common';
import { ProposalVotesController } from './proposal-votes.controller';
import { ProposalVotesService } from './proposal-votes.service';
import { ProposalsController } from './proposals.controller';
import { ProposalsService } from './proposals.service';

/** Proposals, proposal contents and proposal votes (§8.2–§8.4). */
@Module({
  controllers: [ProposalsController, ProposalVotesController],
  providers: [ProposalsService, ProposalVotesService],
})
export class ProposalsModule {}
