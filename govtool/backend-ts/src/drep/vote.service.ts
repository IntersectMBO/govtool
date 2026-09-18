import { Injectable } from '@nestjs/common';
import { assertHexText } from 'src/common/hex';
import { DbService } from 'src/db/db.service';
import { SqlService } from 'src/sql/sq.service';
import { CacheService } from 'src/cache/cache.service';
import { DRepVoteRow, VoteParams } from './drep.type';

@Injectable()
export class VoteService {
  constructor(
    private readonly db: DbService,
    private readonly sql: SqlService,
    private readonly cache: CacheService,
  ) {}

  getVotes(drepId: string): Promise<DRepVoteRow[]> {
    assertHexText(drepId);
    return this.cache.getOrSet('voteRows', drepId.toLowerCase(), async () => {
      const result = await this.db.query<DRepVoteRow>(
        this.sql.load('get-votes.sql'),
        [drepId],
      );
      return result.rows;
    });
  }

  toVoteParams(row: DRepVoteRow): VoteParams {
    return {
      proposalId: String(row.proposal_id),
      drepId: row.drep_id,
      vote: row.vote,
      url: row.url,
      metadataHash: row.doc_hash,
      epochNo: Number(row.epoch_no),
      date: new Date(row.date).toISOString(),
      txHash: row.vote_tx_hash,
    };
  }
}
