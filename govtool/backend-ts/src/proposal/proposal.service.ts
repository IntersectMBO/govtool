import {
  Injectable,
  InternalServerErrorException,
  NotFoundException,
} from '@nestjs/common';

import { assertHexText } from 'src/common/hex';
import { DbService } from 'src/db/db.service';
import { SqlService } from 'src/sql/sq.service';
import {
  EnactedProposalDetailsResponse,
  EnactedProposalDetailsRow,
  GetProposalResponse,
  GovernanceActionSortMode,
  GovernanceActionType,
  ListProposalsResponse,
  ProposalResponse,
  Proposal,
} from './proposal.type';
import { CacheService } from 'src/cache/cache.service';

@Injectable()
export class ProposalService {
  constructor(
    private readonly dbService: DbService,
    private readonly sqlService: SqlService,
    private readonly cacheService: CacheService,
  ) {}

  async list(params: {
    type: GovernanceActionType[];
    sort?: GovernanceActionSortMode;
    page: number;
    pageSize: number;
    drepId?: string;
    search?: string;
  }): Promise<ListProposalsResponse> {
   return this.cacheService.getOrSet('proposalList',{
    type: params.type,
    sort: params.sort,
    page: params.page,
    pageSize: params.pageSize,
    drepId: params.drepId,
    search: params.search
   }, async()=> {
     if (params.drepId) {
      assertHexText(params.drepId);
    }
    const proposals = await this.getProposals('');

    let filtered = this.filterByType(proposals, params.type);
    filtered = this.filterBySearch(filtered, params.search);
    filtered = this.sortProposals(filtered, params.sort);

    const total = filtered.length;
    const start = params.page * params.pageSize;

    return {
      page: params.page,
      pageSize: params.pageSize,
      total,
      elements: filtered.slice(start, start + params.pageSize),
    };
   });
  }

  async get(proposalId: string, drepId?: string): Promise<GetProposalResponse> {
    const { txHash, index } = this.parseProposalId(proposalId);

    if (drepId) {
      assertHexText(drepId);
    }

    const proposals = await this.getProposals(`${txHash}#${index}`);

    if (proposals.length === 0) {
      throw new NotFoundException({
        errorType: 'NotFoundError',
        message: `Proposal with id: ${txHash}#${index} not found`,
      });
    }

    if (proposals.length !== 1) {
      throw new InternalServerErrorException({
        errorType: 'CriticalError',
        message: `Multiple proposals found for id: ${txHash}#${index}. This should never happen`,
      });
    }

    return {
      proposal: proposals[0],
      vote: null,
    };
  }

  async getEnactedDetails(
    type?: GovernanceActionType,
  ): Promise<EnactedProposalDetailsResponse | null> {
    const proposalType =
      type === 'ParameterChange' || type === 'HardForkInitiation'
        ? type
        : 'HardForkInitiation';

    const sql = this.sqlService.load(
      'get-previous-enacted-governance-action-proposal-details.sql',
    );

    const result = await this.dbService.query<EnactedProposalDetailsRow>(sql, [
      proposalType,
    ]);

    if (result.rows.length !== 1) {
      return null;
    }

    const row = result.rows[0];

    return {
      id: this.toInteger(row.id),
      txId: this.toInteger(row.tx_id),
      index: this.toInteger(row.index),
      description: row.description,
      hash: row.hash,
    };
  }

  async getProposals(search: string): Promise<ProposalResponse[]> {
  return this.cacheService.getOrSetStaleWhileRevalidate(
    this.proposalListSnapshotNamespace,
    search,
    () => this.fetchProposals(search),
  );
  }

  private toProposalResponse(row: Proposal): ProposalResponse {
    return {
      id: String(row.id),
      txHash: row.tx_hash,
      index: this.toInteger(row.index),
      type: this.toGovernanceActionType(row.type),
      details: row.description,
      expiryDate: this.toNullableIsoString(row.expiry_date),
      expiryEpochNo: this.toNullableInteger(row.expiration),
      createdDate: this.toIsoString(row.time),
      createdEpochNo: this.toInteger(row.epoch_no),
      url: row.url,
      metadataHash: row.data_hash,
      protocolParams: row.proposal_params,
      title: row.title,
      abstract: row.abstract,
      motivation: row.motivation,
      rationale: row.rationale,
      dRepYesVotes: this.toInteger(row.yes_votes),
      dRepNoVotes: this.toInteger(row.no_votes),
      dRepAbstainVotes: this.toInteger(row.abstain_votes),
      poolYesVotes: this.toInteger(row.pool_yes_votes),
      poolNoVotes: this.toInteger(row.pool_no_votes),
      poolAbstainVotes: this.toInteger(row.pool_abstain_votes),
      ccYesVotes: this.toInteger(row.cc_yes_votes),
      ccNoVotes: this.toInteger(row.cc_no_votes),
      ccAbstainVotes: this.toInteger(row.cc_abstain_votes),
      prevGovActionIndex: this.toNullableInteger(row.prev_gov_action_index),
      prevGovActionTxHash: row.prev_gov_action_tx_hash,
      json: row.json_content,
      authors: row.authors,
    };
  }

  filterByType(
    proposals: ProposalResponse[],
    selectedTypes: GovernanceActionType[],
  ): ProposalResponse[] {
    if (selectedTypes.length === 0) {
      return proposals;
    }

    return proposals.filter((proposal) => selectedTypes.includes(proposal.type));
  }

  filterBySearch(
    proposals: ProposalResponse[],
    search?: string,
  ): ProposalResponse[] {
    if (!search) {
      return proposals;
    }

    const searchLower = search.toLowerCase();

    return proposals.filter((proposal) => {
      const govActionId = `${proposal.txHash}#${proposal.index}`;
      const values = [
        govActionId,
        proposal.title,
        proposal.abstract,
        proposal.motivation,
        proposal.rationale,
      ].filter((value): value is string => value !== null);

      return values.some((value) => value.toLowerCase().includes(searchLower));
    });
  }

  sortProposals(
    proposals: ProposalResponse[],
    sort?: GovernanceActionSortMode,
  ): ProposalResponse[] {
    const copied = [...proposals];

    switch (sort) {
      case 'NewestCreated':
        return copied.sort(
          (a, b) => Date.parse(b.createdDate) - Date.parse(a.createdDate),
        );

      case 'SoonestToExpire':
        return copied.sort(
          (a, b) =>
            this.nullableDateSortValue(a.expiryDate) -
            this.nullableDateSortValue(b.expiryDate),
        );

      case 'MostYesVotes':
        return copied.sort(
          (a, b) => this.totalYesVotes(b) - this.totalYesVotes(a),
        );

      default:
        return copied;
    }
  }

  private totalYesVotes(proposal: ProposalResponse): number {
    return proposal.dRepYesVotes + proposal.poolYesVotes + proposal.ccYesVotes;
  }

  private nullableDateSortValue(value: string | null): number {
    return value === null ? Number.MAX_SAFE_INTEGER : Date.parse(value);
  }

  private parseProposalId(proposalId: string): { txHash: string; index: number } {
    const [txHash, rawIndex] = proposalId.split('#');

    if (!txHash || rawIndex === undefined || rawIndex === '') {
      throw new NotFoundException({
        errorType: 'NotFoundError',
        message: `Proposal with id: ${proposalId} not found`,
      });
    }

    assertHexText(txHash);

    const index = Number(rawIndex);

    if (!Number.isInteger(index)) {
      throw new NotFoundException({
        errorType: 'NotFoundError',
        message: `Proposal with id: ${proposalId} not found`,
      });
    }

    return { txHash, index };
  }

  private toGovernanceActionType(value: string): GovernanceActionType {
    const knownTypes: GovernanceActionType[] = [
      'ParameterChange',
      'HardForkInitiation',
      'TreasuryWithdrawals',
      'NoConfidence',
      'NewCommittee',
      'NewConstitution',
      'InfoAction',
    ];

    return knownTypes.includes(value as GovernanceActionType)
      ? (value as GovernanceActionType)
      : 'InfoAction';
  }

  private toInteger(value: number | string): number {
    return Math.floor(Number(value));
  }

  private toNullableInteger(value: number | string | null): number | null {
    return value === null ? null : this.toInteger(value);
  }

  private toIsoString(value: Date | string): string {
    return value instanceof Date ? value.toISOString() : new Date(value).toISOString();
  }

  private toNullableIsoString(value: Date | string | null): string | null {
    return value === null ? null : this.toIsoString(value);
  }
  private readonly proposalListSnapshotNamespace = 'proposalListSnapshot';

async warmActiveProposalSnapshot(): Promise<void> {
  await this.cacheService.refresh(
    this.proposalListSnapshotNamespace,
    '',
    () => this.fetchProposals(''),
  );
}



private async fetchProposals(search: string): Promise<ProposalResponse[]> {
  const sql = this.sqlService.load('list-proposals.sql');

  const result = await this.dbService.query<Proposal>(sql, [
     search,
      `%${search}%`,
      `%${search}%`,
      `%${search}%`,
      `%${search}%`,
      search,
  ]);
  return result.rows.map((row) => this.toProposalResponse(row));
}
}
