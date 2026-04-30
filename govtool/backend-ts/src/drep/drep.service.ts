import { Injectable, InternalServerErrorException } from "@nestjs/common";

import { assertHexText } from "src/common/hex";
import { DbService } from "src/db/db.service";
import { SqlService } from "src/sql/sq.service";

import { DRepInfo, DRepInfoResponse, DRepVotingPower, DRepVotingPowerList,DRepVotingPowerListResponse , DRepListItem, DRepListResponse, DRepList, DRepListSort,DRepStatus,DRepType,DRepVoteRow,VoteParams,VoteResponse,
} from "./drep.type";
import { ProposalService } from 'src/proposal/proposal.service';
import {
  GovernanceActionSortMode,
  GovernanceActionType,
  ProposalResponse,
} from 'src/proposal/proposal.type';

@Injectable()
export class DRepService {
    constructor(
        private readonly dbService: DbService,
        private readonly sqlService: SqlService,
        private readonly proposalService: ProposalService,
    ){}

    async getVotingPower(drepId: string): Promise<number> {
        assertHexText(drepId);

        const sql = this.sqlService.load('get-voting-power.sql');
        const result = await this.dbService.query<DRepVotingPower>(sql, [drepId]);

        if(result.rows.length === 0){
            return 0;
        }

        return this.toInteger(result.rows[0].amount);
    }

    async getVotingPowerList (
        identifiers: string[],
    ): Promise<DRepVotingPowerListResponse[]> {
        const rows: DRepVotingPowerList[]=[];

        if (identifiers.length === 0) {
            const sql = this.sqlService.load('get-dreps-voting-power-list.sql');
            const result = await this.dbService.query<DRepVotingPowerList>(sql);
            rows.push(...result.rows);
        } else {
            const sql = this.sqlService.load('get-filtered-dreps-voting-power.sql');

            for (const identifier of identifiers) {
                const result = await this.dbService.query<DRepVotingPowerList>(sql, [
                    identifier,
                    identifier,
                ]);
                rows.push(...result.rows);
            }
        }
        return rows.map((row)=> ({
            view: row.view,
            hashRaw: row.hash_raw,
            votingPower: this.toInteger(row.voting_power),
            givenName: row.given_name,
        }));
    }

    async getInfo(drepId: string): Promise<DRepInfoResponse> {
        assertHexText(drepId);
        
        const sql = this.sqlService.load('get-drep-info.sql');
        const result = await this.dbService.query<DRepInfo>(sql, [drepId]);

        if (result.rows.length === 0) {
            return this.emptyDRepInfo();
        }
        if (result.rows.length != 1) {
            throw new InternalServerErrorException({
                errorType: 'CriticalError',
                message: 'Unexpected result from database query in getDRepInfo',
            });
        }
        const row = result.rows[0];
        return {
            isScriptBased: row.is_script_based,
            isRegisteredAsDRep: row.is_registered_as_drep ?? false,
            wasRegisteredAsDRep: row.was_registered_as_drep ?? false,
            isRegisteredAsSoleVoter: row.is_registered_as_sole_voter ?? false,
            wasRegisteredAsSoleVoter: row.was_registered_as_sole_voter ?? false,
            deposit: this.toNullableInteger(row.deposit),
            url: row.url,
            dataHash: row.data_hash,
            votingPower: this.toNullableInteger(row.voting_power),
            dRepRegisterTxHash: row.drep_register_tx_hash,
            dRepRetireTxHash: row.drep_retire_tx_hash,
            soleVoterRegisterTxHash: row.sole_voter_register_tx_hash,
            soleVoterRetireTxHash: row.sole_voter_retire_tx_hash,
            paymentAddress: row.payment_address,
            givenName: row.given_name,
            objectives: row.objectives,
            motivations: row.motivations,
            qualifications: row.qualifications,
            imageUrl: row.image_url,
            imageHash: row.image_hash,
        };
    }

    async list(params: {
        search?: string;
        status: DRepStatus[];
        sort?: DRepListSort;
        page: number;
        pageSize:number;
        seed?: string;
    }): Promise<DRepListResponse> {
        const search = params.search ?? '';
        const sql = this.sqlService.load('list-dreps.sql');

        const result = await this.dbService.query<DRepList>(sql, [
            search,
            search,
            search,
            search,
            search,
            search,
            `%${search}%`,
        ]);

        let dreps = result.rows.map((row)=> this.toDRepListItem(row));

        if(!params.search) {
            dreps = dreps.filter((drep)=> drep.type !== 'SoleVoter');
        } else {
            const searchLower = params.search.toLowerCase();
            dreps = dreps.filter((drep) => {
                if(drep.type === 'DRep'){
                    return true;
                }

                return (
                    drep.view.toLowerCase() === searchLower ||
                    drep.drepId.toLowerCase() === searchLower
                );
            });
        }
        if (params.status.length > 0) {
            dreps = dreps.filter((drep) => params.status.includes(drep.status));
        }
        dreps = this.sortDReps(dreps, params.sort, params.seed);

        const total = dreps.length;
        const start = params.page * params.pageSize;
        const elements = dreps.slice(start,start+params.pageSize);

        return {
            page: params.page,
            pageSize: params.pageSize,
            total,
            elements
        };
    }
    async getVotes(
        drepId: string,
        selectedTypes: GovernanceActionType[] = [],
        sort?: GovernanceActionSortMode,
        search?: string,
        ): Promise<VoteResponse[]> {
        assertHexText(drepId);

        const sql = this.sqlService.load('get-votes.sql');
        const result = await this.dbService.query<DRepVoteRow>(sql, [drepId]);

        if (result.rows.length === 0) {
            return [];
        }

        const voteRows = result.rows;
        const proposals: ProposalResponse[] = [];

        for (const voteRow of voteRows) {
            const matched = await this.proposalService.getProposals(voteRow.gov_action_id);
            proposals.push(...matched);
        }

        let processedProposals = this.proposalService.filterByType(
            proposals,
            selectedTypes,
        );
        processedProposals = this.proposalService.filterBySearch(
            processedProposals,
            search,
        );
        processedProposals = this.proposalService.sortProposals(
            processedProposals,
            sort,
        );

        const voteByGovActionId = new Map<string, DRepVoteRow>();

        for (const voteRow of voteRows) {
            voteByGovActionId.set(voteRow.gov_action_id, voteRow);
        }

        return processedProposals.flatMap((proposal) => {
            const govActionId = `${proposal.txHash}#${proposal.index}`;
            const voteRow = voteByGovActionId.get(govActionId);

            if (!voteRow) {
            return [];
            }

            return [
            {
                vote: this.toVoteParams(voteRow),
                proposal,
            },
            ];
        });
     }



    private emptyDRepInfo(): DRepInfoResponse {
        return {
        isScriptBased: false,
        isRegisteredAsDRep: false,
        wasRegisteredAsDRep: false,
        isRegisteredAsSoleVoter: false,
        wasRegisteredAsSoleVoter: false,
        deposit: null,
        url: null,
        dataHash: null,
        votingPower: null,
        dRepRegisterTxHash: null,
        dRepRetireTxHash: null,
        soleVoterRegisterTxHash: null,
        soleVoterRetireTxHash: null,
        paymentAddress: null,
        givenName: null,
        objectives: null,
        motivations: null,
        qualifications: null,
        imageUrl: null,
        imageHash: null,
        };
    }

    private toDRepListItem(row: DRepList): DRepListItem {
        const deposit = this.toInteger(row.deposit);
        const latestDeposit = this.toInteger(row.latest_deposit);

        return {
            isScriptBased: row.has_script,
            drepId: row.drep_hash,
            view: row.view,
            url: row.url,
            metadataHash: row.metadata_hash,
            deposit,
            votingPower: this.toNullableInteger(row.amount),
            status: this.toDRepStatus(row.active, deposit),
            type: this.toDRepType(latestDeposit, row.url, row.has_non_deregister_voting_anchor),
            latestTxHash: row.tx_hash,
            latestRegistrationDate: this.toIsoString(row.last_register_time),
            metadataError: row.fetch_error,
            paymentAddress: row.payment_address,
            givenName: row.given_name,
            objectives: row.objectives,
            motivations: row.motivations,
            qualifications: row.qualifications,
            imageUrl: row.image_url,
            imageHash: row.image_hash,
            votesLastYear: this.toNullableInteger(row.votes_last_year),
            identityReferences: row.identity_references,
            linkReferences: row.link_references,
        };
    }

    private toDRepStatus(active: boolean, deposit: number): DRepStatus {
        if (deposit < 0) {
            return 'Retired';
        }

        return active ? 'Active' : 'Inactive';
    }

    private toDRepType(
        latestDeposit: number,
        url: string | null,
        hasNonDeregisterVotingAnchor: boolean | null,
        ): DRepType {
        if (latestDeposit >= 0 && url === null) {
            return 'SoleVoter';
        }

        if (latestDeposit >= 0 && url !== null) {
            return 'DRep';
        }

        if (latestDeposit < 0 && !hasNonDeregisterVotingAnchor) {
            return 'SoleVoter';
        }

        return 'DRep';
    }

    private sortDReps(
        dreps: DRepListItem[],
        sort?: DRepListSort,
        seed?: string,
        ): DRepListItem[] {
        const copied = [...dreps];

        switch (sort) {
            case 'VotingPower':
            return copied.sort(
                (a, b) => (b.votingPower ?? -1) - (a.votingPower ?? -1),
            );

            case 'Activity':
            return copied.sort(
                (a, b) => (b.votesLastYear ?? -1) - (a.votesLastYear ?? -1),
            );

            case 'RegistrationDate':
            return copied.sort(
                (a, b) =>
                Date.parse(b.latestRegistrationDate) -
                Date.parse(a.latestRegistrationDate),
            );

            case 'Status':
            return copied.sort(
                (a, b) => this.statusOrder(a.status) - this.statusOrder(b.status),
            );

            case 'Random':
            return copied.sort(
                (a, b) =>
                this.seededHash(`${seed ?? ''}:${a.drepId}`) -
                this.seededHash(`${seed ?? ''}:${b.drepId}`),
            );

            default:
            return copied;
        }
    }

    private statusOrder(status: DRepStatus): number {
        return {
            Active: 0,
            Inactive: 1,
            Retired: 2,
        }[status];
    }

    private seededHash(value: string): number {
        let hash = 0;

        for (let index = 0; index < value.length; index += 1) {
            hash = (hash * 31 + value.charCodeAt(index)) | 0;
        }

        return hash;
    }

    private toIsoString(value: Date | string): string {
        if (value instanceof Date) {
            return value.toISOString();
        }

        return new Date(value).toISOString();
    }



    private toInteger(value: number | string): number {
        return Math.floor(Number(value));
    }

    private toNullableInteger(value: number | string | null): number | null {
        if (value === null) {
            return null;
        }
        return this.toInteger(value);
    }
    
    private toVoteParams(row: DRepVoteRow): VoteParams {
        return {
            proposalId: String(row.proposal_id),
            drepId: row.drep_id,
            vote: row.vote,
            url: row.url,
            metadataHash: row.doc_hash,
            epochNo: this.toInteger(row.epoch_no),
            date: this.toIsoString(row.date),
            txHash: row.vote_tx_hash,
        };
}

    
}