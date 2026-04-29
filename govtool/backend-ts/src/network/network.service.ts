import { Injectable, InternalServerErrorException } from "@nestjs/common";

import { DbService } from "src/db/db.service";
import { GetNetworkInfoResponse, NetworkInfo } from "./network.type";

const GET_NETWORK_INFO_SQL = `
SELECT
  (SELECT MAX(no) FROM epoch) AS current_epoch,
  (SELECT MAX(block_no) FROM block) AS current_block,
  network_name
FROM
  meta;
`;

@Injectable()
export class NetworkService {
    constructor( private readonly dbService: DbService) {}

    async getNetworkInfo() : Promise<GetNetworkInfoResponse> {
        const result = await this.dbService.query<NetworkInfo>(GET_NETWORK_INFO_SQL);

        if (result.rows.length != 1){
            throw this.networkInfoError();
        }
        const row = result.rows[0];
        if (
            row.current_epoch === null ||
            row.current_block === null ||
            row.network_name ===null
        ) {
            throw this.networkInfoError();
        }
        return {
             currentTime: new Date().toISOString(),
             epochNo: this.toInteger(row.current_epoch),
             blockNo: this.toInteger(row.current_block),
             networkName: row.network_name,
        };
    }


private toInteger(value: number | string ) : number {
    const parsed = Number(value);

    if (!Number.isInteger(parsed)) {
        throw this.networkInfoError();
    }
    return parsed;
}    

private networkInfoError(): InternalServerErrorException {
    return new InternalServerErrorException ({
        errorType: 'CriticalError',
        message: "Could not query the network info"
    });
}
}