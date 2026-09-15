import { Controller , Get } from '@nestjs/common';

import { NetworkService } from './network.service';
import { GetNetworkInfoResponse, GetNetworkMetricsResponse, GetNetworkTotalStakeResponse } from './network.type';

@Controller('network')
export class NetworkController {
    constructor(private readonly networkService: NetworkService) {}

    @Get('info')
    getNetworkInfo(): Promise<GetNetworkInfoResponse> {
        return this.networkService.getNetworkInfo();
    }
    @Get('total-stake')
    getNetworkTotalStake(): Promise<GetNetworkTotalStakeResponse> {
        return this.networkService.getNetworkTotalStake();
    }

    @Get('metrics')
    getNetworkMetrics(): Promise<GetNetworkMetricsResponse> {
        return this.networkService.getNetworkMetrics();
    }
}
