import { Controller , Get } from '@nestjs/common';

import { NetworkService } from './network.service';
import { GetNetworkInfoResponse } from './network.type';

@Controller('network')
export class NetworkController {
    constructor(private readonly networkService: NetworkService) {}

    @Get('info')
    getNetworkInfo(): Promise<GetNetworkInfoResponse> {
        return this.networkService.getNetworkInfo();
    }
}
