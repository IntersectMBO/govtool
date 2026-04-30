import { Controller , Get } from "@nestjs/common";

import { EpochService } from "./epoch.service";

@Controller('epoch')
export class EpochController {
    constructor (private readonly epochService: EpochService) {}

    @Get('params')
    getCurrentEpochParams(): Promise<unknown | null> {
        return this.epochService.getCurrentEpochParams();
    }
}