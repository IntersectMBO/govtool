import { Controller, Get } from '@nestjs/common';

import { EpochService } from './epoch.service';
import type { LegacyEpochParams } from './epoch.type';

@Controller('epoch')
export class EpochController {
  constructor(private readonly epochService: EpochService) {}

  @Get('params')
  getCurrentEpochParams(): Promise<LegacyEpochParams> {
    return this.epochService.getCurrentEpochParams();
  }
}
