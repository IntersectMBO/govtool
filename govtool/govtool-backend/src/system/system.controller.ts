import { Controller, Get, HttpException } from '@nestjs/common';
import type {
  FeatureSet,
  ProviderCapabilityDocument,
} from '@govtool/data-providers/chain-data';

import { SystemService } from './system.service';

@Controller()
export class SystemController {
  constructor(private readonly systemService: SystemService) {}

  @Get('throw500')
  throw500(): never {
    throw new HttpException(
      {
        errorType: 'CriticalError',
        message: 'intentional system break for testing purposes',
      },
      500,
    );
  }

  /**
   * The composed capability document — for operators and support bundles.
   * Verbose and provider-shaped on purpose; a component reads `/system/features`
   * instead.
   */
  @Get('system/capabilities')
  getCapabilities(): Promise<ProviderCapabilityDocument> {
    return this.systemService.getCapabilities();
  }

  /** What the browser fetches at boot to decide which controls to render. */
  @Get('system/features')
  getFeatures(): Promise<FeatureSet> {
    return this.systemService.getFeatures();
  }
}
