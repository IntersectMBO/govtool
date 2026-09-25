import { Controller, Get, HttpException } from '@nestjs/common';
import type { ProviderCapabilities } from '@govtool/data-providers/chain-data';

import type { FeatureSet } from './capabilities';
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
   * The configured provider's own declaration, for operators and support
   * bundles. A component reads `/system/features` instead, which is this
   * backend's answer rather than the provider's.
   */
  @Get('system/capabilities')
  getCapabilities(): Promise<ProviderCapabilities> {
    return this.systemService.getCapabilities();
  }

  /** What the browser fetches at boot to decide which controls to render. */
  @Get('system/features')
  getFeatures(): Promise<FeatureSet> {
    return this.systemService.getFeatures();
  }
}
