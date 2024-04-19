import { Controller, Body, Post } from '@nestjs/common';

import { ValidateMetadataDTO } from '@dto';
import { ValidateMetadataResult } from '@types';

import { AppService } from './app.service';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Post('validate')
  validateMetadata(
    @Body() validateMetadataDto: ValidateMetadataDTO,
  ): Promise<ValidateMetadataResult> {
    return this.appService.validateMetadata(validateMetadataDto);
  }
}
