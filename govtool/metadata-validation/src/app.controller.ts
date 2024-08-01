import { Controller, Body, Post } from '@nestjs/common';

import { ValidateMetadataDTO } from '@dto';
import { ValidateMetadataResult } from '@types';

import { AppService } from './app.service';
import { ApiBody, ApiResponse } from '@nestjs/swagger';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Post('validate')
  @ApiBody({
    schema: {
      type: 'object',
      properties: {
        url: { type: 'string' },
        hash: { type: 'string' },
        standard: { type: 'string' },
      },
      required: ['url', 'hash'],
    },
  })
  @ApiResponse({
    status: 200,
    description: 'Validation result',
    schema: {
      type: 'object',
      properties: {
        valid: { type: 'boolean' },
        status: { type: 'string' },
        metadata: { type: 'object' },
      },
    },
  })
  validateMetadata(
    @Body() validateMetadataDto: ValidateMetadataDTO,
  ): Promise<ValidateMetadataResult> {
    return this.appService.validateMetadata(validateMetadataDto);
  }
}
