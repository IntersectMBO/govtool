import { Controller, Get, Param, Res } from '@nestjs/common';
import type { Response } from 'express';

import { SurveyService } from './survey.service';
import { SurveyDefinitionResponse } from './survey.type';

@Controller('survey')
export class SurveyController {
  constructor(private readonly surveyService: SurveyService) {}

  @Get('definition/:txId/:index')
  async getDefinition(
    @Param('txId') txId: string,
    @Param('index') index: string,
    @Res({ passthrough: true }) response: Response,
  ): Promise<SurveyDefinitionResponse> {
    const result = await this.surveyService.getDefinition(txId, index);

    response.setHeader('Cache-Control', 'public, max-age=31536000, immutable');

    return result;
  }
}
