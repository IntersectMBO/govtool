import { Controller, Get, Param, Res } from '@nestjs/common';
import type { Response } from 'express';

import { SURVEY_CACHE_SECONDS, SurveyService } from './survey.service';
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
    // An error must not be cached by a browser or proxy; a definition only
    // briefly, since a rollback can undo the transaction that published it.
    response.setHeader('Cache-Control', 'no-store');
    const result = await this.surveyService.getDefinition(txId, index);
    response.setHeader(
      'Cache-Control',
      `public, max-age=${SURVEY_CACHE_SECONDS}`,
    );
    return result;
  }
}
