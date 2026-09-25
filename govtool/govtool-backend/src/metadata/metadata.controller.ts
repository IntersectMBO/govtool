import {
  Body,
  Controller,
  Get,
  HttpCode,
  Param,
  Post,
  Query,
  Res,
  UsePipes,
  ValidationPipe,
} from '@nestjs/common';
import type { Response } from 'express';
import type {
  MetadataRefreshOutcome,
  MetadataReport,
  MetadataReportSummary,
  MetadataResult,
} from '@govtool/data-providers/metadata';

import {
  MetadataAnchorDto,
  MetadataReportIdDto,
} from './dto/metadata-anchor.dto';
import { ValidateMetadataDto } from './dto/validate-metadata.dto';
import { MetadataGatewayService } from './metadata-gateway.service';
import { MetadataService } from './metadata.service';
import { ValidateMetadataResult } from './metadata.type';

/**
 * Unknown query or body fields are rejected rather than ignored, so a caller
 * cannot smuggle a cache parameter through to the service (D119).
 */
const strict = new ValidationPipe({
  whitelist: true,
  forbidNonWhitelisted: true,
  transform: true,
});

@Controller('metadata')
@UsePipes(strict)
export class MetadataController {
  constructor(
    private readonly metadataService: MetadataService,
    private readonly gateway: MetadataGatewayService,
  ) {}

  @Post('validate')
  validateMetadata(
    @Body() body: ValidateMetadataDto,
  ): Promise<ValidateMetadataResult> {
    return this.metadataService.validateMetadata(body);
  }

  /** Always 200 with a `MetadataResult`: a failure is a value, not an error. */
  @Get('resolve')
  resolve(@Query() query: MetadataAnchorDto): Promise<MetadataResult> {
    return this.gateway.resolve(query.hash, query.url);
  }

  /**
   * Always 200 with a `MetadataRefreshOutcome`. Inside the per-anchor window
   * the outcome carries `retryAfterSeconds`, mirrored in `Retry-After`.
   */
  @Post('retry')
  @HttpCode(200)
  async retry(
    @Body() body: MetadataAnchorDto,
    @Res({ passthrough: true }) res: Response,
  ): Promise<MetadataRefreshOutcome> {
    const outcome = await this.gateway.retry(body.hash, body.url);
    if (outcome.retryAfterSeconds !== undefined) {
      res.setHeader('Retry-After', String(outcome.retryAfterSeconds));
    }
    return outcome;
  }

  @Get('reports')
  listReports(
    @Query() query: MetadataAnchorDto,
  ): Promise<MetadataReportSummary[]> {
    return this.gateway.listReports(query.hash, query.url);
  }

  @Get('reports/:id')
  getReport(@Param() params: MetadataReportIdDto): Promise<MetadataReport> {
    return this.gateway.getReport(params.id);
  }
}
