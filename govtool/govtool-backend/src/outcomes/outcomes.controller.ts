import {
  BadRequestException,
  Body,
  Controller,
  Get,
  HttpCode,
  Param,
  Post,
  Query,
} from '@nestjs/common';

import { MAX_PAGE_SIZE, parsePageValue } from 'src/common/pagination';
import { queryEnum } from 'src/common/query-enum';
import type { LegacyEpochParams } from 'src/epoch/epoch.type';
import { governanceActionTypes } from 'src/proposal/proposal.type';
import { OutcomesService } from './outcomes.service';
import {
  outcomeSortOptions,
  outcomeStatusFilters,
  type OutcomeDetailRow,
  type OutcomeListRow,
  type OutcomeMetadataResponse,
  type OutcomeNetworkMetrics,
  type SignatureVerificationResult,
} from './outcomes.type';
import type { AuthorWitnessInput } from './signature';

const FILTER_WORDS: readonly string[] = [
  ...outcomeStatusFilters,
  ...governanceActionTypes,
];

function badRequest(message: string): BadRequestException {
  return new BadRequestException({ errorType: 'ValidationError', message });
}

function parseEpoch(value: string | undefined): number | undefined {
  if (value === undefined || value === '') return undefined;
  if (!/^\d{1,9}$/.test(value)) {
    throw badRequest('epoch must be a non-negative integer');
  }
  return Number(value);
}

function requiredString(value: unknown, name: string): string {
  if (typeof value !== 'string' || value.trim() === '') {
    throw badRequest(`${name} is required`);
  }
  if (value.length > 2048) throw badRequest(`${name} is too long`);
  return value;
}

/**
 * The routes the governance outcomes UI calls, under the base url the
 * frontend's VITE_OUTCOMES_API_URL names: point it at `<backend>/outcomes`.
 * Query names, defaults and bodies are the ones that UI sends and reads.
 */
@Controller('outcomes/governance-actions')
export class OutcomesGovernanceActionsController {
  constructor(private readonly outcomes: OutcomesService) {}

  @Get()
  list(
    @Query('search') search?: string,
    @Query('filters') filters?: string,
    @Query('sort') sort?: string,
    @Query('page') page?: string,
    @Query('limit') limit?: string,
  ): Promise<OutcomeListRow[]> {
    const filterList =
      typeof filters === 'string' && filters !== ''
        ? filters
            .split(',')
            .map((f) => f.trim())
            .filter((f) => f !== '')
        : [];
    for (const f of filterList) {
      if (!FILTER_WORDS.includes(f)) {
        throw badRequest(`filters must be among: ${FILTER_WORDS.join(', ')}`);
      }
    }
    const pageNo = parsePageValue(page, 1, 'page');
    return this.outcomes.list({
      search: typeof search === 'string' ? search.trim().slice(0, 256) : '',
      filters: [...new Set(filterList)].sort(),
      sort:
        queryEnum(sort === '' ? undefined : sort, outcomeSortOptions, 'sort') ??
        'newestFirst',
      page: pageNo < 1 ? 1 : pageNo,
      limit: Math.max(1, parsePageValue(limit, 12, 'limit', MAX_PAGE_SIZE)),
    });
  }

  @Get('metadata')
  metadata(
    @Query('url') url?: string,
    @Query('hash') hash?: string,
  ): Promise<OutcomeMetadataResponse> {
    return this.outcomes.getMetadata(
      requiredString(url, 'url'),
      requiredString(hash, 'hash'),
    );
  }

  @Get('proposal/:hash')
  proposal(@Param('hash') hash: string): Promise<{ data: unknown }> {
    return this.outcomes.getProposal(hash);
  }

  @Get(':id')
  get(
    @Param('id') id: string,
    @Query('index') index?: string,
  ): Promise<OutcomeDetailRow> {
    return this.outcomes.get(id, index === '' ? undefined : index);
  }
}

@Controller('outcomes/misc')
export class OutcomesMiscController {
  constructor(private readonly outcomes: OutcomesService) {}

  @Get('network/metrics')
  networkMetrics(
    @Query('epoch') epoch?: string,
  ): Promise<OutcomeNetworkMetrics> {
    return this.outcomes.getNetworkMetrics(parseEpoch(epoch));
  }

  @Get('epoch/params')
  epochParams(@Query('epoch') epoch?: string): Promise<LegacyEpochParams> {
    return this.outcomes.getEpochParams(parseEpoch(epoch));
  }

  @Post('verify-signature')
  @HttpCode(201)
  verifySignature(
    @Body() body: AuthorWitnessInput & { metadataUrl?: unknown },
  ): Promise<SignatureVerificationResult> {
    return this.outcomes.verifySignature(body);
  }
}
