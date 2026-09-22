import {
  Body,
  Controller,
  Post,
  Query,
  Req,
  UnsupportedMediaTypeException,
} from '@nestjs/common';

import type { Request } from 'express';
import { IpfsService } from './ipfs.service';
import { UploadResponse } from './ipfs.type';

@Controller('ipfs')
export class IpfsController {
  constructor(private readonly ipfsService: IpfsService) {}

  @Post('upload')
  upload(
    @Query('fileName') fileName: string | undefined,
    @Body() fileContent: unknown,
    @Req() request: Request,
  ): Promise<UploadResponse> {
    if (!request.is('text/plain') || typeof fileContent !== 'string') {
      throw new UnsupportedMediaTypeException('Expected a text/plain body');
    }
    return this.ipfsService.upload(fileName ?? 'data.txt', fileContent);
  }
}
