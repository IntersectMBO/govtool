import {
  Body,
  Controller,
  Post,
  Req,
  UnsupportedMediaTypeException,
} from '@nestjs/common';
import type { Request } from 'express';

import { IpfsService } from './ipfs.service';
import { UploadResponse } from './ipfs.type';

@Controller('ipfs')
export class IpfsController {
  constructor(private readonly ipfsService: IpfsService) {}

  // `fileName` is still accepted in the query and ignored: the contract pins
  // bytes, not a named file.
  @Post('upload')
  upload(
    @Req() request: Request,
    @Body() fileContent: unknown,
  ): Promise<UploadResponse> {
    if (!request.is('text/plain') || typeof fileContent !== 'string') {
      throw new UnsupportedMediaTypeException('Expected a text/plain body');
    }
    return this.ipfsService.upload(
      fileContent,
      request.ip ?? request.socket.remoteAddress ?? 'unknown',
    );
  }
}
