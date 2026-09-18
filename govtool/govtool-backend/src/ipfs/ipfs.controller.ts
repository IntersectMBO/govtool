import { Body, Controller, Post, Query } from '@nestjs/common';

import { IpfsService } from './ipfs.service';
import { UploadResponse } from './ipfs.type';

@Controller('ipfs')
export class IpfsController {
  constructor(private readonly ipfsService: IpfsService) {}

  @Post('upload')
  upload(
    @Query('fileName') fileName: string | undefined,
    @Body() fileContent: string,
  ): Promise<UploadResponse> {
    return this.ipfsService.upload(fileName ?? 'data.txt', fileContent);
  }
}
