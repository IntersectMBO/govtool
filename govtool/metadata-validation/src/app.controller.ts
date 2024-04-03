import { Controller, Body, Post } from '@nestjs/common';
import { AppService } from './app.service';
import { ValidateMetadataDTO } from './dto/validateMetadata.dto';
import { ValidateMetadataResult } from './types/validateMetadata';

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
