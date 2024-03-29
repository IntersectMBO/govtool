import { Controller, Get, Body } from '@nestjs/common';
import { AppService } from './app.service';
import { ValidateMetadataDTO } from './dto/validateMetadata.dto';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get('validate-metadata')
  validateMetadata(
    @Body() validateMetadataDto: ValidateMetadataDTO,
  ): ValidateMetadataDTO {
    return this.appService.validateMetadata(validateMetadataDto);
  }
}
