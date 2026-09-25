import { ConfigService } from '../config/config.service';
import { Module } from '@nestjs/common';

import { MetadataController } from './metadata.controller';
import { MetadataService } from './metadata.service';

@Module({
  controllers: [MetadataController],
  providers: [MetadataService, ConfigService],
})
export class MetadataModule {}
