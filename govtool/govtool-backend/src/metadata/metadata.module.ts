import { Module } from '@nestjs/common';

import { ConfigService } from '../config/config.service';

import { MetadataGatewayService } from './metadata-gateway.service';
import { MetadataController } from './metadata.controller';
import { MetadataService } from './metadata.service';

@Module({
  controllers: [MetadataController],
  providers: [MetadataService, MetadataGatewayService, ConfigService],
})
export class MetadataModule {}
