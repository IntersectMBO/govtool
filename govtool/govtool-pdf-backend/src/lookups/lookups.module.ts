import { Module } from '@nestjs/common';
import { LookupsController } from './lookups.controller';

/** Governance action types (§8.1). */
@Module({ controllers: [LookupsController] })
export class LookupsModule {}
