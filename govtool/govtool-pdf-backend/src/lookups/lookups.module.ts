import { Module } from '@nestjs/common';
import { LookupsController } from './lookups.controller';

/** Governance action types and the six BD lookup lists (§8.1, §8.9). */
@Module({ controllers: [LookupsController] })
export class LookupsModule {}
