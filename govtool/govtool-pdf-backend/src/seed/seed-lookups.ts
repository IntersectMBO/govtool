import type { PrismaClient } from '@prisma/client';
import {
  BD_CONTRACT_TYPES,
  BD_CURRENCIES,
  BD_INTERSECT_COMMITTEES,
  BD_ROAD_MAPS,
  BD_TYPES,
  COUNTRIES,
  GOVERNANCE_ACTION_TYPES,
  LOOKUP_TABLES,
} from './lookups.data';

/**
 * Upsert every §6 row on its fixed id, then move each sequence past max(id)
 * so a later insert without an id cannot collide. Idempotent.
 */
export async function seedLookups(prisma: PrismaClient): Promise<void> {
  await prisma.$transaction(async (tx) => {
    for (const [id, name] of GOVERNANCE_ACTION_TYPES) {
      await tx.governanceActionType.upsert({ where: { id }, create: { id, name }, update: { name } });
    }
    for (const [id, typeName] of BD_TYPES) {
      await tx.bdType.upsert({ where: { id }, create: { id, typeName }, update: { typeName } });
    }
    for (const [id, roadmapName] of BD_ROAD_MAPS) {
      await tx.bdRoadMap.upsert({ where: { id }, create: { id, roadmapName }, update: { roadmapName } });
    }
    for (const [id, committeeName] of BD_INTERSECT_COMMITTEES) {
      await tx.bdIntersectCommittee.upsert({
        where: { id },
        create: { id, committeeName },
        update: { committeeName },
      });
    }
    for (const [id, contractTypeName] of BD_CONTRACT_TYPES) {
      await tx.bdContractType.upsert({
        where: { id },
        create: { id, contractTypeName },
        update: { contractTypeName },
      });
    }
    for (const [id, currencyName, currencyLetterCode, currencyNumberCode] of BD_CURRENCIES) {
      const data = { currencyName, currencyLetterCode, currencyNumberCode };
      await tx.bdCurrency.upsert({ where: { id }, create: { id, ...data }, update: data });
    }
    for (const [id, countryName, alfa2Code, alfa3Code] of COUNTRIES) {
      const data = { countryName, alfa2Code, alfa3Code };
      await tx.countryList.upsert({ where: { id }, create: { id, ...data }, update: data });
    }
    for (const table of LOOKUP_TABLES) {
      // Table names come from the constant list above, never from input.
      await tx.$executeRawUnsafe(
        `SELECT setval(pg_get_serial_sequence('"${table}"', 'id'), GREATEST((SELECT MAX(id) FROM "${table}"), 1))`,
      );
    }
  });
}
