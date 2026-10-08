import type { PrismaClient } from '@prisma/client';
import { GOVERNANCE_ACTION_TYPES, LOOKUP_TABLES } from './lookups.data';

/**
 * Upsert every §6 row on its fixed id, then move each sequence past max(id)
 * so a later insert without an id cannot collide. Idempotent.
 */
export async function seedLookups(prisma: PrismaClient): Promise<void> {
  await prisma.$transaction(async (tx) => {
    for (const [id, name] of GOVERNANCE_ACTION_TYPES) {
      await tx.governanceActionType.upsert({ where: { id }, create: { id, name }, update: { name } });
    }
    for (const table of LOOKUP_TABLES) {
      // Table names come from the constant list above, never from input.
      await tx.$executeRawUnsafe(
        `SELECT setval(pg_get_serial_sequence('"${table}"', 'id'), GREATEST((SELECT MAX(id) FROM "${table}"), 1))`,
      );
    }
  });
}
