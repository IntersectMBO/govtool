// SPEC §11.5 demo data for the Playwright specs that expect existing rows.
// Never run automatically. Idempotent through a marker row: the first demo
// user. The rows mirror what the API's own create paths write (proposal +
// active content).

import { Prisma, PrismaClient } from '@prisma/client';
import { GOVERNANCE_ACTION_TYPES } from './lookups.data';

/** Reward addresses nobody holds a key for (testnet header e0). */
export const DEMO_USERS = [
  { username: `e0${'d1'.repeat(28)}`, govtoolUsername: 'demo_alice' },
  { username: `e0${'d2'.repeat(28)}`, govtoolUsername: 'demo_bob' },
] as const;

/** Tx hash of the proposal submitted as a governance action. */
export const DEMO_SUBMITTED_TX_HASH = 'de'.repeat(32);

type Tx = Prisma.TransactionClient;

async function comments(tx: Tx, proposalId: number, authorIds: number[], label: string): Promise<number> {
  const top = await tx.comment.create({
    data: { proposalId, userId: authorIds[0], text: `Demo comment on ${label}` },
  });
  await tx.comment.create({
    data: { proposalId, userId: authorIds[1], parentId: top.id, text: `Demo reply on ${label}` },
  });
  await tx.comment.create({
    data: { proposalId, userId: authorIds[1], text: `Second demo comment on ${label}` },
  });
  return 3;
}

async function proposal(
  tx: Tx,
  ownerId: number,
  typeId: number,
  typeName: string,
  authorIds: number[],
  submitted: boolean,
) {
  const p = await tx.proposal.create({ data: { userId: ownerId } });
  const hardFork =
    typeId === 6
      ? await tx.proposalHardForkContent.create({
          data: { previousGaHash: null, previousGaId: null, major: '11', minor: '0' },
        })
      : null;
  const label = submitted ? `Demo submitted ${typeName}` : `Demo ${typeName}`;
  const content = await tx.proposalContent.create({
    data: {
      proposalId: p.id,
      userId: ownerId,
      govActionTypeId: typeId,
      name: label.slice(0, 80),
      abstract: `Abstract of ${label}.`,
      motivation: `Motivation of ${label}.`,
      rationale: `Rationale of ${label}.`,
      revActive: true,
      isDraft: false,
      submitted,
      submissionTxHash: submitted ? DEMO_SUBMITTED_TX_HASH : null,
      submissionDate: submitted ? new Date(Date.UTC(2026, 0, 15)) : null,
      hardForkContentId: hardFork?.id ?? null,
      links: { create: [{ position: 0, link: 'https://example.com/demo', text: 'Demo link' }] },
      withdrawals:
        typeId === 2
          ? {
              create: [
                {
                  position: 0,
                  receivingAddress: 'stake_test1urfa857n60fa857n60fa857n60fa857n60fa857n60fa85cyqv8zy',
                  amount: 1000,
                },
              ],
            }
          : undefined,
    },
  });
  if (typeId === 3) {
    await tx.proposalConstitutionContent.create({
      data: {
        contentId: content.id,
        constitutionUrl: 'https://example.com/constitution.txt',
        haveGuardrailsScript: false,
      },
    });
  }
  const n = await comments(tx, p.id, authorIds, label);
  await tx.proposal.update({ where: { id: p.id }, data: { commentsNumber: n } });
}

/**
 * Returns false when the marker row (the first demo user) already exists.
 * One transaction, so a failed run leaves nothing behind.
 */
export async function seedDemo(prisma: PrismaClient): Promise<boolean> {
  const marker = await prisma.user.findUnique({ where: { username: DEMO_USERS[0].username } });
  if (marker) return false;
  await prisma.$transaction(
    async (tx) => {
      const users: Array<{ id: number }> = [];
      for (const u of DEMO_USERS) users.push(await tx.user.create({ data: { ...u } }));
      const ids = users.map((u) => u.id);
      for (const [typeId, typeName] of GOVERNANCE_ACTION_TYPES) {
        await proposal(tx, ids[0], typeId, typeName, ids, false);
      }
      await proposal(tx, ids[1], 1, 'Info Action', ids, true);
    },
    { timeout: 60000 },
  );
  return true;
}
