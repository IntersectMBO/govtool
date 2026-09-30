// SPEC §11.5 demo data for the Playwright specs that expect existing rows.
// Never run automatically. Idempotent through a marker row: the first demo
// user. The rows mirror what the API's own create paths write (proposal +
// active content, BD with master_id = own id and an active poll).

import { Prisma, PrismaClient } from '@prisma/client';
import { BD_TYPES, GOVERNANCE_ACTION_TYPES } from './lookups.data';

/** Reward addresses nobody holds a key for (testnet header e0). */
export const DEMO_USERS = [
  { username: `e0${'d1'.repeat(28)}`, govtoolUsername: 'demo_alice' },
  { username: `e0${'d2'.repeat(28)}`, govtoolUsername: 'demo_bob' },
] as const;

/** Tx hash of the proposal submitted as a governance action. */
export const DEMO_SUBMITTED_TX_HASH = 'de'.repeat(32);

type Tx = Prisma.TransactionClient;

async function comments(
  tx: Tx,
  target: { proposalId: number } | { bdMasterId: number },
  authorIds: number[],
  label: string,
): Promise<number> {
  const top = await tx.comment.create({
    data: { ...target, userId: authorIds[0], text: `Demo comment on ${label}` },
  });
  await tx.comment.create({
    data: { ...target, userId: authorIds[1], parentId: top.id, text: `Demo reply on ${label}` },
  });
  await tx.comment.create({
    data: { ...target, userId: authorIds[1], text: `Second demo comment on ${label}` },
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
  const n = await comments(tx, { proposalId: p.id }, authorIds, label);
  await tx.proposal.update({ where: { id: p.id }, data: { commentsNumber: n } });
}

async function bd(
  tx: Tx,
  creatorId: number,
  typeId: number,
  typeName: string,
  authorIds: number[],
  submittedForVote: Date | null,
) {
  const label = submittedForVote ? `Demo submitted BD (${typeName})` : `Demo BD (${typeName})`;
  const [costing, detail, psapb, ownership, further] = await Promise.all([
    tx.bdCosting.create({
      data: {
        costBreakdown: `Cost breakdown of ${label}.`,
        preferredCurrencyId: 1,
        adaAmount: '100000',
        amountInPreferredCurrency: '50000',
        usdToAdaConversionRate: '0.5',
        adaAmountClone: 100000,
        amountInPreferredCurrencyClone: 50000,
        usdToAdaConversionRateClone: 0.5,
      },
    }),
    tx.bdProposalDetail.create({
      data: {
        proposalName: label,
        proposalDescription: `Description of ${label}.`,
        keyDependencies: 'None',
        maintainAndSupport: 'The proposer',
        keyProposalDeliverables: 'A deliverable',
        resourcingDurationEstimates: 'Three months',
        experience: 'Some',
        contractTypeId: 1,
      },
    }),
    tx.bdPsapb.create({
      data: {
        problemStatement: `Problem of ${label}.`,
        proposalBenefit: 'A benefit',
        supplementaryEndorsement: '',
        explainProposalRoadmap: '',
        typeId,
        roadmapId: 10,
        committeeId: 1,
      },
    }),
    tx.bdProposalOwnership.create({
      data: {
        agreed: true,
        submitedOnBehalf: 'Individual',
        proposalPublicChampion: 'demo_alice',
        socialHandles: '@demo',
        beCountryId: 1,
      },
    }),
    tx.bdFurtherInformation.create({
      data: { links: { create: [{ position: 0, link: 'https://example.com/bd', text: 'BD link' }] } },
    }),
  ]);
  const row = await tx.bd.create({
    data: {
      creatorId,
      isActive: true,
      privacyPolicy: true,
      intersectNamedAdministrator: false,
      submittedForVote,
      costingId: costing.id,
      proposalDetailId: detail.id,
      psapbId: psapb.id,
      proposalOwnershipId: ownership.id,
      furtherInformationId: further.id,
    },
  });
  await tx.bd.update({ where: { id: row.id }, data: { masterId: row.id } });
  await tx.bdPoll.create({ data: { bdMasterId: row.id, isActive: true } });
  const n = await comments(tx, { bdMasterId: row.id }, authorIds, label);
  await tx.bd.update({ where: { id: row.id }, data: { commentsNumber: n } });
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
      // The submitted BD comes first, so it is the oldest: specs open the
      // newest BD and need its editing and poll voting enabled.
      await bd(tx, ids[1], 1, 'Core', ids, new Date());
      for (const [typeId, typeName] of BD_TYPES) {
        await bd(tx, ids[0], typeId, typeName, ids, null);
      }
    },
    { timeout: 60000 },
  );
  return true;
}
