// Budget discussion writes (SPEC §8.8): create, new version, delete chain.

import { Injectable } from '@nestjs/common';
import { Prisma } from '@prisma/client';
import type { AuthUser } from '../auth/auth-user';
import { forbidden, notFound, validationError } from '../common/errors';
import { PrismaService } from '../prisma/prisma.service';
import { BdInput, LookupRef } from './bd-input';

type Tx = Prisma.TransactionClient;

export const UPDATE_LOCKED =
  'Update is not allowed because this entry has already been submitted for voting.';
export const DELETE_LOCKED =
  'Deletion is not allowed because this entry has already been submitted for voting.';

/** Everything the raw create response and a populated read need. */
export const BD_FULL_INCLUDE = {
  creator: true,
  costing: true,
  proposalDetail: true,
  psapb: true,
  proposalOwnership: true,
  furtherInformation: { include: { links: { orderBy: [{ position: 'asc' }, { id: 'asc' }] } } },
} satisfies Prisma.BdInclude;

/** Lock the chain's master row; null when `masterId` is not a master id. */
export async function lockChain(tx: Tx, masterId: number): Promise<boolean> {
  const rows = await tx.$queryRaw<Array<{ id: number }>>`
    SELECT id FROM bds WHERE id = ${masterId} AND master_id = ${masterId} FOR UPDATE`;
  return rows.length > 0;
}

/** The submission lock: any version of the chain has `submitted_for_vote`. */
export async function chainSubmitted(tx: Tx, masterId: number): Promise<boolean> {
  const n = await tx.bd.count({ where: { masterId, submittedForVote: { not: null } } });
  return n > 0;
}

@Injectable()
export class BdsService {
  constructor(private readonly prisma: PrismaService) {}

  /** POST /api/bds. Returns the new row with BD_FULL_INCLUDE. */
  async create(input: BdInput, caller: AuthUser) {
    return this.prisma.$transaction(async (tx) => {
      let commentsNumber = 0;
      let masterId: number | null = null;
      if (input.masterId !== null) {
        // Serialise concurrent versions on the master row, then lock the live
        // row so a concurrent comment increment lands before the copy.
        if (!(await lockChain(tx, input.masterId))) throw notFound();
        const active = await tx.$queryRaw<Array<{ id: number; creator_id: number; comments_number: number }>>`
          SELECT id, creator_id, comments_number FROM bds
          WHERE master_id = ${input.masterId} AND is_active FOR UPDATE`;
        if (active.length === 0) throw notFound();
        if (active[0].creator_id !== caller.id) throw forbidden('Unauthorized');
        if (await chainSubmitted(tx, input.masterId)) throw validationError(UPDATE_LOCKED);
        await this.checkLookups(tx, input.lookups);
        await tx.bd.update({ where: { id: active[0].id }, data: { isActive: false } });
        commentsNumber = active[0].comments_number;
        masterId = input.masterId;
      } else {
        await this.checkLookups(tx, input.lookups);
      }

      const sections = await this.createSections(tx, input);
      const row = await tx.bd.create({
        data: {
          creatorId: caller.id,
          masterId,
          isActive: true,
          privacyPolicy: true,
          intersectNamedAdministrator: input.intersectNamedAdministrator,
          intersectAdminFurtherText: input.intersectAdminFurtherText,
          commentsNumber,
          submittedForVote: null,
          ...sections,
        },
      });
      if (masterId === null) {
        await tx.bd.update({ where: { id: row.id }, data: { masterId: row.id } });
        await tx.bdPoll.create({ data: { bdMasterId: row.id, isActive: true } });
      }
      return tx.bd.findUniqueOrThrow({ where: { id: row.id }, include: BD_FULL_INCLUDE });
    });
  }

  /** DELETE /api/bds/:id (row id): removes the whole chain (Δ40). */
  async deleteChain(rowId: number, caller: AuthUser) {
    return this.prisma.$transaction(async (tx) => {
      const row = await tx.bd.findUnique({ where: { id: rowId } });
      if (!row) throw notFound();
      if (row.creatorId !== caller.id) throw forbidden("You can't delete this proposal.");
      const masterId = row.masterId ?? row.id;
      await lockChain(tx, masterId);
      if (await chainSubmitted(tx, masterId)) throw validationError(DELETE_LOCKED);
      const versions = await tx.bd.findMany({ where: { OR: [{ masterId }, { id: masterId }] } });
      const ids = (pick: (v: (typeof versions)[number]) => number | null) =>
        versions.map(pick).filter((x): x is number => x !== null);
      // Comments (and their reports), the poll (and its votes) and the other
      // versions hang off the master row and cascade with it.
      await tx.comment.deleteMany({ where: { bdMasterId: masterId } });
      await tx.bdPoll.deleteMany({ where: { bdMasterId: masterId } });
      await tx.bd.deleteMany({ where: { id: { in: versions.map((v) => v.id) } } });
      // Sections are SetNull on the BD side: delete them explicitly.
      await tx.bdCosting.deleteMany({ where: { id: { in: ids((v) => v.costingId) } } });
      await tx.bdProposalDetail.deleteMany({ where: { id: { in: ids((v) => v.proposalDetailId) } } });
      await tx.bdPsapb.deleteMany({ where: { id: { in: ids((v) => v.psapbId) } } });
      await tx.bdProposalOwnership.deleteMany({ where: { id: { in: ids((v) => v.proposalOwnershipId) } } });
      await tx.bdFurtherInformation.deleteMany({ where: { id: { in: ids((v) => v.furtherInformationId) } } });
      await tx.bdContactInformation.deleteMany({ where: { id: { in: ids((v) => v.contactInformationId) } } });
      return row;
    });
  }

  private async checkLookups(tx: Tx, refs: LookupRef[]): Promise<void> {
    for (const ref of refs) {
      const delegate = tx[ref.model] as unknown as {
        findUnique(args: { where: { id: number }; select: { id: true } }): Promise<{ id: number } | null>;
      };
      const found = await delegate.findUnique({ where: { id: ref.id }, select: { id: true } });
      if (!found) throw validationError(`${ref.field} is invalid`);
    }
  }

  private async createSections(tx: Tx, input: BdInput) {
    const ownership = await tx.bdProposalOwnership.create({ data: input.ownership });
    const psapb = await tx.bdPsapb.create({ data: input.psapb });
    const detail = await tx.bdProposalDetail.create({ data: input.proposalDetail });
    const costing = await tx.bdCosting.create({ data: input.costing });
    const further = await tx.bdFurtherInformation.create({
      data: { links: { create: input.links } },
    });
    const contact = input.contact ? await tx.bdContactInformation.create({ data: input.contact }) : null;
    return {
      proposalOwnershipId: ownership.id,
      psapbId: psapb.id,
      proposalDetailId: detail.id,
      costingId: costing.id,
      furtherInformationId: further.id,
      contactInformationId: contact?.id ?? null,
    };
  }
}
