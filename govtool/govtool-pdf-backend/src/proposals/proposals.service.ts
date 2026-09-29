// SPEC §8.2 proposals and §8.3 proposal contents.

import { Inject, Injectable } from '@nestjs/common';
import { Prisma } from '@prisma/client';
import type { AuthUser } from '../auth/auth-user';
import { assertOwner } from '../auth/auth.guard';
import type { DataPayload } from '../common/body';
import { badRequestDetails, forbidden, isUniqueViolation, notFound, validationError } from '../common/errors';
import { parseRouteId, readIntRef } from '../common/fields';
import { APP_CONFIG } from '../config/config.module';
import type { AppConfig } from '../config/config';
import { PrismaService } from '../prisma/prisma.service';
import { andWith, hasTopLevel, mentions, removeTopLevel, renameTopLevel } from '../query/filter-helpers';
import { makeCond, parseQuery } from '../query/parse';
import { toPrismaOrderBy, toPrismaPaging, toPrismaWhere } from '../query/prisma';
import {
  ListEnvelope,
  Entity,
  SingleEnvelope,
  list,
  paginationMeta,
  serializeEntity,
  serializeScalars,
  single,
} from '../query/serialize';
import { AndNode, FilterNode } from '../query/types';
import { PROPOSALS_ALLOWLIST } from './proposal.allowlists';
import { CONTENT_ITEM_INCLUDE, PROPOSAL_ITEM_INCLUDE, serializeProposalItem } from './proposal-item';
import { ContentInput, readContentInput } from './proposal-input';
import { ProposalContentResource, ProposalResource } from './proposal.resources';

type Tx = Prisma.TransactionClient;

const NOT_FOUND = 'Proposal not found';
const DRAFT_ONLY = 'You can not access draft proposal details.';
const ALREADY_SUBMITTED = "Proposal can't be updated, it has been already submited";
const FOREIGN = "You can't access this entry";
const TX_HASH_RE = /^[0-9a-fA-F]{64}$/;

/**
 * The /proposals filter rewrites of §8.2 on the parsed AST: `prop_id` becomes
 * `proposal_id` (else only active revisions), `is_draft` scopes to the caller
 * (else only non-drafts). Exported for unit tests.
 */
export function rewriteProposalFilters(filters: AndNode | null, callerId: number | null): AndNode {
  let f = filters;
  const extra: FilterNode[] = [];
  if (hasTopLevel(f, 'prop_id')) {
    f = renameTopLevel(f, 'prop_id', ['proposal_id']);
  } else {
    extra.push(makeCond(ProposalContentResource, ['prop_rev_active'], '$eq', true));
  }
  // `prop_id` only means something at the top level; anywhere else it is not a field.
  if (mentions(f, 'prop_id')) throw validationError('Invalid key prop_id');
  if (hasTopLevel(f, 'is_draft')) {
    if (callerId === null) throw badRequestDetails('User is required');
    f = removeTopLevel(f, 'user_id');
    extra.push(makeCond(ProposalContentResource, ['user_id'], '$eq', callerId));
  } else {
    extra.push(makeCond(ProposalContentResource, ['is_draft'], '$eq', false));
  }
  return andWith(f, ...extra);
}

@Injectable()
export class ProposalsService {
  constructor(
    private readonly prisma: PrismaService,
    @Inject(APP_CONFIG) private readonly config: AppConfig,
  ) {}

  // ------------------------------------------------------------- reads

  async list(raw: Record<string, unknown>, caller: AuthUser | null): Promise<ListEnvelope<Entity>> {
    const q = parseQuery(raw, PROPOSALS_ALLOWLIST);
    const filters = rewriteProposalFilters(q.filters, caller?.id ?? null);
    const where = toPrismaWhere(ProposalContentResource, filters) as Prisma.ProposalContentWhereInput;
    const { skip, take } = toPrismaPaging(q.pagination);
    const [rows, total] = await Promise.all([
      this.prisma.proposalContent.findMany({
        where,
        orderBy: toPrismaOrderBy(ProposalContentResource, q.sort),
        skip,
        take,
        include: { ...CONTENT_ITEM_INCLUDE, proposal: { include: PROPOSAL_ITEM_INCLUDE } },
      }),
      q.pagination.withCount ? this.prisma.proposalContent.count({ where }) : Promise.resolve(null),
    ]);
    return list(
      rows.map((c) => serializeProposalItem(c.proposal, c)),
      paginationMeta(q.pagination, total),
    );
  }

  async findOne(idRaw: string): Promise<SingleEnvelope<Entity>> {
    let proposalId: number | null = null;
    if (/^\d+$/.test(idRaw)) {
      proposalId = parseRouteId(idRaw);
    } else if (TX_HASH_RE.test(idRaw)) {
      const c = await this.prisma.proposalContent.findUnique({
        where: { submissionTxHash: idRaw.toLowerCase() },
        select: { proposalId: true },
      });
      proposalId = c?.proposalId ?? null;
    }
    if (proposalId === null) throw badRequestDetails(NOT_FOUND);
    const proposal = await this.prisma.proposal.findUnique({
      where: { id: proposalId },
      include: PROPOSAL_ITEM_INCLUDE,
    });
    if (!proposal) throw badRequestDetails(NOT_FOUND);
    const content = await this.prisma.proposalContent.findFirst({
      where: { proposalId, revActive: true, isDraft: false },
      orderBy: [{ createdAt: 'desc' }, { id: 'desc' }],
      include: CONTENT_ITEM_INCLUDE,
    });
    if (!content) {
      const draft = await this.prisma.proposalContent.count({
        where: { proposalId, revActive: true, isDraft: true },
      });
      if (draft > 0) throw badRequestDetails(DRAFT_ONLY);
    }
    return single(serializeProposalItem(proposal, content));
  }

  // ------------------------------------------------------------- writes

  private async readInput(d: DataPayload, rule: 'any-field' | 'previous-ga-id'): Promise<ContentInput> {
    const input = readContentInput(d, { networkId: this.config.cardanoNetworkId, hardForkRule: rule });
    const type = await this.prisma.governanceActionType.findUnique({
      where: { id: input.govActionTypeId },
      select: { id: true },
    });
    if (!type) throw validationError('gov_action_type_id is invalid');
    return input;
  }

  /** Content row, its components and relations, inside `tx`. */
  private async insertContent(tx: Tx, proposalId: number, userId: number, input: ContentInput) {
    const hardFork = input.hardFork
      ? await tx.proposalHardForkContent.create({ data: input.hardFork })
      : null;
    const content = await tx.proposalContent.create({
      data: {
        proposalId,
        userId,
        govActionTypeId: input.govActionTypeId,
        name: input.name,
        abstract: input.abstract,
        motivation: input.motivation,
        rationale: input.rationale,
        isDraft: input.isDraft,
        revActive: true,
        submitted: false,
        isLocked: false,
        submissionTxHash: null,
        submissionDate: null,
        hardForkContentId: hardFork?.id ?? null,
        links: { create: input.links.map((l, position) => ({ position, link: l.link, text: l.text })) },
        withdrawals: {
          create: input.withdrawals.map((w, position) => ({
            position,
            receivingAddress: w.receivingAddress,
            amount: w.amount,
          })),
        },
      },
    });
    if (input.constitution) {
      await tx.proposalConstitutionContent.create({ data: { contentId: content.id, ...input.constitution } });
    }
    return content;
  }

  async create(d: DataPayload, caller: AuthUser) {
    const input = await this.readInput(d, 'any-field');
    const { proposal, content } = await this.prisma.$transaction(async (tx) => {
      const proposal = await tx.proposal.create({
        data: { userId: caller.id, likes: 0, dislikes: 0, commentsNumber: 0 },
      });
      const content = await this.insertContent(tx, proposal.id, caller.id, input);
      return { proposal, content };
    });
    // No data.id: pdf-ui reads data.attributes.proposal_id (§8.2).
    return { data: { attributes: { proposal_id: proposal.id, proposal_content_id: content.id } }, meta: {} };
  }

  async remove(idRaw: string, caller: AuthUser): Promise<SingleEnvelope<Entity>> {
    const id = parseRouteId(idRaw);
    const found = id === null ? null : await this.prisma.proposal.findUnique({ where: { id } });
    const proposal = assertOwner(found, (p) => p.userId, caller, FOREIGN);
    await this.prisma.$transaction(async (tx) => {
      const contents = await tx.proposalContent.findMany({
        where: { proposalId: proposal.id, hardForkContentId: { not: null } },
        select: { hardForkContentId: true },
      });
      // Cascades take contents, components, constitution rows, votes, polls,
      // poll votes, comments and their reports. Hard-fork rows are owned by
      // the content through a nullable FK, so they go explicitly (Δ8).
      await tx.proposal.delete({ where: { id: proposal.id } });
      const hardForkIds = contents.map((c) => c.hardForkContentId).filter((x): x is number => x !== null);
      if (hardForkIds.length)
        await tx.proposalHardForkContent.deleteMany({ where: { id: { in: hardForkIds } } });
    });
    return single(serializeEntity(proposal, ProposalResource));
  }

  async createContent(d: DataPayload, caller: AuthUser): Promise<SingleEnvelope<Entity>> {
    const proposalId = readIntRef(d, 'proposal_id');
    if (proposalId === undefined || proposalId === null) throw badRequestDetails('Proposal ID is required');
    const proposal = await this.prisma.proposal.findUnique({ where: { id: proposalId } });
    if (!proposal) throw badRequestDetails(NOT_FOUND);
    if (proposal.userId !== caller.id) throw forbidden(FOREIGN);
    const input = await this.readInput(d, 'previous-ga-id');
    const content = await this.prisma.$transaction(async (tx) => {
      // Serialize revisions of one proposal, so two concurrent non-draft
      // revisions cannot both stay active.
      await tx.$queryRaw`SELECT id FROM proposals WHERE id = ${proposal.id} FOR UPDATE`;
      const submitted = await tx.proposalContent.count({
        where: { proposalId: proposal.id, revActive: true, isDraft: false, submitted: true },
      });
      if (submitted > 0) throw badRequestDetails(ALREADY_SUBMITTED);
      const content = await this.insertContent(tx, proposal.id, proposal.userId, input);
      if (!input.isDraft) {
        await tx.proposalContent.updateMany({
          where: { proposalId: proposal.id, id: { not: content.id }, revActive: true },
          data: { revActive: false },
        });
      }
      return content;
    });
    return single({ id: content.id, attributes: serializeScalars(content, ProposalContentResource) });
  }

  async updateContent(idRaw: string, d: DataPayload, caller: AuthUser): Promise<SingleEnvelope<Entity>> {
    const id = parseRouteId(idRaw);
    const found = id === null ? null : await this.prisma.proposalContent.findUnique({ where: { id } });
    const content = assertOwner(found, (c) => c.userId, caller, FOREIGN);

    if (d.prop_submitted !== true) throw validationError('prop_submitted is invalid');
    const txHash = d.prop_submission_tx_hash;
    if (typeof txHash !== 'string' || !TX_HASH_RE.test(txHash)) {
      throw validationError('prop_submission_tx_hash is invalid');
    }
    let submissionDate: Date | null = null;
    if (d.prop_submission_date !== undefined && d.prop_submission_date !== null) {
      submissionDate = parseSubmissionDate(d.prop_submission_date);
    }

    try {
      const res = await this.prisma.proposalContent.updateMany({
        where: { id: content.id, revActive: true, isDraft: false, submitted: false },
        data: { submitted: true, submissionTxHash: txHash.toLowerCase(), submissionDate },
      });
      if (res.count === 0) throw badRequestDetails(ALREADY_SUBMITTED);
    } catch (e) {
      if (isUniqueViolation(e)) throw validationError('This attribute must be unique');
      throw e;
    }
    const updated = await this.prisma.proposalContent.findUnique({ where: { id: content.id } });
    if (!updated) throw notFound();
    return single({ id: updated.id, attributes: serializeScalars(updated, ProposalContentResource) });
  }
}

/** ISO date or datetime to the UTC date it names; else V. */
export function parseSubmissionDate(v: unknown): Date {
  if (typeof v !== 'string' || !/^\d{4}-\d{2}-\d{2}(?:[T ][\d:.]+(?:Z|[+-]\d{2}:?\d{2})?)?$/.test(v)) {
    throw validationError('prop_submission_date is invalid');
  }
  const d = new Date(v.length === 10 ? `${v}T00:00:00.000Z` : v.replace(' ', 'T'));
  if (Number.isNaN(d.getTime())) throw validationError('prop_submission_date is invalid');
  return new Date(Date.UTC(d.getUTCFullYear(), d.getUTCMonth(), d.getUTCDate()));
}
