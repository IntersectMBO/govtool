// POST /api/bds body (SPEC §8.8): the writable fields only. Everything else
// pdf-ui sends (echoed attributes, `creator`, section `id`s, timestamps,
// counters, `is_active`, `submitted_for_vote`) is ignored (§3.5, Δ3).
// Pure: lookup ids are collected here and checked against the database by
// the service.

import type { DataPayload } from '../common/body';
import { badRequest, validationError } from '../common/errors';
import { readArray, readBool, readIntRef, readObject, readString, readText } from '../common/fields';

/** Section text columns (§5.4). */
const TEXT_MAX = 15000;
const LINK_MAX = 2048;
const MAX_LINKS = 25;

export type LookupModel =
  'countryList' | 'bdType' | 'bdRoadMap' | 'bdIntersectCommittee' | 'bdContractType' | 'bdCurrency';

/** A lookup reference to verify: V `<field> is invalid` when the row is missing. */
export interface LookupRef {
  field: string;
  model: LookupModel;
  id: number;
}

export interface OwnershipData {
  agreed: boolean | null;
  groupName: string | null;
  companyName: string | null;
  typeOfGroup: string | null;
  socialHandles: string | null;
  submitedOnBehalf: string | null;
  companyDomainName: string | null;
  proposalPublicChampion: string | null;
  keyInfoToIdentifyGroup: string | null;
  beCountryId: number | null;
}

export interface PsapbData {
  problemStatement: string | null;
  proposalBenefit: string | null;
  supplementaryEndorsement: string | null;
  explainProposalRoadmap: string | null;
  typeId: number | null;
  roadmapId: number | null;
  committeeId: number | null;
}

export interface ProposalDetailData {
  proposalName: string | null;
  proposalDescription: string | null;
  keyDependencies: string | null;
  maintainAndSupport: string | null;
  keyProposalDeliverables: string | null;
  resourcingDurationEstimates: string | null;
  experience: string | null;
  otherContractType: string | null;
  contractTypeId: number | null;
}

export interface CostingData {
  costBreakdown: string | null;
  preferredCurrencyId: number | null;
  adaAmount: string | null;
  amountInPreferredCurrency: string | null;
  usdToAdaConversionRate: string | null;
  adaAmountClone: number;
  amountInPreferredCurrencyClone: number;
  usdToAdaConversionRateClone: number;
}

export interface LinkData {
  position: number;
  link: string;
  text: string | null;
}

export interface ContactData {
  beFullName: string | null;
  beEmail: string | null;
  submissionLeadFullName: string | null;
  submissionLeadEmail: string | null;
  otherContractType: string | null;
  beCountryOfResId: number | null;
  beNationalityId: number | null;
}

export interface BdInput {
  /** Null for a new BD; the chain's master id for a new version. */
  masterId: number | null;
  intersectNamedAdministrator: boolean;
  intersectAdminFurtherText: string | null;
  ownership: OwnershipData;
  psapb: PsapbData;
  proposalDetail: ProposalDetailData;
  costing: CostingData;
  links: LinkData[];
  contact: ContactData | null;
  lookups: LookupRef[];
}

const nn = <T>(v: T | null | undefined): T | null => (v === undefined ? null : v);

/** `,` → `.`, parseFloat, 0 when not finite (§5.4 `*_clone`). */
export function amountClone(s: string | null): number {
  if (s === null) return 0;
  const n = parseFloat(s.replace(/,/g, '.'));
  return Number.isFinite(n) ? n : 0;
}

function section(d: DataPayload, name: string): DataPayload {
  const s = readObject(d, name);
  if (s === null || s === undefined) throw validationError(`${name} is required`);
  return s;
}

function lookup(s: DataPayload, field: string, model: LookupModel, refs: LookupRef[]): number | null {
  const id = nn(readIntRef(s, field));
  if (id !== null) refs.push({ field, model, id });
  return id;
}

const str = (s: DataPayload, f: string) => nn(readString(s, f));
const text = (s: DataPayload, f: string) => nn(readText(s, f, TEXT_MAX));

const LINK_SCHEMES = new Set(['http:', 'https:', 'ipfs:']);

/** http, https or ipfs only (Δ46, as Δ24 on the proposal side): no `javascript:` links. */
export function isAllowedLink(link: string): boolean {
  try {
    return LINK_SCHEMES.has(new URL(link.trim()).protocol);
  } catch {
    return false;
  }
}

function parseLinks(fi: DataPayload): LinkData[] {
  const raw = nn(readArray(fi, 'proposal_links', MAX_LINKS)) ?? [];
  const out: LinkData[] = [];
  for (const item of raw) {
    if (typeof item !== 'object' || item === null || Array.isArray(item)) {
      throw validationError('proposal_links is invalid');
    }
    const entry = item as DataPayload;
    const link = nn(readString(entry, 'prop_link', { max: LINK_MAX, acceptNumbers: false }));
    // pdf-ui seeds two blank links; blank entries are dropped.
    if (link === null || link.trim() === '') continue;
    if (!isAllowedLink(link)) throw validationError('prop_link is invalid');
    out.push({ position: out.length, link, text: str(entry, 'prop_link_text') });
  }
  return out;
}

export function parseBdInput(d: DataPayload): BdInput {
  if (d.privacy_policy !== true) throw badRequest('Privacy policy must be accepted');

  const ownershipIn = section(d, 'bd_proposal_ownership');
  const psapbIn = section(d, 'bd_psapb');
  const detailIn = section(d, 'bd_proposal_detail');
  const costingIn = section(d, 'bd_costing');
  const furtherIn = section(d, 'bd_further_information');
  const contactIn = nn(readObject(d, 'bd_contact_information'));

  const lookups: LookupRef[] = [];

  const ownership: OwnershipData = {
    agreed: nn(readBool(ownershipIn, 'agreed')),
    groupName: str(ownershipIn, 'group_name'),
    companyName: str(ownershipIn, 'company_name'),
    typeOfGroup: str(ownershipIn, 'type_of_group'),
    socialHandles: str(ownershipIn, 'social_handles'),
    submitedOnBehalf: str(ownershipIn, 'submited_on_behalf'),
    companyDomainName: str(ownershipIn, 'company_domain_name'),
    proposalPublicChampion: str(ownershipIn, 'proposal_public_champion'),
    keyInfoToIdentifyGroup: text(ownershipIn, 'key_info_to_identify_group'),
    beCountryId: lookup(ownershipIn, 'be_country', 'countryList', lookups),
  };

  const psapb: PsapbData = {
    problemStatement: text(psapbIn, 'problem_statement'),
    proposalBenefit: text(psapbIn, 'proposal_benefit'),
    supplementaryEndorsement: text(psapbIn, 'supplementary_endorsement'),
    explainProposalRoadmap: text(psapbIn, 'explain_proposal_roadmap'),
    typeId: lookup(psapbIn, 'type_name', 'bdType', lookups),
    roadmapId: lookup(psapbIn, 'roadmap_name', 'bdRoadMap', lookups),
    committeeId: lookup(psapbIn, 'committee_name', 'bdIntersectCommittee', lookups),
  };

  const proposalDetail: ProposalDetailData = {
    proposalName: text(detailIn, 'proposal_name'),
    proposalDescription: text(detailIn, 'proposal_description'),
    keyDependencies: text(detailIn, 'key_dependencies'),
    maintainAndSupport: text(detailIn, 'maintain_and_support'),
    keyProposalDeliverables: text(detailIn, 'key_proposal_deliverables'),
    resourcingDurationEstimates: text(detailIn, 'resourcing_duration_estimates'),
    experience: text(detailIn, 'experience'),
    otherContractType: text(detailIn, 'other_contract_type'),
    contractTypeId: lookup(detailIn, 'contract_type_name', 'bdContractType', lookups),
  };

  const adaAmount = str(costingIn, 'ada_amount');
  const amountInPreferredCurrency = str(costingIn, 'amount_in_preferred_currency');
  const usdToAdaConversionRate = str(costingIn, 'usd_to_ada_conversion_rate');
  const costing: CostingData = {
    costBreakdown: text(costingIn, 'cost_breakdown'),
    preferredCurrencyId: lookup(costingIn, 'preferred_currency', 'bdCurrency', lookups),
    adaAmount,
    amountInPreferredCurrency,
    usdToAdaConversionRate,
    adaAmountClone: amountClone(adaAmount),
    amountInPreferredCurrencyClone: amountClone(amountInPreferredCurrency),
    usdToAdaConversionRateClone: amountClone(usdToAdaConversionRate),
  };

  const contact: ContactData | null = contactIn
    ? {
        beFullName: str(contactIn, 'be_full_name'),
        beEmail: str(contactIn, 'be_email'),
        submissionLeadFullName: str(contactIn, 'submission_lead_full_name'),
        submissionLeadEmail: str(contactIn, 'submission_lead_email'),
        otherContractType: str(contactIn, 'other_contract_type'),
        beCountryOfResId: lookup(contactIn, 'be_country_of_res', 'countryList', lookups),
        beNationalityId: lookup(contactIn, 'be_nationality', 'countryList', lookups),
      }
    : null;

  return {
    masterId: nn(readIntRef(d, 'master_id')),
    intersectNamedAdministrator: nn(readBool(d, 'intersect_named_administrator')) ?? false,
    intersectAdminFurtherText: text(d, 'intersect_admin_further_text'),
    ownership,
    psapb,
    proposalDetail,
    costing,
    links: parseLinks(furtherIn),
    contact,
    lookups,
  };
}
