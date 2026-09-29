// Budget-discussion fixtures shared by the test/budget*.e2e-spec.ts files.

import type { TestApp } from './helpers/app';
import type { StakeSession } from './helpers/auth';

/** A POST /api/bds `data` payload as pdf-ui's wizard builds it. */
export function bdPayload(name = 'Demo budget', over: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    bd_proposal_ownership: {
      submited_on_behalf: 'Company',
      company_name: 'Acme',
      company_domain_name: 'acme.example',
      be_country: 1,
      group_name: '',
      type_of_group: '',
      key_info_to_identify_group: '',
      social_handles: '@acme',
      agreed: true,
    },
    bd_psapb: {
      problem_statement: 'A problem',
      proposal_benefit: 'A benefit',
      roadmap_name: 10,
      explain_proposal_roadmap: 'Explained',
      type_name: 1,
      committee_name: 2,
      supplementary_endorsement: '',
    },
    bd_proposal_detail: {
      proposal_name: name,
      proposal_description: 'Description',
      key_dependencies: 'None',
      maintain_and_support: 'Us',
      key_proposal_deliverables: 'Things',
      resourcing_duration_estimates: 'Months',
      experience: 'Lots',
      contract_type_name: 1,
      other_contract_type: '',
    },
    bd_costing: {
      ada_amount: '1000',
      usd_to_ada_conversion_rate: '0,5',
      amount_in_preferred_currency: '500',
      cost_breakdown: 'Breakdown',
      preferred_currency: 1,
    },
    bd_further_information: {
      proposal_links: [{ prop_link: 'https://example.com/a', prop_link_text: 'A' }, { prop_link: '' }],
    },
    intersect_named_administrator: false,
    intersect_admin_further_text: 'Further',
    privacy_policy: true,
    ...over,
  };
}

export interface CreatedBd {
  id: number;
  master_id: string;
  [k: string]: unknown;
}

/** POST /api/bds and return the raw body (expects 200). */
export async function createBd(
  t: TestApp,
  s: StakeSession,
  name?: string,
  over: Record<string, unknown> = {},
): Promise<CreatedBd> {
  const res = await t
    .api()
    .post('/api/bds')
    .set(s.auth)
    .send({ data: bdPayload(name, over) });
  if (res.status !== 200) throw new Error(`POST /api/bds ${res.status}: ${JSON.stringify(res.body)}`);
  return res.body as CreatedBd;
}

/** The pdf-ui detail/edit query (SingleBudgetDiscussion, CreateBudgetDiscussionDialog). */
export const DETAIL_QUERY =
  'populate[0]=creator&populate[1]=bd_costing.preferred_currency&populate[2]=bd_proposal_detail.contract_type_name&populate[3]=bd_further_information.proposal_links&populate[4]=bd_psapb.type_name&populate[5]=bd_psapb.roadmap_name&populate[6]=bd_psapb.committee_name&populate[7]=bd_proposal_ownership.be_country';

/** pdf-ui's lib/helpers.js cleanObject, used by the edit flow. */
export function cleanObject(obj: unknown): unknown {
  if (Array.isArray(obj)) return obj.map(cleanObject);
  if (obj !== null && typeof obj === 'object') {
    const out: Record<string, unknown> = {};
    for (const [key, raw] of Object.entries(obj)) {
      if (['id', 'createdAt', 'updatedAt'].includes(key)) continue;
      const value = cleanObject(raw);
      if (
        (key === 'data' || key === 'attributes') &&
        value &&
        typeof value === 'object' &&
        !Array.isArray(value)
      ) {
        Object.assign(out, value);
      } else {
        out[key] = value;
      }
    }
    return out;
  }
  return obj;
}

type Json = Record<string, any>;

/** The edit-flow body pdf-ui posts: cleaned GET response, relation ids restored, master_id set. */
export function editFlowBody(response: Json, masterId: string): Json {
  const d = cleanObject(response) as Json;
  const a = response.attributes;
  d.master_id = masterId;
  d.bd_proposal_ownership.be_country = a.bd_proposal_ownership?.data?.attributes?.be_country?.data?.id;
  d.bd_psapb.roadmap_name = a.bd_psapb?.data?.attributes?.roadmap_name?.data?.id;
  d.bd_psapb.type_name = a.bd_psapb?.data?.attributes?.type_name?.data?.id;
  d.bd_psapb.committee_name = a.bd_psapb?.data?.attributes?.committee_name?.data?.id;
  d.bd_proposal_detail.contract_type_name =
    a.bd_proposal_detail?.data?.attributes?.contract_type_name?.data?.id;
  d.bd_costing.preferred_currency = a.bd_costing?.data?.attributes?.preferred_currency?.data?.id;
  return d;
}
