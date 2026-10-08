// Proposal-side fixtures created through the API (SPEC §11.4).

import type { TestApp } from './app';
import type { StakeSession } from './auth';

/** A valid testnet stake address for treasury withdrawals. */
export const STAKE_TEST_ADDRESS = 'stake_test1urfa857n60fa857n60fa857n60fa857n60fa857n60fa85cyqv8zy';

export function proposalBody(overrides: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    gov_action_type_id: 1,
    prop_name: 'Test proposal',
    prop_abstract: 'Abstract',
    prop_motivation: 'Motivation',
    prop_rationale: 'Rationale',
    proposal_links: [{ prop_link: 'https://example.com', prop_link_text: 'Example' }],
    proposal_withdrawals: [],
    proposal_constitution_content: {},
    is_draft: false,
    ...overrides,
  };
}

/** POST /api/proposals; returns the new proposal and content ids. */
export async function createProposal(
  t: TestApp,
  s: StakeSession,
  overrides: Record<string, unknown> = {},
): Promise<{ proposalId: number; contentId: number }> {
  const res = await t
    .api()
    .post('/api/proposals')
    .set(s.auth)
    .send({ data: proposalBody(overrides) });
  if (res.status !== 200) throw new Error(`createProposal: ${res.status} ${JSON.stringify(res.body)}`);
  return {
    proposalId: res.body.data.attributes.proposal_id as number,
    contentId: res.body.data.attributes.proposal_content_id as number,
  };
}

/** POST /api/polls on an owned proposal; returns the poll id. */
export async function createPoll(t: TestApp, owner: StakeSession, proposalId: number): Promise<number> {
  const res = await t
    .api()
    .post('/api/polls')
    .set(owner.auth)
    .send({
      data: {
        proposal_id: String(proposalId),
        poll_start_dt: new Date().toISOString(),
        is_poll_active: true,
      },
    });
  if (res.status !== 200) throw new Error(`createPoll: ${res.status} ${JSON.stringify(res.body)}`);
  return res.body.data.id as number;
}
