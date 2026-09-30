// Text sorts linguistically and case-insensitively (SPEC §4.4, und-x-icu), so
// the Playwright 8B_2 / 11B_3 `localeCompare` checks hold on mixed case.

import { createTestApp, TestApp } from './helpers/app';
import { loginStake } from './helpers/auth';
import { expectList } from './helpers/envelope';

const NAMES = ['beta Two', 'Alpha one', 'ALPHA Zed', 'gamma', 'Beta three', 'éclair', 'Zulu'];
const expected = (dir: 'asc' | 'desc') => {
  const s = [...NAMES].sort((x, y) => x.replace(/ /g, '').localeCompare(y.replace(/ /g, '')));
  return dir === 'asc' ? s : s.reverse();
};

describe('text collation (e2e)', () => {
  let t: TestApp;
  beforeAll(async () => {
    t = await createTestApp();
  });
  afterAll(async () => {
    await t.close();
  });

  it('the sorted columns carry und-x-icu', async () => {
    const rows = await t.prisma.$queryRaw<Array<{ c: string }>>`
      SELECT table_name || '.' || column_name AS c FROM information_schema.columns
      WHERE table_schema = 'public' AND collation_name = 'und-x-icu' ORDER BY 1`;
    expect(rows.map((r) => r.c)).toEqual(
      expect.arrayContaining([
        'proposal_contents.name',
        'bd_proposal_details.proposal_name',
        'users.govtool_username',
        'comments.text',
        'governance_action_types.name',
      ]),
    );
  });

  it('proposals: sort[prop_name] ASC/DESC matches localeCompare', async () => {
    const s = await loginStake(t);
    for (const name of NAMES) {
      await t
        .api()
        .post('/api/proposals')
        .set(s.auth)
        .send({ data: { gov_action_type_id: 1, prop_name: name } })
        .expect(200);
    }
    for (const dir of ['asc', 'desc'] as const) {
      const data = expectList(
        await t
          .api()
          .get(`/api/proposals?filters[gov_action_type_id]=1&sort[prop_name]=${dir.toUpperCase()}`),
      );
      expect(data.map((d) => (d.attributes.content as any).attributes.prop_name)).toEqual(expected(dir));
    }
  });

  it('BDs: sort[bd_proposal_detail][proposal_name] ASC/DESC matches localeCompare', async () => {
    const s = await loginStake(t);
    for (const name of NAMES) {
      const detail = await t.prisma.bdProposalDetail.create({ data: { proposalName: name } });
      const bd = await t.prisma.bd.create({
        data: { creatorId: s.user.id, privacyPolicy: true, isActive: true, proposalDetailId: detail.id },
      });
      await t.prisma.bd.update({ where: { id: bd.id }, data: { masterId: bd.id } });
    }
    for (const dir of ['ASC', 'DESC'] as const) {
      const data = expectList(
        await t
          .api()
          .get(
            `/api/bds?filters[$and][0][is_active]=true&sort[bd_proposal_detail][proposal_name]=${dir}&populate[0]=bd_proposal_detail`,
          ),
      );
      expect(data.map((d) => (d.attributes.bd_proposal_detail as any).data.attributes.proposal_name)).toEqual(
        expected(dir === 'ASC' ? 'asc' : 'desc'),
      );
    }
  });

  it('$containsi still matches across case under the ICU collation', async () => {
    const data = expectList(await t.api().get('/api/proposals?filters[prop_name][$containsi]=ALPHA'));
    expect(data).toHaveLength(2);
  });
});
