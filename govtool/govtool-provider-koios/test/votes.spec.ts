import { FakeKoios } from './fake-http';
import { expectChainDataError } from './expect-error';
import { KoiosVotesApi } from '../src/api/governance/votes.api';
import { toVoterRef } from '../src/mappers/vote.mapper';

const PROPOSAL =
  'gov_action17m7nv7839mw93hv889tqzj0umv9ckm780f0nq02fep78f50uedxqq6g5mt9';
const DREP = 'drep1ytc6867ae0xmkekvmex79r9akyy28eu8nu03jf3xu9fle6c82l4eq';

function voteRow(overrides: Record<string, unknown> = {}) {
  return {
    vote_tx_hash:
      'c9b11588e508c325a260754086a6c5f40fe0ba4daa1c92a77ee59c8b55949755',
    voter_role: 'DRep',
    voter_id: DREP,
    proposal_id: PROPOSAL,
    proposal_tx_hash:
      'f6fd3678f12edc58dd8739560149fcdb0b8b6fc77a5f303d49c87c74d1fccb4c',
    proposal_index: 0,
    proposal_type: 'InfoAction',
    epoch_no: 656,
    block_height: 13953841,
    block_time: 1789675385,
    vote: 'Yes',
    meta_url: null,
    meta_hash: null,
    meta_json: null,
    ...overrides,
  };
}

function api(koios: FakeKoios): KoiosVotesApi {
  return new KoiosVotesApi(koios.client());
}

describe('KoiosVotesApi', () => {
  it('reads votes from /vote_list, which carries the vote transaction', async () => {
    const koios = new FakeKoios().on('vote_list', [voteRow()], { total: 1 });
    const { data } = await api(koios).list({ proposalId: PROPOSAL });

    const record = data.elements[0]!;
    expect(record.txRef.txHash).toBe(voteRow().vote_tx_hash);
    expect(record.txRef.block).toBe(13953841);
    expect(record.vote).toBe('yes');
    expect(record.voter).toMatchObject({ role: 'drep', id: DREP });
    // A CIP-105 id is derivable for a DRep and is carried for older clients.
    expect(record.voter.cip105Id).toMatch(/^drep1/);
  });

  it('never claims a per-vote voting power, because Koios has none', async () => {
    const koios = new FakeKoios().on('vote_list', [voteRow()]);
    const { data } = await api(koios).list({ proposalId: PROPOSAL });
    expect(data.elements[0]!.votingPower).toBeNull();
  });

  it('projects a rationale anchor without fetching it', async () => {
    const koios = new FakeKoios().on('vote_list', [
      voteRow({
        meta_url: 'https://example.test/rationale.jsonld',
        meta_hash: 'caa4',
        meta_json: { body: { summary: 'Because.' } },
      }),
    ]);
    const { data } = await api(koios).list({ proposalId: PROPOSAL });
    expect(data.elements[0]!.rationale).toMatchObject({
      standard: 'CIP100',
      status: 'valid',
      anchor: {
        url: 'https://example.test/rationale.jsonld',
        dataHash: 'caa4',
      },
      body: { summary: 'Because.' },
    });
  });

  it('translates every filter into a PostgREST predicate', async () => {
    const koios = new FakeKoios().on('vote_list', []);
    await api(koios).list({
      proposalId: PROPOSAL,
      voterId: DREP,
      role: ['drep', 'cc'],
      vote: ['yes', 'abstain'],
      proposalType: ['UpdateCommittee'],
      sort: 'oldest',
    });

    const call = koios.lastCallTo('vote_list')!;
    expect(call.params).toMatchObject({
      proposal_id: `eq.${PROPOSAL}`,
      voter_id: `eq.${DREP}`,
      voter_role: 'in.(DRep,ConstitutionalCommittee)',
      vote: 'in.(Yes,Abstain)',
      proposal_type: 'in.(NewCommittee)',
      order: 'block_time.asc',
    });
  });

  describe('superseded votes', () => {
    const earlier = voteRow({ vote: 'No', vote_tx_hash: 'aa', block_time: 1 });
    const later = voteRow({ vote: 'Yes', vote_tx_hash: 'bb', block_time: 2 });

    it('drops a vote a later one by the same voter replaced', async () => {
      const koios = new FakeKoios().on('vote_list', [later, earlier]);
      const { data } = await api(koios).list({ proposalId: PROPOSAL });
      expect(data.elements).toHaveLength(1);
      expect(data.elements[0]!.txRef.txHash).toBe('bb');
    });

    it('keeps both, correctly flagged, when asked for the history', async () => {
      const koios = new FakeKoios().on('vote_list', [later, earlier]);
      const { data } = await api(koios).list({
        proposalId: PROPOSAL,
        includeSuperseded: true,
      });
      expect(data.elements.map((vote) => vote.isCurrent)).toEqual([
        true,
        false,
      ]);
    });

    it('keeps votes by different voters on the same proposal', async () => {
      const koios = new FakeKoios().on('vote_list', [
        later,
        voteRow({
          voter_id: 'pool1abc',
          voter_role: 'SPO',
          vote_tx_hash: 'cc',
        }),
      ]);
      const { data } = await api(koios).list({ proposalId: PROPOSAL });
      expect(data.elements).toHaveLength(2);
    });
  });

  describe('voter identity', () => {
    // Regression: `/vote_list` returns `voter_hex: null` for *every* role, so
    // the credential hash has to be decoded from the bech32 id. It used to be
    // derived only for DReps, leaving SPO and committee voters with
    // `hash: ''` — found by the live conformance check, not by a fixture.
    const POOL = 'pool1tnl3yxmj8848vq6meduhz9n5520a7dwsh05r5gfyvlatj87k3jl';

    it('derives an SPO voter hash from the pool bech32, with no voter_hex', async () => {
      const koios = new FakeKoios().on('vote_list', [
        voteRow({ voter_role: 'SPO', voter_id: POOL, voter_hex: null }),
      ]);
      const { data } = await api(koios).list({ proposalId: PROPOSAL });

      const voter = data.elements[0]!.voter;
      expect(voter.role).toBe('spo');
      expect(voter.id).toBe(POOL);
      // a pool id is a bare 28-byte key hash — 56 hex characters
      expect(voter.hash).toMatch(/^[0-9a-f]{56}$/);
      expect(voter.isScriptBased).toBe(false);
      // CIP-105 is a DRep-only form
      expect(voter.cip105Id).toBeUndefined();
    });

    it('strips the CIP-129 header from a committee voter and reads its script bit', async () => {
      const koios = new FakeKoios().on('vote_list', [
        voteRow({
          voter_role: 'ConstitutionalCommittee',
          // cc_hot, script credential: header 0x03
          voter_id:
            'cc_hot1qw46h2at4w46h2at4w46h2at4w46h2at4w46h2at4w46h2c0m907k',
          voter_hex: null,
        }),
      ]);
      const { data } = await api(koios).list({ proposalId: PROPOSAL });

      const voter = data.elements[0]!.voter;
      expect(voter.role).toBe('cc');
      expect(voter.hash).toMatch(/^[0-9a-f]{56}$/);
      expect(voter.isScriptBased).toBe(true);
    });

    it('prefers an explicit hex over decoding, for the rows that carry one', async () => {
      // `/vote_list` never supplies it, but `proposal_voting_summary` and the
      // committee rows do, and those callers pass it through.
      const hex = 'ab'.repeat(28);
      expect(toVoterRef('SPO', POOL, hex).hash).toBe(hex);
      // and with none, it falls back to decoding the bech32
      expect(toVoterRef('SPO', POOL).hash).toMatch(/^[0-9a-f]{56}$/);
    });

    it('never leaves a voter hash empty for any role Koios reports', async () => {
      const koios = new FakeKoios().on('vote_list', [
        voteRow(),
        voteRow({ voter_role: 'SPO', voter_id: POOL, vote_tx_hash: 'aa' }),
      ]);
      const { data } = await api(koios).list({ proposalId: PROPOSAL });
      for (const vote of data.elements) {
        expect(vote.voter.hash).not.toBe('');
      }
    });
  });

  it('gives a pool-votes row a real voter hash, decoded from the pool id', async () => {
    // Regression: `/pool_votes` returns no voter columns — the pool is the
    // query parameter — and the voter was hand-rolled with `hash: ''`
    // instead of going through `toVoterRef`. Found by the live conformance
    // check; the unit tests never asserted the hash.
    const POOL = 'pool1m83drqwlugdt9jn7jkz8hx3pne53acfkd539d9cj8yr92dr4k9y';
    const ref = toVoterRef('SPO', POOL);

    expect(ref.role).toBe('spo');
    expect(ref.id).toBe(POOL);
    expect(ref.hash).toMatch(/^[0-9a-f]{56}$/);
  });

  it('fetches a single vote by its transaction', async () => {
    const koios = new FakeKoios().on('vote_list', [voteRow()]);
    const { data } = await api(koios).get(voteRow().vote_tx_hash);
    expect(data.proposal.id).toBe(PROPOSAL);
  });

  it('raises NOT_FOUND for a transaction that carried no vote', async () => {
    const koios = new FakeKoios().on('vote_list', []);
    const error = await expectChainDataError(api(koios).get('deadbeef'));
    expect(error.code).toBe('NOT_FOUND');
  });

  it('refuses a vote index, which Koios does not record', async () => {
    const koios = new FakeKoios().on('vote_list', [voteRow()]);
    const error = await expectChainDataError(
      api(koios).get('deadbeef', { index: 1 }),
    );
    expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
  });

  it('refuses a text search over rationales', async () => {
    const koios = new FakeKoios().on('vote_list', []);
    const error = await expectChainDataError(api(koios).list({ search: 'k' }));
    expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
  });

  it('rejects the direct-voter role, which Koios does not model', async () => {
    const koios = new FakeKoios().on('vote_list', []);
    const error = await expectChainDataError(
      api(koios).list({ role: ['direct'] }),
    );
    expect(error.code).toBe('INVALID_INPUT');
  });
});
