import type { DRepService } from '../src/drep/drep.service';
import type { VoteParams, VoteResponse } from '../src/drep/drep.type';
import { ProposalController } from '../src/proposal/proposal.controller';
import type { ProposalService } from '../src/proposal/proposal.service';
import type {
  GetProposalResponse,
  ProposalResponse,
} from '../src/proposal/proposal.type';

const DREP_HEX = 'a'.repeat(56);
const TX_A = 'A'.repeat(64);
const TX_B = 'b'.repeat(64);

const proposal = (txHash: string, index: number) =>
  ({ txHash, index }) as ProposalResponse;

const vote: VoteParams = {
  proposalId: '1',
  drepId: DREP_HEX,
  vote: 'yes',
  url: null,
  metadataHash: null,
  epochNo: 10,
  date: '2026-01-01T00:00:00.000Z',
  txHash: 'c'.repeat(64),
};

function controller(votes: VoteResponse[]) {
  const list = jest.fn().mockResolvedValue({
    page: 0,
    pageSize: 10,
    total: 0,
    elements: [],
  });
  const get = jest.fn().mockResolvedValue({
    proposal: proposal(TX_A, 0),
    vote: null,
  } satisfies GetProposalResponse);
  const getVoteRows = jest.fn().mockResolvedValue(votes);
  // The vote history with documents: these routes must not wait on it.
  const getVotes = jest.fn().mockResolvedValue(votes);
  return {
    list,
    getVoteRows,
    getVotes,
    controller: new ProposalController(
      { list, get } as unknown as ProposalService,
      { getVoteRows, getVotes } as unknown as DRepService,
    ),
  };
}

describe('GET /proposal/get with drepId', () => {
  it("returns the DRep's vote on the action", async () => {
    const { controller: c } = controller([
      { vote, proposal: proposal(TX_A.toLowerCase(), 0) },
    ]);
    await expect(c.get(`${TX_A}#0`, DREP_HEX)).resolves.toMatchObject({
      vote,
    });
  });

  it("reads the DRep's votes without resolving their documents", async () => {
    const { controller: c, getVoteRows, getVotes } = controller([]);
    await c.get(`${TX_A}#0`, DREP_HEX);
    expect(getVoteRows).toHaveBeenCalledWith(DREP_HEX);
    expect(getVotes).not.toHaveBeenCalled();
  });

  it('returns null when the DRep voted on other actions only', async () => {
    const { controller: c } = controller([
      { vote, proposal: proposal(TX_B, 0) },
    ]);
    await expect(c.get(`${TX_A}#0`, DREP_HEX)).resolves.toMatchObject({
      vote: null,
    });
  });

  it('reads no votes for the "undefined" a disconnected frontend sends', async () => {
    const { controller: c, getVoteRows } = controller([]);
    await expect(c.get(`${TX_A}#0`, 'undefined')).resolves.toMatchObject({
      vote: null,
    });
    expect(getVoteRows).not.toHaveBeenCalled();
  });
});

describe('GET /proposal/list with drepId', () => {
  it('leaves out the actions the DRep has voted on', async () => {
    const {
      controller: c,
      list,
      getVotes,
    } = controller([{ vote, proposal: proposal(TX_B, 3) }]);
    await c.list(undefined, undefined, undefined, '0', '10', DREP_HEX);
    expect(list).toHaveBeenCalledWith(
      expect.objectContaining({ excludedIds: new Set([`${TX_B}#3`]) }),
    );
    expect(getVotes).not.toHaveBeenCalled();
  });

  it('excludes nothing without a DRep', async () => {
    const { controller: c, list, getVoteRows } = controller([]);
    await c.list();
    expect(list).toHaveBeenCalledWith(
      expect.objectContaining({ excludedIds: new Set() }),
    );
    expect(getVoteRows).not.toHaveBeenCalled();
  });
});
